package com.hanhuy.android.keepshare

import java.net.URI

import android.annotation.TargetApi
import android.app.{Notification, KeyguardManager, PendingIntent, NotificationManager}
import android.content._
import android.os.{Build, Handler, HandlerThread, Bundle}
import android.support.v4.app.NotificationCompat
import android.view.accessibility.{AccessibilityNodeInfo, AccessibilityEvent}
import com.hanhuy.android.common._
import com.hanhuy.android.conversions._

import android.accessibilityservice.{AccessibilityService => Accessibility}

import scala.collection.JavaConversions._
import scala.collection.immutable.Queue
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.Try

import Futures._

/**
 * The clipboard remains vulnerable to
 * https://www2.dcsec.uni-hannover.de/files/p170.pdf
 * Until this bug is fixed https://code.google.com/p/android/issues/detail?id=41037
 * duplicated at https://code.google.com/p/android/issues/detail?id=56097
 *
 * Mitigations against clipboard sniffing are clipboard data randomization,
 * and pasting data randomly from the clipboard when it matches what we need
 * to paste. However, the android bug where it inserts an additional space
 * breaks this behavior.
 *
 * No longer uses the clipboard for Lollipop and newer. Thus impervious.
 * @author pfnguyen
 */
object AccessibilityService {
  private var _running = false
  def running = _running
  lazy val lollipopAndNewer = Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP
  // flag to disable fill prompt/processing during lock screen
  var filling: Boolean = false
  var fillInfo = Option.empty[AccessibilityFillEvent]

  val ACTION_CANCEL = "com.hanhuy.android.keepshare.action.CANCEL_FILL"
  val ACTION_SEARCH = "com.hanhuy.android.keepshare.action.SEARCH_FILL"

  val EXTRA_WINDOWID = "com.hanhuy.android.keepshare.extra.WINDOW_ID"
  val EXTRA_PACKAGE  = "com.hanhuy.android.keepshare.extra.PACKAGE"
  val EXTRA_URI      = "com.hanhuy.android.keepshare.extra.URI"

  var lastCallback: Option[Runnable] = None

  def childrenToSeq(node: AccessibilityNodeInfo): Seq[AccessibilityNodeInfo] = {
    (0 until node.getChildCount) map node.getChild
  }

  object AccessibilityTree {
    def apply(node: AccessibilityNodeInfo) = new AccessibilityTree(Option(node))
  }
  class AccessibilityTree private(val node: Option[AccessibilityNodeInfo]) extends Iterable[AccessibilityTree] {
    lazy val children = node map { n =>
      childrenToSeq(n) map AccessibilityTree.apply
    } getOrElse Seq.empty

    override def iterator = new Iterator[AccessibilityTree] {
      var queue = Queue(AccessibilityTree.this)
      override def hasNext = queue.nonEmpty
      override def next() = {
        val (n, q) = queue.dequeue
        queue = q ++ n.children
        n
      }
    }

    // this kinda stinks, but should catch the vast majority of text fields
    def isEditable = node exists (n => n.isEditable || n.getClassName == "android.widget.EditText")
    def isPassword = node exists (_.isPassword)
    def packageName = node map (_.getPackageName)
    def windowId = node map (_.getWindowId)

    def viewIdResourceName: Option[String] = node flatMap { n =>
      Option(n.getViewIdResourceName)
    }

    def findNodeById(id: String): Option[AccessibilityNodeInfo] =
      node flatMap { _.findAccessibilityNodeInfosByViewId(id).headOption }

    override def mkString = {
      val t = node map { _.getClassName } getOrElse "null"
      val tx = node map { _.getText } getOrElse "null"
      val c = children map (_.toString)
      "Node[%s: id=%s password=%s text=%s children=[%s]]" format (
        t, viewIdResourceName, isPassword, tx, c mkString ",")
    }

    def dispose() {
      iterator foreach (_.node foreach (_.recycle))
    }
  }

  lazy val EXCLUDED_PACKAGES = if (lollipopAndNewer)
    Set(Application.instance.getPackageName, "com.android.systemui", "")
  else
    Set(Application.instance.getPackageName, "com.android.chrome", "com.android.systemui")

  implicit class RunManagedResource[A : ManagedResource.ResourceManager](val mr: ManagedResource[A]) {
    def run[B](f: A => B): B = try {
      f(mr.res)
    } finally {
      mr.cleanup foreach (_())
      implicitly[ManagedResource.ResourceManager[A]].dispose(mr.res)
    }
  }
}

@TargetApi(18)
class AccessibilityService extends Accessibility with EventBus.RefOwner {
  import AccessibilityService._
  val log = Logcat("AccessibilityService")
  private var lastCanceledSearchURI: Option[URI] = None
  private var lastSearchURI: Option[URI] = None

  private val thread = new HandlerThread("AccessibilityService")
  private lazy val handler = {
    thread.start()
    new Handler(thread.getLooper)
  }
  /**
   * @param windowId must match current view tree or no processing will occur
   * @param f (packageName, searchURI, view tree, password field)
   * @tparam A whatever you want
   * @return Option[A] result of f
   */
  def withTree[A](windowId: Int)(
    f: (String, Option[URI], AccessibilityTree, AccessibilityTree) => A): Option[A] = {

    val root = getRootInActiveWindow
    ManagedResource(AccessibilityTree(root)).run { tree =>
      // half overlays, like IME will show with a different window ID
      // notifications seem to put in the wrong package name as well
      // any views with systemui should be filtered out
      if (tree.windowId.contains(windowId) && !tree.exists(
        _.viewIdResourceName exists (_ startsWith "com.android.systemui"))) {
        for {
          packageName <- tree.packageName
          password <- tree find (_.isPassword)
        } yield {
          val searchURI = if (packageName == "com.android.chrome") {
            tree.findNodeById("com.android.chrome:id/url_bar") flatMap { u =>
              val url = u.getText.toString
              Try(new URI(if (url.indexOf(":/") < 0) "http://" + url else url)).toOption
            }
          } else if (packageName == "com.sec.android.app.sbrowser") {
            tree.findNodeById("com.sec.android.app.sbrowser:id/location_bar_edit_text") flatMap { u =>
              val url = u.getText.toString
              Try(new URI(if (url.indexOf(":/") < 0) "http://" + url else url)).toOption
            }
          } else if (packageName == "com.android.browser") {
            tree.findNodeById("com.android.browser:id/url") flatMap { u =>
              val url = u.getText.toString
              Try(new URI(if (url.indexOf(":/") < 0) "http://" + url else url)).toOption
            }
          } else if (packageName == "com.opera.browser" || packageName == "com.opera.mini.native") {
            // mini uses a different package name
            val pkg = if (packageName == "com.opera.browser") packageName else "com.opera.android"
            tree.findNodeById(pkg + ":id/url_field") flatMap { u =>
              val url = u.getText.toString
              Try(new URI(if (url.indexOf(":/") < 0) "http://" + url else url)).toOption
            }
          } else {
            val appHost = packageName.toString
              .split( """\.""").reverse.mkString(".")
            Some(new URI("android-package://" + appHost))
          }
          f(packageName.toString, searchURI, tree, password)
        }
      } else {
        None
      }
    }
  }
  override def onAccessibilityEvent(event: AccessibilityEvent) {
    import AccessibilityEvent._

    // don't handle form fill for ourself and system
    val packageName =
      Option(event) flatMap (e => Option(e.getPackageName)) flatMap (p => Option(p.toString)) getOrElse ""
    if (!EXCLUDED_PACKAGES(packageName) && filling) {
      val windowId = event.getWindowId
      event.getEventType match {
        case TYPE_WINDOW_CONTENT_CHANGED | TYPE_WINDOW_STATE_CHANGED =>

          lastCallback foreach handler.removeCallbacks
          val r: Runnable = { () =>
            withTree(windowId) { (pkg, searchURI, tree, password) =>

              if (lastCanceledSearchURI != searchURI) {

                fillInfo match {
                  case Some(x) => fillIn(x, pkg, searchURI, tree, password)
                  case None => searchURI foreach { uri =>
                    val builder = new NotificationCompat.Builder(this)
                    val extras = new Bundle()
                    extras.putString(EXTRA_PACKAGE, packageName)
                    extras.putString(EXTRA_URI, uri.toString)
                    extras.putInt(EXTRA_WINDOWID, windowId)
                    builder.setSmallIcon(R.drawable.ic_lock)
                      .setPriority(Notification.PRIORITY_HIGH)
                      .setCategory(Notification.CATEGORY_SYSTEM)
                      .setVibrate(Array(0l))
                      .setContentTitle(getString(R.string.form_fill_notif_title, getString(R.string.appname)))
                      .setContentText(getString(R.string.form_fill_notif_text, uri.getHost))
                      .setTicker(getString(R.string.form_fill_notif_text, uri.getHost))
                      .setAutoCancel(true)
                      .setDeleteIntent(PendingIntent.getBroadcast(this, 0,
                        new Intent(ACTION_CANCEL).putExtras(extras),
                          PendingIntent.FLAG_UPDATE_CURRENT))
                      .setContentIntent(PendingIntent.getBroadcast(this, 1,
                        new Intent(ACTION_SEARCH).putExtras(extras),
                          PendingIntent.FLAG_UPDATE_CURRENT))

                    this.systemService[NotificationManager].notify(
                      Notifications.NOTIF_FOUND, builder.build())
                    lastSearchURI = searchURI
                  }
                }
              }
            } getOrElse {
              this.systemService[NotificationManager].cancel(Notifications.NOTIF_FOUND)
            }
          }
          handler.post(r)
          lastCallback = Some(r)
        case _ =>
      }
    }
  }

  override def onInterrupt() = ()

  override def onCreate() {
    super.onCreate()
    log.d("Launching accessibility service")
    registerReceiver(receiver, Seq(ACTION_CANCEL, ACTION_SEARCH,
      Intent.ACTION_SCREEN_OFF, Intent.ACTION_USER_PRESENT))
    filling = !this.systemService[KeyguardManager].isKeyguardLocked
    AccessibilityService._running = true
  }

  override def onDestroy() {
    AccessibilityService._running = false
    super.onDestroy()
    log.d("Exiting accessibility service")
    thread.quit()
    unregisterReceiver(receiver)
    this.systemService[NotificationManager].cancel(Notifications.NOTIF_FOUND)
  }

  val receiver: BroadcastReceiver = (c: Context, intent: Intent) =>
    intent.getAction match {
      case Intent.ACTION_SCREEN_OFF =>
        filling = false
        this.systemService[NotificationManager].cancel(Notifications.NOTIF_FOUND)
      case Intent.ACTION_USER_PRESENT => filling = true
      case ACTION_CANCEL =>
        lastCanceledSearchURI = lastSearchURI
      case ACTION_SEARCH =>
        val newIntent = new Intent(this, classOf[AccessibilitySearchActivity])
        newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK |
          Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS)
        newIntent.setAction(Intent.ACTION_SEND)
        newIntent.putExtras(intent)
        startActivity(newIntent)
    }

  ServiceBus += {
    case a@AccessibilityFillEvent(_, _, _, _, _) =>
      lastCallback foreach handler.removeCallbacks
      val r: Runnable = { () => fillIn(a) }
      handler.post(r)
      lastCallback = Some(r)
  }

  def fillIn(event: AccessibilityFillEvent,
             pkg: String,
             searchURI: Option[URI],
             tree: AccessibilityTree,
             passwordField: AccessibilityTree): Unit = synchronized {
    log.d("Fill in %s == %s ?" format (event.pkg, pkg))
    if (event.pkg == pkg && (searchURI.map (_.toString) == Option(event.uri))) {
      // needs to run on ui thread
      //val clipboard = systemService[ClipboardManager]
      //val clip = clipboard.getPrimaryClip

      fillInfo = None
      lastCanceledSearchURI = None
      val edits = tree collect { case t if t.isEditable => t }
      val text = (edits takeWhile (!_.isPassword)).lastOption

      // do password first because some username fields have autocompletes...
      passwordField.node foreach { n =>
          pasteData(n, event.password) }

      text foreach (_.node foreach { n =>
        Thread.sleep(100)
        pasteData(n, event.username)
      })

      //clipboard.setPrimaryClip(clip)
      true
    } else fillInfo = Some(event)
  }
  def fillIn(event: AccessibilityFillEvent): Unit = synchronized {
     withTree(event.windowId) { (pkg, searchURI, tree, passwordField) =>
      fillIn(event, pkg, searchURI, tree, passwordField)
    } getOrElse { fillInfo = Some(event) }
  }

  def pasteData(node: AccessibilityNodeInfo, data: String) {
    def preamble = Future.main {
        node.performAction(AccessibilityNodeInfo.ACTION_FOCUS)
      } ~ Future.main {
        // unfortunately, this doesn't work for password fields...
        val text = node.getText
        if (text != null && !text.toString.isEmpty) {
          val args = new Bundle
          import AccessibilityNodeInfo._
          args.putInt(ACTION_ARGUMENT_SELECTION_START_INT, 0)
          args.putInt(ACTION_ARGUMENT_SELECTION_END_INT, text.length)
          node.performAction(ACTION_SET_SELECTION, args)
          node.performAction(ACTION_CUT)
        }
      }

    def pasting = if (lollipopAndNewer) {
      Future.main {
        val args = new Bundle
        args.putCharSequence(AccessibilityNodeInfo.ACTION_ARGUMENT_SET_TEXT_CHARSEQUENCE, data)
        node.performAction(AccessibilityNodeInfo.ACTION_SET_TEXT, args)
      }
    } else {
      Future.main {
        this.systemService[ClipboardManager].setPrimaryClip(
          ClipData.newPlainText("", data))
      } ~ Future.main {
        node.performAction(AccessibilityNodeInfo.ACTION_PASTE)
      }
    }

    def epilogue = Future.main {
        node.performAction(AccessibilityNodeInfo.ACTION_CLEAR_SELECTION)
        this.systemService[ClipboardManager].setPrimaryClip(
          ClipData.newPlainText("", ""))
      } ~ Future.main {
        node.performAction(AccessibilityNodeInfo.ACTION_CLEAR_FOCUS)
      }

    // make sure all previous actions complete from each main
    // block before proceeding to next, each call within each
    // future may post items onto the ui thread iteself
    Await.ready(if (lollipopAndNewer) pasting else preamble ~ pasting ~ epilogue, Duration.Inf)
  }

}
