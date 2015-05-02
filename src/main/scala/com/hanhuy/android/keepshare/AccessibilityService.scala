package com.hanhuy.android.keepshare

import java.net.URI

import android.annotation.TargetApi
import android.app.{Notification, KeyguardManager, PendingIntent, NotificationManager}
import android.content._
import android.os.{Handler, HandlerThread, Bundle}
import android.support.v4.app.NotificationCompat
import android.view.accessibility.{AccessibilityNodeInfo, AccessibilityEvent}
import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.android.common._
import com.hanhuy.android.common.RichLogger._

import android.accessibilityservice.{AccessibilityService => Accessibility}

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.immutable.Queue

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
 * @author pfnguyen
 */
object AccessibilityService {
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
  class AccessibilityTree private(val node: Option[AccessibilityNodeInfo]) {
    lazy val children = node map { n =>
      childrenToSeq(n) map AccessibilityTree.apply
    } getOrElse Seq.empty


    @tailrec
    final def find(f: AccessibilityTree => Boolean, bfsQueue: Queue[AccessibilityTree] = Queue(this)): Option[AccessibilityTree] = bfsQueue.dequeueOption match {
      case Some((x, xs)) => if (f(x)) Some(x) else find(f, xs ++ x.children)
      case _ => None
    }
    def exists(f: AccessibilityTree => Boolean): Boolean = find(f).isDefined

    def collect[B](pf: PartialFunction[AccessibilityTree,B]): Seq[B] =
      if (pf isDefinedAt this)
        Vector(pf.apply(this)) else { Vector.empty } ++ _collect(pf)

    private def _collect[B](pf: PartialFunction[AccessibilityTree,B]): Seq[B] =
      (children collect pf) ++ (children map (_ _collect pf)).flatten

    // this kinda stinks, but should catch the vast majority of text fields
    def isEditable = node exists (_.isEditable)
    def isPassword = node exists (_.isPassword)
    def packageName = node map (_.getPackageName)
    def windowId = node map (_.getWindowId)

    def viewIdResourceName: Option[String] = node flatMap { n =>
      Option(n.getViewIdResourceName)
    }

    def findNodeById(id: String): Option[AccessibilityNodeInfo] =
      node flatMap { _.findAccessibilityNodeInfosByViewId(id).headOption }

    override def toString = {
      val t = node map { _.getClassName } getOrElse "null"
      val tx = node map { _.getText } getOrElse "null"
      val c = children map (_.toString)
      "Node[%s: id=%s password=%s text=%s children=[%s]]" format (
        t, viewIdResourceName, isPassword, tx, c mkString ",")
    }

    def dispose() {
      node foreach (_.recycle())
      children foreach (_.dispose())
    }
  }

  val EXCLUDED_PACKAGES = Set("com.hanhuy.android.keepshare",
    "com.android.systemui")
}

@TargetApi(18)
class AccessibilityService extends Accessibility with EventBus.RefOwner {
  val _implicits: RichContext = this
  import _implicits._
  import AccessibilityService._
  implicit val TAG = LogcatTag("AccessibilityService")
  private var lastWindowId: Option[Int] = None
  private var lastFoundWindowId: Option[Int] = None

  private val thread = new HandlerThread("AccessibilityService")
  private lazy val handler = {
    thread.start
    new Handler(thread.getLooper)
  }
  /**
   * @param windowId must match current view tree or no processing will occur
   * @param f (packageName, searchURI, view tree, password field)
   * @tparam A whatever you want
   * @return Option[A] result of f
   */
  def withTree[A](windowId: Int)(f: (String, Option[URI],
      AccessibilityTree, Option[AccessibilityTree]) => A): Option[A] = {

    val root = getRootInActiveWindow
    val tree = AccessibilityTree(root)
    // half overlays, like IME will show with a different window ID
    // notifications seem to put in the wrong package name as well
    // any views with systemui should be filtered out
    val r = if (tree.windowId.exists (_ == windowId) && !tree.exists (
        _.viewIdResourceName exists (_ startsWith "com.android.systemui"))) {
      val password = tree find (_.isPassword)

      // ugly
      val packageName = tree.packageName.get
      val searchURI = if (password.isDefined) {
        if (packageName == "com.android.chrome") {
          val urlbar = tree.findNodeById("com.android.chrome:id/url_bar")
          val d = urlbar map { u =>
            val url = u.getText.toString

            util.control.Exception.catching(classOf[Exception]) opt {
              new URI(if (url.indexOf(":/") < 0) "http://" + url else url)
            }
          } getOrElse None
          d
        } else if (packageName == "com.android.browser") {
          val urlbar = tree.findNodeById("com.android.browser:id/url")
          val d = urlbar map { u =>
            val url = u.getText.toString
            Some(new URI(if (url.indexOf(":/") < 0) "http://" + url else url))
          } getOrElse None
          d
        } else {
          val appHost = packageName.toString
            .split( """\.""").reverse.mkString(".")
          Some(new URI("android-package://" + appHost))
        }
      } else None
      Some(f(packageName, searchURI, tree, password))
    } else {
      d("Not handling because %s != %s; %s", tree.windowId, windowId, tree)
      None
    }

    tree.dispose()
    r
  }
  override def onAccessibilityEvent(event: AccessibilityEvent) {
    import AccessibilityEvent._

    // don't handle form fill for ourself and system
    val packageName = event.getPackageName.toString
    if (!EXCLUDED_PACKAGES(packageName) && filling) {
      val windowId = event.getWindowId
      event.getEventType match {
        case TYPE_WINDOW_CONTENT_CHANGED | TYPE_WINDOW_STATE_CHANGED =>

          lastCallback foreach handler.removeCallbacks
          val r: Runnable = { () =>
            withTree(windowId) { (pkg, searchURI, tree, password) =>

              if (password.isDefined) {

                fillInfo map (fillIn(_, pkg, searchURI, tree, password)) getOrElse {

                  searchURI foreach { uri =>
                    if (lastFoundWindowId != Option(windowId)) {
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

                      systemService[NotificationManager].notify(
                        Notifications.NOTIF_FOUND, builder.build())
                    }
                  }
                }

                lastFoundWindowId = Some(windowId)
              } else {
                if (lastFoundWindowId != Option(windowId)) {
                  systemService[NotificationManager].cancel(Notifications.NOTIF_FOUND)
                }
              }
            }
            lastWindowId = Some(windowId)
          }
          handler.post(r)
          lastCallback = Some(r)


        case TYPE_VIEW_HOVER_ENTER | TYPE_VIEW_HOVER_EXIT =>
          lastWindowId = Some(event.getWindowId)
        case _ =>
      }
    }
  }

  override def onInterrupt() = ()

  override def onCreate() {
    super.onCreate()
    d("Launching accessibility service")
    registerReceiver(receiver, Seq(ACTION_CANCEL, ACTION_SEARCH,
      Intent.ACTION_SCREEN_OFF, Intent.ACTION_USER_PRESENT))
    filling = !systemService[KeyguardManager].isKeyguardLocked
  }

  override def onDestroy() {
    super.onDestroy()
    d("Exiting accessibility service")
    thread.quit()
    unregisterReceiver(receiver)
    systemService[NotificationManager].cancel(Notifications.NOTIF_FOUND)
  }

  val receiver: BroadcastReceiver = (c: Context, intent: Intent) =>
    intent.getAction match {
      case Intent.ACTION_SCREEN_OFF =>
        filling = false
        systemService[NotificationManager].cancel(Notifications.NOTIF_FOUND)
      case Intent.ACTION_USER_PRESENT => filling = true
      case ACTION_CANCEL =>
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
             passwordField: Option[AccessibilityTree]): Unit = synchronized {
    d("Fill in %s == %s ?" format (event.pkg, pkg))
    if (event.pkg == pkg && passwordField.isDefined &&
        (searchURI.map (_.toString) == Option(event.uri))) {
      // needs to run on ui thread
      //val clipboard = systemService[ClipboardManager]
      //val clip = clipboard.getPrimaryClip

      fillInfo = None
      lastFoundWindowId = None
      val edits = tree collect { case t if t.isEditable => t }
      val text = (edits takeWhile (!_.isPassword)).lastOption

      // do password first because some username fields have autocompletes...
      passwordField foreach (_.node foreach { n =>
        pasteData(n, event.password) })

      text foreach (_.node foreach { n =>
        Thread.sleep(100)
        pasteData(n, event.username) })
      if (text.isEmpty)
        w("No username field for password form: " + tree)

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
    var lockCounter = 0
    val lock = new Object
    def block(i: Int) = lock.synchronized {
      if (lockCounter != i)
        lock.wait()
    }

    def clear(i: Int) = lock.synchronized {
      lockCounter = i
      lock.notify()
    }

    UiBus.post {
      node.performAction(AccessibilityNodeInfo.ACTION_FOCUS)
      clear(1)
    }
    block(1)

    UiBus.post {
      // unfortunately, this doesn't work for password fields...
      val text = node.getText
      if (text != null && !text.isEmpty) {
        val args = new Bundle
        import AccessibilityNodeInfo._
        args.putInt(ACTION_ARGUMENT_SELECTION_START_INT, 0)
        args.putInt(ACTION_ARGUMENT_SELECTION_END_INT, text.length)
        node.performAction(ACTION_SET_SELECTION, args)
        node.performAction(ACTION_CUT)
      }
      clear(2)
    }
    block(2)

    UiBus.post {
      systemService[ClipboardManager].setPrimaryClip(
        ClipData.newPlainText("", data))
      clear(3)
    }
    block(3)

    UiBus.post {
      node.performAction(AccessibilityNodeInfo.ACTION_PASTE)
      clear(4)
    }
    block(4)
    UiBus.post {
      systemService[ClipboardManager].setPrimaryClip(
        ClipData.newPlainText("", ""))
      clear(5)
    }
    block(5)
  }

}
