package com.hanhuy.android.keepshare

import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.android.common._
import com.hanhuy.android.common.RichLogger._
import com.hanhuy.keepassj.{PwDefs, PwEntry}

import collection.JavaConversions._

import android.provider.Settings.Secure
import android.app.{AlertDialog, Activity}
import android.os.Bundle
import android.graphics.Bitmap
import android.graphics.drawable.BitmapDrawable
import android.content.{ComponentName, ContentResolver, Context, Intent}
import java.net.URI
import android.widget.{BaseAdapter, Adapter, CursorAdapter, Toast}
import android.view.{ViewGroup, View}
import android.database.Cursor
import android.view.inputmethod.InputMethodManager

import TypedResource._

import scala.util.Try

object ShareActivity {
  implicit val TAG = LogcatTag("ShareActivity")

  def subhosts(host: String): Seq[String] =
    host.split("""\.""").tails.toList filter (_.size > 1) map (_ mkString ".")

  //@tailrec -- can't be due to concat
  def goUp(uri: URI): Seq[URI] = {
    val u = uri.resolve("..").normalize
    if (u == uri)
      Seq(uri)
    else
      Seq(uri) ++ goUp(u)
  }
  def queryDatabase(c: Context, settings: Settings,
                    query: Seq[String]): Option[List[PwEntry]] = {
    v("possible queries: " + query)
    val km = new KeyManager(c, settings)
    var opened = true
    if (!Database.isOpen) {
      opened = false
      if (!settings.get(Settings.FIRST_RUN)) {
        km.loadKey()
        km.getConfig match {
          case Left(err) => err match {
            case KeyError.NeedPin =>
              c match {
                case a: Activity =>
                  a.startActivityForResult(
                    new Intent(c, classOf[PINEntryActivity]),
                    RequestCodes.REQUEST_PIN)
                case _ => UiBus.post {
                  Toast.makeText(c,
                    c.getString(R.string.appname) +
                      c.getString(R.string.pin_unlock_required),
                    Toast.LENGTH_SHORT).show()
                }
              }
            case _ =>
              UiBus.post {
                Toast.makeText(c,
                  R.string.failed_verify, Toast.LENGTH_LONG).show()
              }
              c match {
                case a: Activity =>
                  a.startActivityForResult(SetupActivity.intent,
                    RequestCodes.REQUEST_SETUP)
                case _ =>
              }
          }
          case Right((db, pw, keyf)) =>
            val b = new Bundle
            Try(Database.open(db, Option(pw), Option(keyf))) map (
              _ => opened = true) recover {
              case e: Exception =>
                Toast.makeText(c, c.getString(R.string.failed_to_open) +
                  e.getMessage,
                  Toast.LENGTH_LONG).show()
                c match {
                  case a: Activity =>
                    a.startActivityForResult(SetupActivity.intent,
                      RequestCodes.REQUEST_SETUP)
                  case _ =>
                }
            }
        }
      } else {
        c match {
          case a: Activity =>
            a.startActivityForResult(SetupActivity.intent,
              RequestCodes.REQUEST_SETUP)
          case _ =>
        }
      }
    }
    if (opened) {
      query collectFirst Function.unlift(Database.search)
    } else
      None
  }
  def selectHandler(a: Activity, settings: Settings, entry: PwEntry) = {
    val imm = a.systemService[InputMethodManager]
    val keyboardEnabled =  imm.getEnabledInputMethodList exists {
      _.getPackageName == "com.hanhuy.android.keepshare" }

    if (!keyboardEnabled) {
      val d = new AlertDialog.Builder(a)
        .setTitle(R.string.keyboard_not_enabled_title)
        .setMessage(R.string.keyboard_not_enabled_prompt)
        .setPositiveButton(android.R.string.yes, {() =>
          val intent = new Intent
          intent.setComponent(new ComponentName("com.android.settings",
            "com.android.settings.LanguageSettings"))
          a.startActivity(intent)
        })
        .setNegativeButton(android.R.string.no, {() => a.finish()})
        .show()
    } else {
      val intent = new Intent(a, classOf[CredentialHolderService])
      intent.putExtra(CredentialHolderService.EXTRA_TITLE,
        Database.getField(entry, PwDefs.TitleField) orNull
      )
      intent.putExtra(CredentialHolderService.EXTRA_USERNAME,
        Database.getField(entry, PwDefs.UserNameField) orNull
      )
      intent.putExtra(CredentialHolderService.EXTRA_PASSWORD,
        Database.getField(entry, PwDefs.PasswordField) orNull
      )
      a.startService(intent)
      UiBus.post {
        val token = a.getWindow.getAttributes.token
        imm.setInputMethod(token, PasswordIME.NAME)
        val ime = Secure.getString(
          a.getContentResolver, Secure.DEFAULT_INPUT_METHOD)
        if (PasswordIME.NAME != ime) {
          settings.set(Settings.IME, ime)
          UiBus.handler.delayed(500) { imm.showInputMethodPicker() }
        }
        UiBus.post {
          a.finish()
        }
      }
    }
  }
}
class ShareActivity extends Activity with TypedViewHolder {
  import ShareActivity._
  val _implicit: RichActivity = this
  import _implicit._

  val EXTRA_SCREENSHOT = "share_screenshot"
  lazy val settings = new Settings(this)


  def init() {
    setContentView(R.layout.share)
    val extras = getIntent.getExtras
    val (url, subject) = {
      val u = extras.getString(Intent.EXTRA_TEXT)
      val s = extras.getString(Intent.EXTRA_SUBJECT)
      if (u.charAt(0) == '"') {
        val end = u.lastIndexOf('"')
        if (u.length > end + 2)
          (u.substring(end + 2, u.length).trim, u.substring(1, end))
        else
          (u, s)
      } else (u, s)
    }

    findView(TR.cancel) onClick {
      ServiceBus.send(ShareActivityCancel)
      finish()
    }

    if (url.indexOf(":") == -1) {
      Toast.makeText(this, R.string.not_a_url, Toast.LENGTH_SHORT).show()
      finish()
      return
    }

    findView(TR.subject).setText(subject + " - " + url)

    if (extras.containsKey(EXTRA_SCREENSHOT)) {
      val bitmap: Bitmap = extras.getParcelable("share_screenshot")
      findView(TR.share_screenshot).setImageDrawable(
        new BitmapDrawable(getResources, bitmap))
    } else findView(TR.share_screenshot).setVisibility(View.GONE)

    async {
      val uri = new URI(url)
      val uris = ShareActivity.goUp(uri) map (_.toString)
      val subhosts = ShareActivity.subhosts(uri.getHost)
      val results = ShareActivity.queryDatabase(this, settings, uris ++ subhosts)

      results foreach { result =>

        UiBus.post {
          findView(TR.flipper).showNext()
          val list = findView(TR.list)
          list.setEmptyView(findView(TR.empty))

            val adapter = new BaseAdapter {
              override def getItemId(i: Int) = i

              override def getCount = result.size

              override def getView(i: Int, view: View, c: ViewGroup) = {
                val row = Option(view) getOrElse {
                  getLayoutInflater.inflate(R.layout.pwitem, c, false)
                }
                row.findView(TR.name).setText(
                  Database.getField(getItem(i), PwDefs.TitleField) orNull)
                row.findView(TR.username).setText(
                  Database.getField(getItem(i), PwDefs.UserNameField) orNull)
                row
              }

              override def getItem(i: Int) = result(i)
            }
            val onClickHandler = { pos: Int =>
              findView(TR.continu).setEnabled(true)
              findView(TR.continu).onClick {
                ShareActivity.selectHandler(this, settings, adapter.getItem(pos))
              }
            }
            list.onItemClick(onClickHandler)
            list.setAdapter(adapter)
            if (adapter.getCount < 2) {
              findView(TR.select_prompt).setVisibility(View.GONE)
              if (adapter.getCount == 1) {
                list.setItemChecked(0, true)
                onClickHandler(0)
                findView(TR.continu).setEnabled(true)
              }
            }
        }
      }
    }
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    Option(settings.get(Settings.VERIFY_DATA)) map { _ => init() } getOrElse {
      startActivityForResult(
        SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    val success = request match {
      case RequestCodes.REQUEST_SETUP => result == Activity.RESULT_OK
      case RequestCodes.REQUEST_PIN => result == Activity.RESULT_OK
    }
    if (success) init() else finish()
  }
}
