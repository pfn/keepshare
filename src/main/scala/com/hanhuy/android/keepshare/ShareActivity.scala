package com.hanhuy.android.keepshare

import AndroidConversions._
import RichLogger._

import android.provider.Settings.Secure
import android.app.Activity
import android.os.Bundle
import android.graphics.Bitmap
import android.graphics.drawable.BitmapDrawable
import android.content.{ContentResolver, Context, Intent}
import java.net.{URI, MalformedURLException, URL}
import android.widget.{Adapter, CursorAdapter, Toast}
import android.view.{ViewGroup, View}
import com.keepassdroid.provider.Contract
import android.database.Cursor
import android.view.inputmethod.InputMethodManager

object ShareActivity {

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
                    query: Seq[String]): Cursor = {
    val km = new KeyManager(c, settings)
    val cr = c.getContentResolver
    val r = cr.query(Contract.URI, null, "", null, null)
    var opened = true
    if (r == null) {
      opened = false
      val googleUser = settings.get(Settings.GOOGLE_USER)
      if (googleUser != null) {
        km.accountName = settings.get(Settings.GOOGLE_USER)
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
                    c.getString(R.string.app_name) +
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
            b.putString(Contract.EXTRA_DATABASE, db)
            b.putString(Contract.EXTRA_PASSWORD, pw)
            b.putString(Contract.EXTRA_KEYFILE, keyf)
            val res = cr.call(Contract.URI, Contract.METHOD_OPEN, null, b)
            if (res == null || res.containsKey(Contract.EXTRA_ERROR)) {
              Toast.makeText(c, c.getString(R.string.failed_to_open) +
                  res.getString(Contract.EXTRA_ERROR),
                Toast.LENGTH_LONG).show()
              c match {
                case a: Activity =>
                  a.startActivityForResult(SetupActivity.intent,
                    RequestCodes.REQUEST_SETUP)
                case _ =>
              }
            } else opened = true
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
      var cursor: Cursor = null
      query find { q =>
        cursor = cr.query(Contract.URI, null, q, null, null)
        val hasResults = cursor.getCount > 0
        if (!hasResults)
          cursor.close()
        hasResults
      }
      cursor
    } else
      null
  }
  def selectHandler(a: Activity, settings: Settings, cursor: Cursor) = {
    val imm = a.systemService[InputMethodManager]
    val intent = new Intent(a, classOf[ClipboardService])
    intent.putExtra(ClipboardService.EXTRA_TITLE,
      cursor.getString(cursor.getColumnIndex(Contract.TITLE)))
    intent.putExtra(ClipboardService.EXTRA_USERNAME,
      cursor.getString(cursor.getColumnIndex(Contract.USERNAME)))
    intent.putExtra(ClipboardService.EXTRA_PASSWORD,
      cursor.getString(cursor.getColumnIndex(Contract.PASSWORD)))
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
class ShareActivity extends Activity with TypedViewHolder {
  implicit val TAG = LogcatTag("ShareActivity")
  val _implicit: RichContext = this
  import _implicit._

  val EXTRA_SCREENSHOT = "share_screenshot"
  lazy val settings = new Settings(this)


  def init() {
    setContentView(R.layout.share)
    val extras = getIntent.getExtras
    val url = extras.getString(Intent.EXTRA_TEXT)

    findView(TR.cancel) onClick finish

    try {
      new URL(url) // throws if malformed

      val subject = extras.getString(Intent.EXTRA_SUBJECT)
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
        val cursor = ShareActivity.queryDatabase(this, settings,
          uris ++ subhosts)

        if (cursor != null) {

          UiBus.post {
            findView(TR.flipper).showNext()
            val list = findView(TR.list)
            list.setEmptyView(findView(TR.empty))

            if (!cursor.isClosed) {

              val adapter = new CursorAdapter(this, cursor, false) {
                def newView(p1: Context, cursor: Cursor, c: ViewGroup) = {
                  val view = getLayoutInflater.inflate(R.layout.pwitem, c, false)
                  view.findView(TR.name).setText(
                    cursor.getString(cursor.getColumnIndex(Contract.TITLE)))
                  view.findView(TR.username).setText(
                    cursor.getString(cursor.getColumnIndex(Contract.USERNAME)))
                  view
                }

                def bindView(view: View, c: Context, cursor: Cursor) {
                  view.findView(TR.name).setText(
                    cursor.getString(cursor.getColumnIndex(Contract.TITLE)))
                  view.findView(TR.username).setText(
                    cursor.getString(cursor.getColumnIndex(Contract.USERNAME)))
                }
              }
              val onClickHandler = { pos: Int =>
                val cursor = adapter.getItem(pos).asInstanceOf[Cursor]
                findView(TR.continu).setEnabled(true)
                findView(TR.continu).onClick (
                  ShareActivity.selectHandler(this, settings, cursor))
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
    } catch {
      case e: MalformedURLException =>
        Toast.makeText(this, R.string.not_a_url, Toast.LENGTH_SHORT).show()
        finish()
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
