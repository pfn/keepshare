package com.hanhuy.android.keepshare

import AndroidConversions._

import android.app.Activity
import android.os.Bundle
import android.graphics.Bitmap
import android.graphics.drawable.BitmapDrawable
import android.content.{Context, Intent}
import java.net.{URI, MalformedURLException, URL}
import android.widget.{CursorAdapter, Toast}
import android.view.{ViewGroup, View}
import com.keepassdroid.provider.Contract
import android.database.Cursor

object ShareActivity {

  def subhosts(host: String): Seq[String] =
    host.split("""\.""").tails.toList filter (_.size > 1) map (_ mkString ".")

}
class ShareActivity extends Activity with TypedViewHolder {
  implicit val TAG = LogcatTag("ShareActivity")

  val EXTRA_SCREENSHOT = "share_screenshot"
  lazy val settings = new Settings(this)

  //@tailrec -- can't be due to concat
  private def goUp(uri: URI): Seq[URI] = {
    val u = uri.resolve("..").normalize
    if (u == uri)
      Seq(uri)
    else
      Seq(uri) ++ goUp(u)
  }

  def init() {
    setContentView(R.layout.share)
    val extras = getIntent.getExtras
    val url = extras.getString(Intent.EXTRA_TEXT)

    findView(TR.cancel) onClick (finish)

    try {
      new URL(url) // throws if malformed
      val km = new KeyManager(this, settings)

      val subject = extras.getString(Intent.EXTRA_SUBJECT)
      findView(TR.subject).setText(subject + " - " + url)

      if (extras.containsKey(EXTRA_SCREENSHOT)) {
        val bitmap: Bitmap = extras.getParcelable("share_screenshot")
        findView(TR.share_screenshot).setImageDrawable(
          new BitmapDrawable(getResources, bitmap))
      } else findView(TR.share_screenshot).setVisibility(View.GONE)

      async {
        val uri = new URI(url)
        val uris = goUp(uri) map (_.toString)
        val subhosts = ShareActivity.subhosts(uri.getHost)

        val cr = getContentResolver
        val r = cr.query(Contract.URI, null, "", null, null)
        var opened = true
        if (r == null) {
          opened = false
          km.accountName = settings.get(Settings.GOOGLE_USER)
          km.loadKey()
          val k = km.localKey
          val db = KeyManager.decryptToString(
            k, settings.get(Settings.DATABASE_FILE))
          val pw = KeyManager.decryptToString(
            k, settings.get(Settings.PASSWORD))
          val keyf = KeyManager.decryptToString(
            k, settings.get(Settings.KEYFILE_PATH))
          val verifier = KeyManager.decryptToString(
            k, settings.get(Settings.VERIFY_DATA))
          if (verifier != KeyManager.VERIFIER) {
            Toast.makeText(this,
              R.string.failed_verify, Toast.LENGTH_LONG).show()
            startActivityForResult(new Intent(this, classOf[SetupActivity]),
              RequestCodes.REQUEST_SETUP)
          } else {
            val b = new Bundle
            b.putString(Contract.EXTRA_DATABASE, db)
            b.putString(Contract.EXTRA_PASSWORD, pw)
            b.putString(Contract.EXTRA_KEYFILE, keyf)
            val res = cr.call(Contract.URI, Contract.METHOD_OPEN, null, b)
            if (res == null || res.containsKey(Contract.EXTRA_ERROR)) {
              Toast.makeText(
                this, getString(R.string.failed_to_open) +
                  res.getString(Contract.EXTRA_ERROR), Toast.LENGTH_LONG).show()
              startActivityForResult(new Intent(this, classOf[SetupActivity]),
                RequestCodes.REQUEST_SETUP)
            } else opened = true
          }
        }
        if (opened) {
          var cursor: Cursor = null
          uris find { uri =>
            cursor = cr.query(Contract.URI, null, uri, null, null)
            cursor.getCount > 0
          } orElse (subhosts find { host =>
            cursor = cr.query(Contract.URI, null, host, null, null)
            cursor.getCount > 0
          })
          UiBus.post {
            findView(TR.flipper).showNext()
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
            val list = findView(TR.list)
            list.setEmptyView(findView(TR.empty))

            val onClickHandler = { pos: Int =>
              val cursor = adapter.getItem(pos).asInstanceOf[Cursor]
              findView(TR.continu).setEnabled(true)
              findView(TR.continu).onClick {
              val intent = new Intent(this, classOf[ClipboardService])
              intent.putExtra(ClipboardService.EXTRA_TITLE,
              cursor.getString(cursor.getColumnIndex(Contract.TITLE)))
              intent.putExtra(ClipboardService.EXTRA_USERNAME,
              cursor.getString(cursor.getColumnIndex(Contract.USERNAME)))
              intent.putExtra(ClipboardService.EXTRA_PASSWORD,
              cursor.getString(cursor.getColumnIndex(Contract.PASSWORD)))
              startService(intent)
              finish()
            }
            }
            list.onItemClick(onClickHandler)
            list.setAdapter(adapter)
            if (adapter.getCount == 1) {
              findView(TR.select_prompt).setVisibility(View.GONE)
              list.setItemChecked(0, true)
              onClickHandler(0)
              findView(TR.continu).setEnabled(true)
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
        new Intent(this, classOf[SetupActivity]), RequestCodes.REQUEST_SETUP)
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    if (request == RequestCodes.REQUEST_SETUP && result == Activity.RESULT_OK) {
      init()
    } else
      finish()
  }
}
