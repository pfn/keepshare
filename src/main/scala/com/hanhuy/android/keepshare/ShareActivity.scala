package com.hanhuy.android.keepshare

import RichLogger._

import collection.JavaConversions._

import android.app.Activity
import android.os.Bundle
import android.graphics.Bitmap
import android.graphics.drawable.BitmapDrawable
import android.content.Intent
import java.net.{URI, MalformedURLException, URL}
import android.widget.Toast

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
    v(getIntent.toString)
    val extras = getIntent.getExtras

    val values = extras.keySet() map { k => k -> extras.get(k) }

    val url = extras.getString(Intent.EXTRA_TEXT)

    try {
      new URL(url)


      val uri = new URI(url)
      val uris = goUp(uri) map (_.toString)
      val subhosts = ShareActivity.subhosts(uri.getHost)
      v(subhosts mkString "\n")

      v(uris mkString "\n")

      val subject = extras.getString(Intent.EXTRA_SUBJECT)

      if (extras.containsKey(EXTRA_SCREENSHOT)) {
        val bitmap: Bitmap = extras.getParcelable("share_screenshot")
        findView(TR.share_screenshot).setImageDrawable(
          new BitmapDrawable(getResources, bitmap))
      }
      v(getIntent.getExtras.toString)
      v(values mkString "\n")
    } catch {
      case e: MalformedURLException =>
        Toast.makeText(this, "Not a URL", Toast.LENGTH_SHORT).show()
        finish()
    }
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    Option(settings.get(Settings.VERIFY_DATA)) map { _ => init() } getOrElse {
      startActivityForResult(
        new Intent(this, classOf[MainActivity]), RequestCodes.REQUEST_SETUP)
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    if (request == RequestCodes.REQUEST_SETUP && result == Activity.RESULT_OK) {
      init()
    } else
      finish()
  }
}
