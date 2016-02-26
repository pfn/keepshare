package com.hanhuy.android.keepshare

import java.net.URLDecoder

import android.os.Bundle
import android.support.v7.app.AppCompatActivity
import android.widget.Toast

import com.hanhuy.android.common._
import com.hanhuy.android.keepshare.Database.{Hotp, Totp}
import com.hanhuy.android.keepshare.EntryViewActivity.EntryCreateData

import collection.JavaConverters._
import scala.util.Try

/**
  * @author pfnguyen
  */
class CreateFromUrlActivity extends AppCompatActivity {
  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    handleIntent()
    finish()
  }

  def handleIntent(): Unit = getIntent.? flatMap (_.getData.?) foreach { uri =>
    if (uri.getScheme == "otpauth") {
      val method = uri.getAuthority match {
        case "totp" =>
          Totp(uri.getQueryParameter("period").?.flatMap(s => Try(s.toInt).toOption) getOrElse 30).?
        case "hotp" =>
          Hotp(uri.getQueryParameter("counter").?.flatMap(s => Try(s.toLong).toOption) getOrElse 0l).?
        case _ => None
      }
      val segments = uri.getPathSegments.asScala
      val lastSegment = uri.getLastPathSegment

      val issuer    = uri.getQueryParameter("issuer").? orElse
        (if (segments.size > 1) segments.headOption else None) orElse
        lastSegment.split(':').headOption.map(s => URLDecoder.decode(s, "utf-8"))

      val identity  = lastSegment.split(':').lastOption.map(s => URLDecoder.decode(s, "utf-8"))

      val size    = uri.getQueryParameter("digits").?.flatMap(s => Try(s.toInt).toOption).getOrElse(6)
      val secret    = uri.getQueryParameter("secret").?
      val algorithm = uri.getQueryParameter("algorithm").?.fold("SHA1")(_.toUpperCase)

      val data = for {
        m <- method
        s <- secret
      } yield {
        val (mode,sizekey,addl) = m match {
          case Hotp(counter) => (EntryViewActivity.HOTP_MODE,
            "HmacOtp-Size",
            (false, "HmacOtp-Counter", counter.toString) :: Nil)
          case Totp(step)    => (EntryViewActivity.TOTP_MODE,
            "TimeOtp-Size",
            (false, "TimeOtp-Step",    step.toString) :: Nil)
        }
        EntryCreateData(title = issuer, username = identity, fields =
          (false, mode, "true") ::
            (false, sizekey, size.toString) ::
            (true, "HmacOtp-Secret-Base32", s) ::
            (false, "HmacOtp-Algorithm", algorithm) ::
            Nil ++ addl)
      }
      if (data.isEmpty)
        Toast.makeText(this, "Unable to parse 'otpauth' from: " + uri, Toast.LENGTH_LONG).show()
      data.foreach { _ => EntryViewActivity.create(this, None, data) }
    }
  }
}
