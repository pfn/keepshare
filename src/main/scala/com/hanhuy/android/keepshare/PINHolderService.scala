package com.hanhuy.android.keepshare

import RichLogger._
import AndroidConversions._

import android.app.Service
import android.content.Intent
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.{SecretKey, SecretKeyFactory}
import android.os.Handler

object PINHolderService {
  var instance = Option.empty[PINHolderService]

  val EXTRA_PIN = "com.hanhuy.android.keepshare.extra.PIN"

  val PIN_VERIFIER = EXTRA_PIN

  def keyFor(pin: String): SecretKey = {
    val spec = new PBEKeySpec(pin.toCharArray,
      "com.hanhuy.android.keepshare".getBytes("utf-8"), 1000, 256)
    val kf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    // must convert the KEY or else IV failure on 4.2 and below
    new SecretKeySpec(kf.generateSecret(spec).getEncoded, KeyManager.ALG)
  }
}

/** Makes a best effort at holding the user's PIN-key in memory for
  * the requested amount of time.
  */
class PINHolderService extends Service {
  import PINHolderService._
  implicit private val TAG = LogcatTag("PINHolderService")

  private val handler = new Handler

  lazy val settings = Settings(this)

  def pinKey: SecretKey =  {
    handler.removeCallbacks(finishRunner)
    handler.postDelayed(finishRunner,
      settings.get(Settings.PIN_TIMEOUT) * 60 * 1000)
    _key
  }
  private var _key: SecretKey = _

  def onBind(p1: Intent) = null

  override def onCreate() {
    instance = Some(this)
  }

  override def onDestroy() {
    instance = None
  }

  val finishRunner: Runnable = () => {
    stopSelf()
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int) = {
    val pin = intent.getStringExtra(EXTRA_PIN)
    _key = keyFor(pin)
    handler.removeCallbacks(finishRunner)
    handler.postDelayed(finishRunner,
      settings.get(Settings.PIN_TIMEOUT) * 60 * 1000)
    Service.START_NOT_STICKY
  }
}
