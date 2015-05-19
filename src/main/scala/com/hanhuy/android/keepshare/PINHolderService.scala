package com.hanhuy.android.keepshare

import com.hanhuy.android.common.{ServiceBus, LogcatTag, AndroidConversions, RichLogger}
import RichLogger._
import AndroidConversions._

import android.app.{NotificationManager, Notification, PendingIntent, Service}
import android.content.{IntentFilter, Context, BroadcastReceiver, Intent}
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}
import javax.crypto.{SecretKey, SecretKeyFactory}
import android.os.{SystemClock, Handler}
import android.support.v4.app.NotificationCompat

object Notifications {
  val NOTIF_FOUND = 0
  val NOTIF_DATABASE_UNLOCKED = 1
  val NOTIF_CREDENTIALS_READY = 2
  val NOTIF_DATABASE_SAVING = 3
}
object PINHolderService {
  var instance = Option.empty[PINHolderService]

  val EXTRA_PIN = "com.hanhuy.android.keepshare.extra.PIN"
  val ACTION_CANCEL = "com.hanhuy.android.keepshare.action.PIN_CANCEL"

  val PIN_VERIFIER = EXTRA_PIN

  def keyFor(pin: String): SecretKey = {
    val spec = new PBEKeySpec(pin.toCharArray,
      "com.hanhuy.android.keepshare".getBytes("utf-8"), 1000, 256)
    val kf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    // must convert the KEY or else IV failure on 4.2 and below
    new SecretKeySpec(kf.generateSecret(spec).getEncoded, KeyManager.ALG)
  }

  def ping(): Unit = {
    instance foreach (_.ping())
  }
}

/** Makes a best effort at holding the user's PIN-key in memory for
  * the requested amount of time.
  */
class PINHolderService extends Service {
  import PINHolderService._
  implicit private val TAG = LogcatTag("PINHolderService")

  private val handler = new Handler

  lazy val nm = this.systemService[NotificationManager]
  lazy val settings = Settings(this)
  private var shutdownAt = SystemClock.uptimeMillis

  private def ping(): Unit = {
    handler.removeCallbacks(finishRunner)
    val pinTimeout = settings.get(Settings.PIN_TIMEOUT) * 60 * 1000
    shutdownAt = SystemClock.uptimeMillis + pinTimeout
    handler.postAtTime(finishRunner, shutdownAt)
    nm.notify(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
  }

  def pinKey: SecretKey = {
    ping()
    _key
  }
  private var _key: SecretKey = _

  def onBind(p1: Intent) = null

  override def onCreate() {
    instance = Some(this)
    ServiceBus.send(PINServiceStart)
  }

  override def onDestroy() {
    Database.close()
    instance = None
    ServiceBus.send(PINServiceExit)
  }

  val notificationRunner: Runnable = () => {
    nm.notify(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
    handler.postDelayed(notificationRunner, 1000)
  }

  val finishRunner: Runnable = () => {
    unregisterReceiver(receiver)
    handler.removeCallbacks(notificationRunner)
    stopForeground(true)
    stopSelf()
  }

  def notification = {
    val remaining = (shutdownAt - SystemClock.uptimeMillis) / 1000
    val minutes = remaining / 60
    val seconds = remaining % 60
    val ms = f"($minutes%d:$seconds%02d)"
    new NotificationCompat.Builder(this)
      .setPriority(Notification.PRIORITY_MIN)
      .setContentText(getString(R.string.pin_holder_notif_text) + " " + ms)
      .setContentTitle(getString(R.string.pin_holder_notif_title, getString(R.string.appname)))
      .setSmallIcon(R.drawable.ic_lock)
      .setContentIntent(PendingIntent.getBroadcast(
      this, 0, new Intent(ACTION_CANCEL), PendingIntent.FLAG_UPDATE_CURRENT))
    .build
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int) = {
    val pin = intent.getStringExtra(EXTRA_PIN)
    _key = keyFor(pin)
    ping()
    handler.postDelayed(notificationRunner, 1000)
    startForeground(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
    registerReceiver(receiver, ACTION_CANCEL)
    Service.START_NOT_STICKY
  }

  val receiver = new BroadcastReceiver {
    override def onReceive(c: Context, i: Intent) {
      handler.removeCallbacks(finishRunner)
      finishRunner.run()
    }
  }
}
