package com.hanhuy.android.keepshare

import com.hanhuy.android.common._
import android.app.{Notification, NotificationManager, PendingIntent, Service}
import android.content.{BroadcastReceiver, Context, Intent}

import android.os.{Handler, SystemClock}
import android.support.v4.app.NotificationCompat
import android.widget.RemoteViews
import org.bouncycastle.crypto.digests.SHA1Digest
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter

import scala.util.Try

object Notifications {
  val NOTIF_FOUND = 0
  val NOTIF_DATABASE_UNLOCKED = 1
  val NOTIF_CREDENTIALS_READY = 2
  val NOTIF_DATABASE_SAVING = 3
}
object PINHolderService {
  var instance = Option.empty[PINHolderService]

  val EXTRA_PIN         = "com.hanhuy.android.keepshare.extra.PIN"
  val ACTION_CANCEL     = "com.hanhuy.android.keepshare.action.PIN_CANCEL"
  val ACTION_ADD_TIME_1 = "com.hanhuy.android.keepshare.action.ADD_TIME_1"
  val ACTION_SAVED_PIN  = "com.hanhuy.android.keepshare.action.SAVED_PIN"

  val PIN_VERIFIER = EXTRA_PIN

  def pbkdf2(pw: String): KeyManager.Key = {
    val gen = new PKCS5S2ParametersGenerator(new SHA1Digest)
    gen.init(pw.getBytes("utf-8"), "com.hanhuy.android.keepshare".getBytes("utf-8"), 1000)
    gen.generateDerivedParameters(256).asInstanceOf[KeyParameter].getKey
  }

  def keyFor(pin: String): KeyManager.Key = pbkdf2(pin)

  def start(pin: String): Unit = {
    val intent = new Intent(Application.instance, classOf[PINHolderService])
    intent.putExtra(PINHolderService.EXTRA_PIN, pin)
    Application.instance.startService(intent)
  }
  def startWithKey(key: String): Unit = {
    if (!BuildConfig.DEBUG)
      throw new IllegalStateException("This cannot run in RELEASE!")
    val intent = new Intent(Application.instance, classOf[PINHolderService])
    intent.setAction(ACTION_SAVED_PIN)
    intent.putExtra(PINHolderService.EXTRA_PIN, key)
    Application.instance.startService(intent)
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
  private val log = Logcat("PINHolderService")

  private val handler = new Handler

  lazy val nm = this.systemService[NotificationManager]
  lazy val settings = Settings(this)
  private var shutdownAt = SystemClock.uptimeMillis

  private def ping(): Unit = {
    handler.removeCallbacks(finishRunner)
    val pinTimeout = settings.get(Settings.PIN_TIMEOUT) * 60 * 1000
    shutdownAt = math.max(shutdownAt, SystemClock.uptimeMillis + pinTimeout)
    handler.postAtTime(finishRunner, shutdownAt)
    nm.notify(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
  }

  def pinKey: KeyManager.Key = {
    ping()
    _key
  }
  private var _key: KeyManager.Key = _

  def onBind(p1: Intent) = null

  override def onCreate() {
    instance = Some(this)
    ServiceBus.send(PINServiceStart)
  }

  override def onDestroy() {
    Database.close()
    instance = None
    ServiceBus.send(PINServiceExit)
    // should have been called before we can be destroyed
    handler.removeCallbacks(finishRunner)
  }

  val notificationRunner: Runnable = () => {
    nm.notify(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
    handler.postDelayed(notificationRunner, 1000)
  }


  def notification = {
    val remaining = (shutdownAt - SystemClock.uptimeMillis) / 1000
    val minutes = remaining / 60
    val seconds = remaining % 60
    val ms = f"$minutes%d:$seconds%02d"
    val view = new RemoteViews(getPackageName, R.layout.notification)
    view.setTextViewText(R.id.title, getString(R.string.pin_holder_notif_title, getString(R.string.appname)))
    view.setTextViewText(R.id.text, getString(R.string.pin_holder_notif_text, ms))
    view.setImageViewResource(R.id.icon, R.drawable.ic_launcher)
    view.setOnClickPendingIntent(R.id.action1, PendingIntent.getBroadcast(
        this, 0, new Intent(ACTION_ADD_TIME_1), PendingIntent.FLAG_UPDATE_CURRENT))
    view.setOnClickPendingIntent(R.id.action2, PendingIntent.getBroadcast(
      this, 0, new Intent(ACTION_CANCEL), PendingIntent.FLAG_UPDATE_CURRENT))
    val n = new NotificationCompat.Builder(this)
      .setPriority(Notification.PRIORITY_MIN)
      .setContent(view)
      .setSmallIcon(R.drawable.ic_lock)
      .setContentIntent(PendingIntent.getActivity(
      this, 0, new Intent(this, classOf[BrowseActivity]), PendingIntent.FLAG_UPDATE_CURRENT))
    .build
    n
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int) = {
    def startup(): Unit = {
      ping()
      handler.postDelayed(notificationRunner, 1000)
      startForeground(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
      registerReceiver(receiver, Seq(ACTION_CANCEL, ACTION_ADD_TIME_1))
    }
    if (ACTION_SAVED_PIN == intent.getAction && BuildConfig.DEBUG) {
      intent.getStringExtra(EXTRA_PIN).? match {
        case Some(key) =>
          _key = KeyManager.bytes(key)
          startup()
        case None =>
          stopSelf(startId)
      }
    } else {
      val pin = Option(intent.getStringExtra(EXTRA_PIN))
      pin match {
        case Some(p) =>
          _key = keyFor(p)
          startup()
        case None =>
          stopSelf(startId)
      }
    }
    Service.START_NOT_STICKY
  }

  val receiver = new BroadcastReceiver {
    override def onReceive(c: Context, i: Intent) {
      i.getAction match {
        case ACTION_CANCEL =>
          handler.removeCallbacks(finishRunner)
          finishRunner.run ()
        case ACTION_ADD_TIME_1 =>
          shutdownAt = shutdownAt + 60 * 1000
          handler.removeCallbacks(finishRunner)
          handler.postAtTime(finishRunner, shutdownAt)
          nm.notify(Notifications.NOTIF_DATABASE_UNLOCKED, notification)
        case _ =>
      }
    }
  }

  val finishRunner: Runnable = () => {
    Try(unregisterReceiver(receiver))
    handler.removeCallbacks(notificationRunner)
    stopForeground(true)
    stopSelf()
  }
}
