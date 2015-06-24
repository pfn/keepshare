package com.hanhuy.android.keepshare

import com.hanhuy.android.common._

import android.app.{PendingIntent, Service}
import android.content._
import android.os.Handler
import android.support.v4.app.NotificationCompat
import android.view.inputmethod.InputMethodManager
import android.provider.Settings.Secure
import com.hanhuy.android.common.{EventBus, ServiceBus}

object CredentialHolderService {
  val EXTRA_TITLE    = "com.hanhuy.android.keepshare.extra.TITLE"
  val EXTRA_USERNAME = "com.hanhuy.android.keepshare.extra.USERNAME"
  val EXTRA_PASSWORD = "com.hanhuy.android.keepshare.extra.PASSWORD"

  var instance: Option[CredentialHolderService] = None

  val ACTION_IME = "com.hanhuy.android.keepshare.action.IME"
  val ACTION_CANCEL = "com.hanhuy.android.keepshare.action.CRED_CANCEL"
}

/** Holds selected credentials in memory for the requested time
  */
class CredentialHolderService extends Service with EventBus.RefOwner {
  import CredentialHolderService._
  def onBind(i: Intent) = null
  val handler = new Handler

  protected[keepshare] var password: String = _
  protected[keepshare] var username: String = _

  private lazy val settings = new Settings(this)

  val finishRunnable: Runnable = () => {
    instance foreach { _ =>
      stopForeground(true)
      stopSelf()
    }
    instance = None
  }

  ServiceBus += {
    case KeyboardExit =>
      handler.removeCallbacks(finishRunnable)
      finishRunnable.run()
  }

  override def onCreate() {
    val filter = new IntentFilter
    filter.addAction(ACTION_IME)
    filter.addAction(ACTION_CANCEL)
    registerReceiver(receiver, filter)
  }
  override def onDestroy() {
    unregisterReceiver(receiver)
    ServiceBus.send(ServiceExit)
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int): Int = {
    if (intent == null) return Service.START_NOT_STICKY

    instance = Some(this)
    handler.removeCallbacks(finishRunnable)

    val title = intent.getStringExtra(EXTRA_TITLE)
    username  = intent.getStringExtra(EXTRA_USERNAME)
    password  = intent.getStringExtra(EXTRA_PASSWORD)

    val builder = new NotificationCompat.Builder(this)
      .setContentText(username)
      .setContentTitle(getString(R.string.appname) + ": " + title)
      .addAction(android.R.drawable.ic_menu_delete, getString(R.string.clear),
      PendingIntent.getBroadcast(this, 0,
        new Intent(ACTION_CANCEL), PendingIntent.FLAG_UPDATE_CURRENT))
      .setSmallIcon(R.drawable.ic_lock)
      .setContentIntent(PendingIntent.getBroadcast(
        this, 0, new Intent(ACTION_IME), PendingIntent.FLAG_UPDATE_CURRENT))
      .setTicker(getString(R.string.credentials_available))

    handler.postDelayed(finishRunnable,
      settings.get(Settings.KEYBOARD_TIMEOUT) * 1000)

    startForeground(Notifications.NOTIF_CREDENTIALS_READY, builder.build)
    Service.START_STICKY
  }

  val receiver = new BroadcastReceiver {
    def onReceive(c: Context, intent: Intent) {
      intent.getAction match {
        case ACTION_CANCEL =>
          handler.removeCallbacks(finishRunnable)
          finishRunnable.run()
        case ACTION_IME =>
          val ime = Secure.getString(
            getContentResolver, Secure.DEFAULT_INPUT_METHOD)
          if (PasswordIME.NAME != ime) {
            settings.set(Settings.IME, ime)
            c.systemService[InputMethodManager].showInputMethodPicker()
          }
      }
    }
  }
}
