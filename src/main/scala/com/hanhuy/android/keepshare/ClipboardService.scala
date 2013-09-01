package com.hanhuy.android.keepshare

import AndroidConversions._

import android.app.{PendingIntent, Service}
import android.content._
import android.os.Handler
import android.support.v4.app.NotificationCompat
import java.util.UUID
import android.widget.Toast

object ClipboardService {
  val EXTRA_TITLE    = "com.hanhuy.android.keepshare.extra.TITLE"
  val EXTRA_USERNAME = "com.hanhuy.android.keepshare.extra.USERNAME"
  val EXTRA_PASSWORD = "com.hanhuy.android.keepshare.extra.PASSWORD"

  val EXTRA_UUID     = "com.hanhuy.android.keepshare.extra.UUID"

  val ACTION_COPY_USERNAME = "com.hanhuy.android.keepshare.action.COPY_USERNAME"
  val ACTION_COPY_PASSWORD = "com.hanhuy.android.keepshare.action.COPY_PASSWORD"
  val ACTION_CLEAR         = "com.hanhuy.android.keepshare.action.CLEAR"
}
class ClipboardService extends Service {
  import ClipboardService._
  val _implicit: RichContext = this
  import _implicit._
  def onBind(i: Intent) = null
  val handler = new Handler
  private var password: String = _
  private var username: String = _
  private var uuid: String = _

  private lazy val settings = new Settings(this)

  val finishRunnable: Runnable = () => {
    stopForeground(true)
    systemService[ClipboardManager].setText("")
    unregisterReceiver(receiver)
    stopSelf()
  }

  override def onStartCommand(intent: Intent, flags: Int, startId: Int) = {
    handler.removeCallbacks(finishRunnable)

    val title = intent.getStringExtra(EXTRA_TITLE)
    username  = intent.getStringExtra(EXTRA_USERNAME)
    password  = intent.getStringExtra(EXTRA_PASSWORD)

    // like a csrf token
    uuid      = UUID.randomUUID.toString

    val userIntent = new Intent(ACTION_COPY_USERNAME)
    userIntent.putExtra(EXTRA_UUID, uuid)
    val passIntent = new Intent(ACTION_COPY_PASSWORD)
    passIntent.putExtra(EXTRA_UUID, uuid)
    val clearIntent = new Intent(ACTION_CLEAR)
    clearIntent.putExtra(EXTRA_UUID, uuid)
    val builder = new NotificationCompat.Builder(this)
      .addAction(R.drawable.ic_menu_friendslist,
        getString(R.string.copy_username),
        PendingIntent.getBroadcast(this, 0, userIntent,
          PendingIntent.FLAG_UPDATE_CURRENT))
      .addAction(R.drawable.ic_menu_login, getString(R.string.copy_password),
        PendingIntent.getBroadcast(this, 1,
          passIntent, PendingIntent.FLAG_UPDATE_CURRENT))
      .addAction(android.R.drawable.ic_menu_delete, getString(R.string.clear),
        PendingIntent.getBroadcast(this, 2,
          clearIntent, PendingIntent.FLAG_UPDATE_CURRENT))
      .setContentText(username)
      .setContentTitle(title)
      .setSmallIcon(R.drawable.ic_lock)
      .setTicker(getString(R.string.credentials_available))
    handler.postDelayed(finishRunnable, settings.get(Settings.TIMEOUT) *1000)
    val filter = new IntentFilter
    filter.addAction(ACTION_COPY_USERNAME)
    filter.addAction(ACTION_COPY_PASSWORD)
    filter.addAction(ACTION_CLEAR)
    registerReceiver(receiver, filter)

    startForeground(1, builder.build)

    Service.START_STICKY
  }

  // can this receiver be protected? a malicious app that is aware of
  // this notification can:
  // detect the presence of the notification
  // send a broadcast for ACTION_COPY_XXX
  // scrape the clipboard for the resulting username and password
  val receiver = new BroadcastReceiver {
    def onReceive(c: Context, intent: Intent) {
      val cm = c.systemService[ClipboardManager]

      // "csrf" wannabe
      val token = intent.getStringExtra(EXTRA_UUID)
      if (uuid != token) {
        Toast.makeText(c, R.string.invalid_token, Toast.LENGTH_LONG).show()
      } else intent.getAction match {
        case ACTION_COPY_USERNAME => cm.setText(username)
          Toast.makeText(c, R.string.copied_username, Toast.LENGTH_SHORT).show()
        case ACTION_COPY_PASSWORD => cm.setText(password)
          Toast.makeText(c, R.string.copied_password, Toast.LENGTH_SHORT).show()
        case ACTION_CLEAR =>
          handler.removeCallbacks(finishRunnable)
          finishRunnable.run()
        case _ =>
      }
    }
  }
}
