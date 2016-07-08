package com.hanhuy.android.keepshare

import android.support.v7.app.AppCompatActivity
import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.android.common._
import android.app.{Activity, AlertDialog, ProgressDialog}
import android.os.{Bundle, Vibrator}
import android.view.{Menu, MenuItem, View}
import android.content.Intent
import Futures._
import org.acra.ACRA

import scala.util.Try

object PINEntryActivity {
  val EXTRA_NO_FINGERPRINT = "com.hanhuy.android.keepshare.extra.NO_FINGERPRINT"
  def requestPIN(a: Activity): Unit = {
    a.startActivityForResult(new Intent(a, classOf[PINEntryActivity]),
      RequestCodes.REQUEST_PIN)
  }
}
class PINEntryActivity extends AppCompatActivity with TypedFindView with DialogManager {
  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  lazy val vibrator = this.systemService[Vibrator]
  lazy val fpm = FingerprintManager(this, settings)
  lazy val fpmObs = fpm.authenticate()
  lazy val keys = List(views.pin_0, views.pin_1, views.pin_2, views.pin_3,
    views.pin_4, views.pin_5, views.pin_6, views.pin_7, views.pin_8, views.pin_9)
  private[this] var subscription = Option.empty[Sub]

  private[this] var pin = ""

  override def onDestroy() = {
    dismissAllDialogs()
    super.onDestroy()
  }

  override def onStart() = {
    super.onStart()
    def validatePin() {
      views.pin.setText(pin)
      vibrator.vibrate(20)
      views.pin_ok.setEnabled(pin.length > 0)
    }
    val failCount = settings.get(Settings.PIN_FAIL_COUNT)
    if (failCount > 0) {
      val failTime = settings.get(Settings.PIN_FAIL_TIME)
      val delay = (math.pow(2, failCount) * 5).toInt
      val unlockTime = failTime + delay * 1000
      val now = System.currentTimeMillis
      if (now < unlockTime) {
        val delayTime = unlockTime - now
        keys.foreach(_.setEnabled(false))
        views.pin_error.setVisibility(View.VISIBLE)
        views.pin_error.setText("Previous login failed, please wait")
        UiBus.handler.postDelayed(() => {
          keys.foreach(_.setEnabled(true))
          views.pin_error.setVisibility(View.GONE)
        }, delayTime)
      }
    }

    val cloudKey = km.fetchCloudKey()
    var verifyCount = 0
    def verifyPin() {
      verifyCount += 1
      val pinKey = PINHolderService.keyFor(pin)
      val verifier = settings.get(Settings.PIN_VERIFIER)

      cloudKey onSuccessMain { case key =>
        val decrypted = Try(KeyManager.decryptToString(pinKey,
          KeyManager.decryptToString(key, verifier))).toOption

        if (decrypted contains PINHolderService.PIN_VERIFIER) {
          settings.set(Settings.PIN_FAIL_COUNT, 0)
          PINHolderService.start(pin)
          setResult(Activity.RESULT_OK)
          fpm.registerPin(pin)
          finish()
        } else {
          views.pin_error.setVisibility(View.VISIBLE)
          views.pin_error.setText(R.string.try_again)
          views.pin.setText("")
          pin = ""
          UiBus.handler.removeCallbacks(clearError)
          UiBus.handler.postDelayed(clearError, 1000)
          if (verifyCount > 2) {
            settings.set(Settings.PIN_FAIL_COUNT, settings.get(Settings.PIN_FAIL_COUNT) + 1)
            settings.set(Settings.PIN_FAIL_TIME, System.currentTimeMillis)
            views.pin_error.setText(R.string.no_more_tries)
            setResult(Activity.RESULT_CANCELED)
            finish()
          }
        }
      }
      cloudKey.onFailureMain { case e =>
        views.pin_error.setVisibility(View.VISIBLE)
        views.pin_error.setText(R.string.key_changed_clear_data)
        Application.logException("cloudKey onFailureMain", e)
      }

      if (!cloudKey.isCompleted) {
        val d = showingDialog(ProgressDialog.show(this, getString(R.string.loading_key),
          getString(R.string.fetching_key), true, false))
        cloudKey onSuccessMain { case _ =>
          dismissDialog(d)
        }
      }
    }
    val onClick = { view: View =>
      clearError.run()
      view.getId match {
        case R.id.pin_9    => pin += "9"
          validatePin()
        case R.id.pin_8    => pin += "8"
          validatePin()
        case R.id.pin_7    => pin += "7"
          validatePin()
        case R.id.pin_6    => pin += "6"
          validatePin()
        case R.id.pin_5    => pin += "5"
          validatePin()
        case R.id.pin_4    => pin += "4"
          validatePin()
        case R.id.pin_3    => pin += "3"
          validatePin()
        case R.id.pin_2    => pin += "2"
          validatePin()
        case R.id.pin_1    => pin += "1"
          validatePin()
        case R.id.pin_0    => pin += "0"
          validatePin()
        case R.id.pin_ok   =>
          views.pin_ok.setEnabled(false)
          if (pin.nonEmpty)
            verifyPin()
        case R.id.pin_back =>
          pin = pin.dropRight(1)
          validatePin()
      }
    }

    keys ++ List(views.pin_ok, views.pin_back) foreach(_.onClick(onClick))
    if (fpm.hasFingerprints && allowFingerprint) {
      def fingerprintError(err: CharSequence, sub: Sub): Unit = {
        views.fingerprint_icon.animate().alpha(0.0f).setListener(BrowseActivity.animationEnd(
          _ => views.fingerprint_icon.setVisibility(View.GONE))).start()
        views.pin_error.setVisibility(View.VISIBLE)
        views.pin_error.setText("Fingerprint:  " + err)
        UiBus.handler.removeCallbacks(clearError)
        UiBus.handler.postDelayed(clearError, 5000)
        sub.unsubscribe()
      }
      views.fingerprint_icon.setVisibility(View.VISIBLE)
      import FingerprintManager._
      subscription = Some(fpmObs.subscribe2((r, sub) => r match {
        case FingerprintSuccess(fpin) =>
          sub.unsubscribe()
          views.pin_error.setVisibility(View.GONE)
          views.pin.setText("xxxxxx")
          pin = fpin
          views.fingerprint_icon.animate().rotation(360.0f).setDuration(250).setListener(
            BrowseActivity.animationEnd(_ => verifyPin())).start()
        case FingerprintFailure(errorString) =>
          views.pin_error.setVisibility(View.VISIBLE)
          views.pin_error.setText(errorString)
          UiBus.handler.removeCallbacks(clearError)
          UiBus.handler.postDelayed(clearError, 3000)
        case FingerprintException(ex) => fingerprintError(ex.getMessage, sub)
        case FingerprintUnavailable => fingerprintError(r.toString, sub)
        case FingerprintAuthenticationError(code, err) => fingerprintError(err, sub)
      }))
    }
  }

  def allowFingerprint = !Option(getIntent).exists(_.getBooleanExtra(PINEntryActivity.EXTRA_NO_FINGERPRINT, false))
  override def onStop() = {
    super.onStop()
    subscription.foreach(_.unsubscribe())
  }

  val clearError: Runnable = () => {
    views.pin_error.setText("")
  }

  lazy val views: TypedViewHolder.pin_setup = TypedViewHolder.setContentView(this, TR.layout.pin_setup)

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.enter_pin_subtitle))
    views.pin_prompt.setText(R.string.verify_pin)
    setResult(Activity.RESULT_CANCELED)

  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.main, menu)
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case R.id.clear_user_data =>
      new AlertDialog.Builder(this)
        .setTitle(R.string.clear_user_data)
        .setMessage(R.string.confirm_clear_data)
        .setPositiveButton(android.R.string.yes, { () =>
          settings.clear()
          KeyManager.clear()
          finish()
        })
        .setNegativeButton(android.R.string.no, null)
        .show()
      true
    case _ => super.onOptionsItemSelected(item)
  }
}
