package com.hanhuy.android.keepshare

import android.support.v7.app.AppCompatActivity
import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.android.common._

import android.app.{Dialog, ProgressDialog, AlertDialog, Activity}
import android.os.{Vibrator, Bundle}
import android.view.{MenuItem, Menu, View}
import android.content.{DialogInterface, Intent}

import Futures._

import scala.util.Try

object PINEntryActivity {
  def requestPIN(a: Activity): Unit = {
    a.startActivityForResult(new Intent(a, classOf[PINEntryActivity]),
      RequestCodes.REQUEST_PIN)
  }
}
class PINEntryActivity extends AppCompatActivity with TypedFindView {
  private var currentDialog = Option.empty[Dialog]
  private val log = Logcat("PINEntryActivity")
  lazy val prompt = findView(TR.pin_prompt)
  lazy val pinEntry = findView(TR.pin)
  lazy val ok = findView(TR.pin_ok)
  lazy val error = findView(TR.pin_error)
  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  lazy val vibrator = this.systemService[Vibrator]

  private var pin = ""

  override def onDestroy() = {
    currentDialog foreach (_.dismiss())
    currentDialog = None
    super.onDestroy()
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.enter_pin_subtitle))
    setContentView(R.layout.pin_setup)
    prompt.setText(R.string.verify_pin)
    setResult(Activity.RESULT_CANCELED)
    val cloudKey = km.fetchCloudKey()

    def validatePin() {
      pinEntry.setText(pin)
      vibrator.vibrate(20)
      ok.setEnabled(pin.length > 0)
    }

    val clearError: Runnable = () => {
      error.setText("")
    }

    var verifyCount = 0
    def verifyPin() {
      verifyCount += 1
      val pinKey = PINHolderService.keyFor(pin)
      val verifier = settings.get(Settings.PIN_VERIFIER)

      cloudKey onSuccessMain { case key =>
        val decrypted = Try(KeyManager.decryptToString(pinKey,
            KeyManager.decryptToString(key, verifier))).toOption

        if (decrypted contains PINHolderService.PIN_VERIFIER) {
          PINHolderService.start(pin)
          setResult(Activity.RESULT_OK)
          finish()
        } else {
          error.setVisibility(View.VISIBLE)
          error.setText(R.string.try_again)
          pinEntry.setText("")
          pin = ""
          UiBus.handler.removeCallbacks(clearError)
          UiBus.handler.postDelayed(clearError, 1000)
          if (verifyCount > 2) {
            error.setText(R.string.no_more_tries)
            setResult(Activity.RESULT_CANCELED)
            finish()
          }
        }
      }

      if (!cloudKey.isCompleted) {
        currentDialog = Some(ProgressDialog.show(this, getString(R.string.loading_key),
          getString(R.string.fetching_key), true, false))
        cloudKey onSuccessMain { case _ =>
          currentDialog foreach (_.dismiss())
          currentDialog = None
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
          ok.setEnabled(false)
          verifyPin()
        case R.id.pin_back =>
          pin = pin.dropRight(1)
          validatePin()
      }
    }

    Seq(R.id.pin_9, R.id.pin_8, R.id.pin_7,
      R.id.pin_6, R.id.pin_5, R.id.pin_4,
      R.id.pin_3, R.id.pin_2, R.id.pin_1,
      R.id.pin_0, R.id.pin_ok, R.id.pin_back) foreach {
      findViewById(_).onClick(onClick)
    }
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
