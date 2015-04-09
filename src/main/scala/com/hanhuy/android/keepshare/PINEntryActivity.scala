package com.hanhuy.android.keepshare

import com.hanhuy.android.common.{UiBus, RichLogger, AndroidConversions, LogcatTag}
import AndroidConversions._
import RichLogger._

import android.app.{ProgressDialog, AlertDialog, Activity}
import android.os.Bundle
import android.view.{MenuItem, Menu, View}
import android.content.Intent

class PINEntryActivity extends Activity with TypedViewHolder {
  private implicit val TAG = LogcatTag("PINEntryActivity")
  lazy val prompt = findView(TR.pin_prompt)
  lazy val pinEntry = findView(TR.pin)
  lazy val ok = findView(TR.pin_ok)
  lazy val error = findView(TR.pin_error)
  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)

  private var pin = ""
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.enter_pin_subtitle))
    setContentView(R.layout.pin_setup)
    prompt.setText(R.string.verify_pin)
    setResult(Activity.RESULT_CANCELED)

    def validatePin() {
      pinEntry.setText(pin)
      ok.setEnabled(pin.length > 0)
    }

    val clearError: Runnable = () => {
      error.setText("")
    }

    def verifyPin() {
      val pinKey = PINHolderService.keyFor(pin)
      val verifier = settings.get(Settings.PIN_VERIFIER)

      val withKey = () => {
        val decrypted = try {
          KeyManager.cloudKey map { key =>
            KeyManager.decryptToString(pinKey,
              KeyManager.decryptToString(key, verifier))
          }
        } catch {
          case e: Exception =>
            v("Failed to decrypt", e)
            None
        }

        if (decrypted contains PINHolderService.PIN_VERIFIER) {
          val intent = new Intent(this, classOf[PINHolderService])
          intent.putExtra(PINHolderService.EXTRA_PIN, pin)
          startService(intent)
          setResult(Activity.RESULT_OK)
          finish()
        } else {
          error.setVisibility(View.VISIBLE)
          error.setText(R.string.try_again)
          pinEntry.setText("")
          pin = ""
          UiBus.handler.removeCallbacks(clearError)
          UiBus.handler.postDelayed(clearError, 1000)
        }
      }

      if (KeyManager.cloudKey.isEmpty) {
        val pd = ProgressDialog.show(this, getString(R.string.loading),
          getString(R.string.fetching_key), true, false)
        async {
          km.loadKey()
          UiBus.post {
            pd.dismiss()
            withKey()
          }
        }
      } else {
        withKey()
      }
    }
    val onClick = { view: View =>
      clearError()
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
