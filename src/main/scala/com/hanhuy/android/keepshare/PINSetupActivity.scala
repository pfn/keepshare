package com.hanhuy.android.keepshare

import android.support.v7.app.AppCompatActivity
import com.hanhuy.android.common._
import com.hanhuy.android.extensions._
import android.app.Activity
import android.os.Bundle
import android.view.View
import com.hanhuy.android.common.{Futures, UiBus}
import Futures._
import android.widget.Toast

class PINSetupActivity extends AppCompatActivity {

  lazy val views: TypedViewHolder.pin_setup = TypedViewHolder.setContentView(this, TR.layout.pin_setup)
  private var pin = Seq.empty[String]
  private var selectedPin = pin
  lazy val settings = Settings(this)

  private val clearError: Runnable = () => {
    views.pin_error.setVisibility(View.INVISIBLE)
  }

  private def verifyMatch() {
    if (pin == selectedPin) {
      val thePin = pin mkString ""
      PINHolderService.start(thePin)
      val km = new KeyManager(this, settings)
      val waitforit = for {
        key <- km.fetchCloudKey()
        lk1 <- km.localKey
      } yield {
        lk1.right foreach { localKey =>
          val pinKey = PINHolderService.keyFor(thePin)
          val newkey = KeyManager.encrypt(key, KeyManager.encrypt(
            pinKey, localKey.getEncoded))
          settings.set(Settings.LOCAL_KEY, newkey)
          settings.set(Settings.NEEDS_PIN, true)
          settings.set(Settings.PIN_TIMESTAMP, System.currentTimeMillis)
          FingerprintManager(this, settings).registerPin(thePin)
          settings.set(Settings.PIN_VERIFIER,
            KeyManager.encrypt(key, KeyManager.encrypt(pinKey,
              PINHolderService.PIN_VERIFIER)))
          setResult(Activity.RESULT_OK)
          finish()
        }
        lk1.left foreach { ex =>
          throw new IllegalStateException("Local key is not available: " + ex)
        }
      }

      waitforit.onFailureMain { case e =>
          Toast.makeText(this, "Failed to save new PIN: " + e.getMessage, Toast.LENGTH_LONG).show()
        Application.logException("waitforit onFailureMain", e)
      }
    }
    else {
      views.pin_error.setVisibility(View.VISIBLE)
      views.pin_error.setText(R.string.try_again)
      views.pin.setText("")
      pin = Seq.empty
      UiBus.handler.removeCallbacks(clearError)
      UiBus.handler.postDelayed(clearError, 1000)
    }
  }

  private def validatePin() {
    clearError.run()
    views.pin.setText(pin mkString "")
    if (selectedPin.nonEmpty) {
      views.pin_ok.setEnabled(true)
      views.pin_error.setVisibility(View.INVISIBLE)
    } else if (pin.size < 4 && pin.nonEmpty) {
      views.pin_error.setVisibility(View.VISIBLE)
      views.pin_error.setText(R.string.pin_at_least_4)
      views.pin_ok.setEnabled(false)
    } else {
      views.pin_error.setVisibility(View.INVISIBLE)
      if (pin.nonEmpty)
        views.pin_ok.setEnabled(true)
    }
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.setup_pin_subtitle))
    setContentView(R.layout.pin_setup)

    val onClick = { view: View =>
      view.getId match {
        case R.id.pin_9    => pin :+= "9"
          validatePin()
        case R.id.pin_8    => pin :+= "8"
          validatePin()
        case R.id.pin_7    => pin :+= "7"
          validatePin()
        case R.id.pin_6    => pin :+= "6"
          validatePin()
        case R.id.pin_5    => pin :+= "5"
          validatePin()
        case R.id.pin_4    => pin :+= "4"
          validatePin()
        case R.id.pin_3    => pin :+= "3"
          validatePin()
        case R.id.pin_2    => pin :+= "2"
          validatePin()
        case R.id.pin_1    => pin :+= "1"
          validatePin()
        case R.id.pin_0    => pin :+= "0"
          validatePin()
        case R.id.pin_ok   =>
          if (selectedPin.nonEmpty) {
            verifyMatch()
          } else {
            selectedPin = pin
            pin = Seq.empty
            views.pin_ok.setEnabled(false)
            views.pin_prompt.setText(R.string.confirm_pin)
            views.pin.setText("")
          }
        case R.id.pin_back =>
          pin = pin.dropRight(1)
          validatePin()
      }
    }

    List(views.pin_9, views.pin_8, views.pin_7,
      views.pin_6, views.pin_5, views.pin_4,
      views.pin_3, views.pin_2, views.pin_1,
      views.pin_0, views.pin_ok, views.pin_back).foreach(_.onClick(onClick))
  }
}
