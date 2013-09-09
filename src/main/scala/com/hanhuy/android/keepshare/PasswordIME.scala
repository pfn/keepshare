package com.hanhuy.android.keepshare

import AndroidConversions._
import TypedResource._

import android.inputmethodservice.{Keyboard, InputMethodService}
import android.inputmethodservice.KeyboardView.OnKeyboardActionListener
import android.view.inputmethod.{EditorInfo, ExtractedTextRequest, InputMethodManager}
import android.text.InputType
import android.widget.Toast
import android.view.KeyEvent

object PasswordIME {
  val NAME = "com.hanhuy.android.keepshare/.PasswordIME"
}
class PasswordIME extends InputMethodService with OnKeyboardActionListener
with EventBus.RefOwner {
  implicit val TAG = LogcatTag("PasswordIME")
  val _implicit: RichContext = this
  import _implicit._

  lazy val settings = Settings(this)
  lazy val inputView = getLayoutInflater.inflate(TR.layout.password_ime, null)
  lazy val input = new Keyboard(this, R.xml.keys)
  private var passwordPress = 0l
  private var isPassword = false

  private val LONG_PRESS = 1500

  ServiceBus += {
    case ServiceExit => quitIME()
  }

  override def onCreateInputView() = {
    inputView.setOnKeyboardActionListener(this)
    inputView.setKeyboard(input)
    inputView
  }

  private def setText(text: String) {
    val ic = getCurrentInputConnection
    ic.beginBatchEdit
    val etr = new ExtractedTextRequest
    val et = ic.getExtractedText(etr, 0)
    ic.setSelection(0, 0)
    ic.deleteSurroundingText(0, et.text.length)

    ic.commitText(text, text.length)

    ic.endBatchEdit
  }

  override def onStartInputView(info: EditorInfo, restarting: Boolean) {
    import InputType._
    if (CredentialHolderService.instance.isDefined) {
      val itype = info.inputType
      isPassword = itype & TYPE_MASK_CLASS match {
        case TYPE_CLASS_NUMBER => itype & TYPE_MASK_VARIATION match {
          case TYPE_NUMBER_VARIATION_PASSWORD => true
          case _ => false
        }
        case TYPE_CLASS_TEXT => itype & TYPE_MASK_VARIATION match {
            case TYPE_TEXT_VARIATION_PASSWORD
               | TYPE_TEXT_VARIATION_WEB_PASSWORD
               | TYPE_TEXT_VARIATION_VISIBLE_PASSWORD => true
            case _ => false
          }
      }
    } else {
      Toast.makeText(this,
        R.string.no_passwords_selected, Toast.LENGTH_SHORT).show()
      quitIME()
    }
  }

  def onRelease(key: Int) {
    key match {
      case 195 =>
        CredentialHolderService.instance map { service =>
          if (service.username != null)
            setText(service.username)
        } getOrElse quitIME()
      case 196 =>
        CredentialHolderService.instance map { service =>
          if (isPassword) {
            if (service.password != null)
              setText(service.password)
          } else {

            val longPress =
              System.currentTimeMillis - passwordPress > LONG_PRESS
            if (!settings.get(Settings.PASSWORD_OVERRIDE) || !longPress) {
              Toast.makeText(this, if (settings.get(Settings.PASSWORD_OVERRIDE))
                R.string.no_password_input_override else
                R.string.no_password_input, Toast.LENGTH_SHORT).show()
            } else if (service.password != null)
              setText(service.password)
          }
        } getOrElse quitIME()
        passwordPress = Integer.MAX_VALUE
      case 10 => sendDefaultEditorAction(true)
      case 14 => setText("")
      case 1 => quitIME()
    }
  }

  private def quitIME() {
    val token = getWindow.getWindow.getAttributes.token
    if (honeycombAndNewer) {
      systemService[InputMethodManager].switchToLastInputMethod(token)
    } else {
      val oldIME = settings.get(Settings.IME)
      if (oldIME != null)
        systemService[InputMethodManager].setInputMethod(token, oldIME)
    }
    ServiceBus.send(KeyboardExit)
  }
  def onPress(key: Int) {
    if (key == 196) {
      passwordPress = System.currentTimeMillis
      UiBus.handler.removeCallbacks(overrideRunner)
      UiBus.handler.postDelayed(overrideRunner, LONG_PRESS)
    }
  }

  val overrideRunner: Runnable = () => {
    if (passwordPress != Integer.MAX_VALUE)
      Toast.makeText(this,
        "Overriding password input", Toast.LENGTH_SHORT).show()
  }

  def onText(p1: CharSequence) {}
  def swipeDown() {}
  def swipeLeft() {}
  def swipeRight() {}
  def swipeUp() {}
  def onKey(p1: Int, p2: Array[Int]) {}
}
