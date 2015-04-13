package com.hanhuy.android.keepshare

import android.support.v7.app.ActionBarActivity
import android.view.ViewGroup
import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.android.common._
import com.hanhuy.android.common.RichLogger._
import TypedResource._

import android.inputmethodservice.{Keyboard, InputMethodService}
import android.inputmethodservice.KeyboardView.OnKeyboardActionListener
import android.view.inputmethod.{EditorInfo, ExtractedTextRequest, InputMethodManager}
import android.text.InputType
import android.widget.Toast
import android.app.{Activity, AlertDialog}
import android.os.Bundle
import android.content.Intent

object PasswordIME {
  val NAME = "com.hanhuy.android.keepshare/.PasswordIME"

  val EXTRA_PACKAGE = "com.hanhuy.android.keepshare.extra.PACKAGE"
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
  private var packagePrompted = Option.empty[String]

  private val LONG_PRESS = 1500

  ServiceBus += {
    case ServiceExit => quitIME()
    case ShareActivityCancel => quitIME()
  }

  override def onCreateInputView() = {
    inputView.getParent match {
      case v: ViewGroup =>
        v.removeView(inputView)
      case _ =>
    }
    inputView.setOnKeyboardActionListener(this)
    inputView.setKeyboard(input)
    inputView
  }

  private def setText(text: String) {
    val ic = getCurrentInputConnection
    val etr = new ExtractedTextRequest
    ic.beginBatchEdit
    ic.setSelection(0, 0)
    Option(ic.getExtractedText(etr, 0)) foreach { et =>
      ic.deleteSurroundingText(0, et.text.length)
    }
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
        case _ => false
      }
    } else {
      v("Prompting for password search")
      if (!packagePrompted.exists(_==info.packageName)) {
        packagePrompted = Option(info.packageName)

        UiBus += {
          case IMESearchCancel =>
            v("Received cancel, quitting IME")
            quitIME()
            EventBus.Remove
          case IMESearchOk =>
            val intent = new Intent(this, classOf[ShareActivity])
            val appHost = info.packageName.split("""\.""").reverse.mkString(".")
            intent.setAction(Intent.ACTION_SEND)
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK |
              Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS)
            intent.setType("text/plain")
            intent.putExtra(Intent.EXTRA_SUBJECT, "Application search")
            intent.putExtra(Intent.EXTRA_TEXT, "android-package://" + appHost)
            startActivity(intent)
            EventBus.Remove
        }
        val intent = new Intent(this, classOf[IMESearchActivity])
        intent.addFlags(
          Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_MULTIPLE_TASK)
        intent.putExtra(PasswordIME.EXTRA_PACKAGE, info.packageName)
        startActivity(intent)
      }
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
        switchInputMethod(oldIME)
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
        R.string.override_password_input, Toast.LENGTH_SHORT).show()
  }

  def onText(p1: CharSequence) {}
  def swipeDown() {}
  def swipeLeft() {}
  def swipeRight() {}
  def swipeUp() {}
  def onKey(p1: Int, p2: Array[Int]) {}
}

class IMESearchActivity extends Activity with TypedViewHolder {
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.ime_search_activity)
    val packageName = getIntent.getStringExtra(PasswordIME.EXTRA_PACKAGE)
    setTitle(getTitle + getString(R.string.ime_search_title))
    findView(TR.text).setText(
      getString(R.string.ime_search_prompt, packageName))
    findView(TR.confirm) onClick {
      UiBus.send(IMESearchOk)
      finish()
    }
    findView(TR.cancel) onClick { finish() }
  }

  override def onStop() {
    super.onStop()
    UiBus.send(IMESearchCancel)
  }
}
