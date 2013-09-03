package com.hanhuy.android.keepshare

import AndroidConversions._

import collection.JavaConversions._

import android.app.Activity
import android.os.Bundle
import android.content.{ComponentName, ActivityNotFoundException, Intent}
import android.accounts.AccountManager
import android.view.{View, MenuItem, Menu}
import android.widget.{CompoundButton, Toast}
import java.io.File
import android.text.{Editable, TextWatcher}
import com.keepassdroid.provider.Contract
import android.view.inputmethod.InputMethodManager

object RequestCodes {

  val REQUEST_ACCOUNT_PICKER = 1
  val REQUEST_AUTHORIZATION  = 2

  val BROWSE_DATABASE = 3
  val BROWSE_KEYFILE  = 4
  val REQUEST_SETUP   = 5

}
class SetupActivity extends Activity with TypedViewHolder {
  implicit val TAG = LogcatTag("MainActivity")
  import KeyManager._
  import RequestCodes._
  val _implicit: RichActivity = this
  import _implicit._

  lazy val flipper = findView(TR.flipper)
  lazy val settings = Settings(this)
  lazy val keymanager = new KeyManager(this, settings)
  lazy val keyboardToggle = findViewById(
    R.id.toggle_keyboard).asInstanceOf[CompoundButton]

  private def pickAccount() {
    settings.set(Settings.GOOGLE_USER, null)
    startActivityForResult(
      keymanager.newChooseAccountIntent, REQUEST_ACCOUNT_PICKER)
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.main, menu)
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
      case R.id.clear_user_data =>
        settings.clear()
        KeyManager.clear()
        finish()
        true
      case _ => super.onOptionsItemSelected(item)
    }
  }

  def error(error: String) {
    val view = findView(TR.error_text)
    findView(TR.success_text).setVisibility(View.GONE)
    view.setVisibility(View.VISIBLE)
    view.setText(error)
    findView(TR.save).setEnabled(false)
  }
  def error(err: Int): Unit = error(getString(err))
  def success(msg: String) {
    findView(TR.error_text).setVisibility(View.GONE)
    val view = findView(TR.success_text)
    view.setVisibility(View.VISIBLE)
    view.setText(msg)
  }
  def success(msg: Int): Unit = success(getString(msg))

  override def onResume() {
    super.onResume()
    val imm = systemService[InputMethodManager]
    val list = imm.getEnabledInputMethodList
    val enabled = list exists (
      _.getPackageName == "com.hanhuy.android.keepshare")
    keyboardToggle.setChecked(enabled)
    findView(TR.password_override).setChecked(
      settings.get(Settings.PASSWORD_OVERRIDE))

  }
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.setup_subtitle))
    setContentView(R.layout.setup)

    keyboardToggle onCheckedChanged { b =>
      val imm = systemService[InputMethodManager]
      val list = imm.getEnabledInputMethodList
      val enabled = list exists (
        _.getPackageName == "com.hanhuy.android.keepshare")

      if (b != enabled) {
        startActivity(Intent.makeMainActivity(
          new ComponentName("com.android.settings",
            "com.android.settings.LanguageSettings")))
      }
    }

    val user = Option(settings.get(Settings.GOOGLE_USER))
    val connect = findView(TR.connect)
    if (user.isEmpty)
      flipper.setDisplayedChild(1)
    connect onClick {
      connect.setEnabled(false)
      pickAccount()
    }

    val browseHandler = { v: View =>
      val intent = new Intent(Intent.ACTION_GET_CONTENT)
      intent.setType("file/*")

      // TODO support content provider URIs for google Drive, Dropbox, etc.
      try {
        startActivityForResult(intent, v.getId match {
          case R.id.browse_database => BROWSE_DATABASE
          case R.id.browse_keyfile => BROWSE_KEYFILE
        })
      } catch {
        case e: ActivityNotFoundException =>
          Toast.makeText(this,
            R.string.must_install_explorer,
            Toast.LENGTH_SHORT).show()
      }
    }

    findView(TR.password_override) onCheckedChanged { b =>
      findView(TR.password).setText("")
      findView(TR.save).setEnabled(true)
    }

    findView(TR.timeout) onCheckedChanged { i =>
      findView(TR.save).setEnabled(true)
    }
    findView(TR.timeout).check(settings.get(Settings.TIMEOUT) match {
      case 45 => R.id.timeout_45
      case 30 => R.id.timeout_30
      case 15 => R.id.timeout_15
      case _  => R.id.timeout_60
    })
    findView(TR.browse_database) onClick browseHandler
    findView(TR.browse_keyfile) onClick browseHandler


    val watcher = new TextWatcher {
      def beforeTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int) {}

      def onTextChanged(p1: CharSequence, p2: Int, p3: Int, p4: Int) {
        findView(TR.save).setEnabled(true)
        findView(TR.error_text).setVisibility(View.GONE)
        findView(TR.success_text).setVisibility(View.GONE)
      }

      def afterTextChanged(p1: Editable) {}
    }
    findView(TR.key_file_name).addTextChangedListener(watcher)
    findView(TR.file_name).addTextChangedListener(watcher)
    findView(TR.password).addTextChangedListener(watcher)
    findView(TR.save) onClick { view: View =>
      findView(TR.error_text).setVisibility(View.GONE)
      findView(TR.success_text).setVisibility(View.GONE)
      view.setEnabled(false)
      val database = findView(TR.file_name).getText.toString

      val keyfile = findView(TR.key_file_name).getText.toString
      if (database.trim != "") {

        val keyf = new File(keyfile.trim).getAbsoluteFile
        if (keyfile.trim != "") {
          if (!keyf.exists) {
            error(R.string.keyfile_no_exist)
          }
        }
        val password = findView(TR.password).getText.toString
        if (password.trim == "") {
          error(R.string.password_no_blank)
        } else {

          val db = new File(database.trim).getAbsoluteFile
          if (!db.isFile) {
            error(R.string.database_no_exist)
          } else {
            findView(TR.progress2).setVisibility(View.VISIBLE)
            async {
              val b = new Bundle
              val keyfilepath = if (keyf.isFile) keyf.getAbsolutePath else ""
              b.putString(Contract.EXTRA_DATABASE, db.getAbsolutePath)
              b.putString(Contract.EXTRA_PASSWORD, password.trim)
              b.putString(Contract.EXTRA_KEYFILE, keyfilepath)
              try {
                val result = getContentResolver.call(
                  Contract.URI, Contract.METHOD_OPEN, null, b)
                if (result != null && result.containsKey(Contract.EXTRA_ERROR)) {
                  UiBus.post {  error(result.getString(Contract.EXTRA_ERROR)) }
                } else if (result == null) {
                  UiBus.post { error(R.string.keepass_no_respond) }
                } else {
                  val k = keymanager.localKey
                  val encdb = KeyManager.encrypt(k, db.getAbsolutePath)
                  val encpw = KeyManager.encrypt(k, password.trim)
                  val enckeyf = KeyManager.encrypt(k, keyfilepath)
                  val verifier = KeyManager.encrypt(k, KeyManager.VERIFIER)

                  settings.set(Settings.VERIFY_DATA, verifier)
                  settings.set(Settings.PASSWORD, encpw)
                  settings.set(Settings.KEYFILE_PATH, enckeyf)
                  settings.set(Settings.DATABASE_FILE, encdb)
                  settings.set(Settings.TIMEOUT,
                    findView(TR.timeout).getCheckedRadioButtonId match {
                      case R.id.timeout_60 => 60
                      case R.id.timeout_45 => 45
                      case R.id.timeout_30 => 30
                      case R.id.timeout_15 => 15
                    })
                  settings.set(Settings.PASSWORD_OVERRIDE,
                    findView(TR.password_override).isChecked)
                  UiBus.post {
                    success(R.string.settings_saved)
                    setResult(Activity.RESULT_OK)
                  }
                }
              } catch {
                case e: IllegalArgumentException => UiBus.post {
                  error(R.string.keepassdroid_not_installed)
                  RichLogger.e("failed to communicate with keepassdroid", e)
                }
              }
              UiBus.post { findView(TR.progress2).setVisibility(View.GONE) }
            }
          }
        }
      } else {
        error(R.string.database_no_blank)
      }
    }

    user foreach { name =>
      keymanager.accountName = name
      async {
        val k = keymanager.loadKey()
        (Option(settings.get(Settings.DATABASE_FILE))
          , Option(settings.get(Settings.PASSWORD))
          , Option(settings.get(Settings.KEYFILE_PATH))
          , Option(settings.get(Settings.VERIFY_DATA))) match {
          case (Some(encdb), Some(encpw), Some(enckeyf), Some(encverifier)) =>
            val k = keymanager.localKey
            val verifier = KeyManager.decryptToString(k, encverifier)
            if (KeyManager.VERIFIER == verifier) {
              UiBus.post {
                findView(TR.file_name).setText(
                  KeyManager.decryptToString(k, encdb))
                findView(TR.password).setText(
                  KeyManager.decryptToString(k, encpw))
                findView(TR.key_file_name).setText(
                  KeyManager.decryptToString(k, enckeyf))
                success(R.string.ready_to_use)
                findView(TR.save).setEnabled(false)
              }
            } else {
              Toast.makeText(this,
                R.string.decrypt_saved_failed, Toast.LENGTH_SHORT).show()
            }
          case _ =>
        }
        if (k != null) UiBus.post {
          findView(TR.progress2).setVisibility(View.GONE)
        }
      }
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    request match {
      case REQUEST_ACCOUNT_PICKER =>
        if (result == Activity.RESULT_OK) {
          Option(data.getStringExtra(AccountManager.KEY_ACCOUNT_NAME)) foreach {
            n =>
            keymanager.accountName = n
            settings.set(Settings.GOOGLE_USER, n)
            findView(TR.progress).setVisibility(View.VISIBLE)
            async {
              val k = keymanager.loadKey()
              if (k != null) UiBus.post {
                flipper.setDisplayedChild(0)
                findView(TR.save).setEnabled(true)
                findView(TR.progress2).setVisibility(View.GONE)
              }
            }
          }
        } else finish()
      case REQUEST_AUTHORIZATION =>
        if (result == Activity.RESULT_OK) {
          flipper.setDisplayedChild(0)
          data.getStringExtra(EXTRA_STATE) match {
            case STATE_LOAD =>
            case STATE_SAVE => async {
              keymanager.createKey()
            }
            case _ =>
          }
        } else {
          pickAccount()
        }
      // TODO support content provider URIs for google Drive, Dropbox, etc.
      case BROWSE_DATABASE =>
        if (result == Activity.RESULT_OK)
          findView(TR.file_name).setText(data.getData.getPath)
      case BROWSE_KEYFILE =>
        if (result == Activity.RESULT_OK)
          findView(TR.key_file_name).setText(data.getData.getPath)
    }
  }
}
