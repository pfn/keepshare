package com.hanhuy.android.keepshare

import android.support.v7.app.ActionBarActivity
import com.hanhuy.android.common._
import AndroidConversions._

import collection.JavaConversions._

import android.app.{AlertDialog, ProgressDialog, Activity}
import android.os.Bundle
import android.content.{ComponentName, ActivityNotFoundException, Intent}
import android.view.{View, MenuItem, Menu}
import android.widget.{CheckBox, CompoundButton, Toast}
import java.io.{FileOutputStream, File}
import android.text.{Editable, TextWatcher}
import android.view.inputmethod.InputMethodManager
import android.provider.OpenableColumns

import scala.util.Try

object RequestCodes {

  val REQUEST_ACCOUNT_PICKER = 1
  val REQUEST_AUTHORIZATION  = 2
  val BROWSE_DATABASE        = 3
  val BROWSE_KEYFILE         = 4
  val REQUEST_SETUP          = 5
  val REQUEST_PIN            = 6
  val REQUEST_SIGN_IN        = 7
  val REQUEST_SETUP_PIN      = 8

  val EXTRA_FOR_RESULT = "com.hanhuy.android.keepshare.extra.FOR_RESULT"
}
object SetupActivity {
  lazy val intent = {
    val intent = Intent.makeMainActivity(
      new ComponentName("com.hanhuy.android.keepshare",
        "com.hanhuy.android.keepshare.SetupActivity"))
    intent.putExtra(RequestCodes.EXTRA_FOR_RESULT, true)
    intent
  }
}
class SetupActivity extends ActionBarActivity with TypedViewHolder {
  implicit val TAG = LogcatTag("SetupActivity")
  import KeyManager._
  import RequestCodes._
  val _implicit: RichActivity = this
  import _implicit._

  lazy val flipper = findView(TR.flipper)
  lazy val settings = Settings(this)
  lazy val keymanager = new KeyManager(this, settings)
  lazy val keyboardToggle = findViewById(
    R.id.toggle_keyboard).asInstanceOf[CheckBox]

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.setup, menu)
    getMenuInflater.inflate(R.menu.main, menu)
    if (settings.get(Settings.NEEDS_PIN)) {
      menu.findItem(R.id.menu_setup_pin).setVisible(false)
    } else {
      menu.findItem(R.id.menu_change_pin).setVisible(false)
    }
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
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
      case R.id.menu_setup_pin =>
        if (keymanager.ready) {
          startActivityForResult(new Intent(this, classOf[PINSetupActivity]),
            RequestCodes.REQUEST_SETUP_PIN)
        } else {
          Toast.makeText(this,
            R.string.setup_required_for_pin, Toast.LENGTH_SHORT).show()
        }
        true
      case R.id.menu_change_pin =>
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
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
    val imm = this.systemService[InputMethodManager]
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
      val imm = this.systemService[InputMethodManager]
      val list = imm.getEnabledInputMethodList
      val enabled = list exists (
        _.getPackageName == "com.hanhuy.android.keepshare")

      if (b != enabled) {
        startActivity(Intent.makeMainActivity(
          new ComponentName("com.android.settings",
            "com.android.settings.LanguageSettings")))
      }
    }

    val connect = findView(TR.connect)
    if (settings.get(Settings.FIRST_RUN)) {
      settings.clear()
      KeyManager.clear()
      flipper.setDisplayedChild(1)
    }
    def onNext(): Unit = {
      findView(TR.progress).setVisibility(View.VISIBLE)
      async {
        val k = keymanager.loadKey()
        if (k.nonEmpty) UiBus.post {
          settings.set(Settings.FIRST_RUN, false)
          flipper.setDisplayedChild(0)
          findView(TR.save).setEnabled(true)
          findView(TR.progress2).setVisibility(View.GONE)
        }
      }
    }
    connect onClick {
      connect.setEnabled(false)
      onNext()
      // go to next step!
    }

    val browseHandler = { v: View =>
      val intent = new Intent(if (kitkatAndNewer)
        Intent.ACTION_OPEN_DOCUMENT else Intent.ACTION_GET_CONTENT)
      intent.addCategory(Intent.CATEGORY_OPENABLE)
      intent.setType(if (kitkatAndNewer) "application/*" else "file/*")

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
    findView(TR.timeout).check(settings.get(Settings.KEYBOARD_TIMEOUT) match {
      case 45 => R.id.timeout_45
      case 30 => R.id.timeout_30
      case 15 => R.id.timeout_15
      case _  => R.id.timeout_60
    })
    findView(TR.service_timeout).check(settings.get(Settings.PIN_TIMEOUT) match {
      case 60 => R.id.svctimeout_60
      case 15 => R.id.svctimeout_15
      case 5  => R.id.svctimeout_5
      case _  => R.id.svctimeout_1
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
              Try(Database.open(
                db.getAbsolutePath,
                Option(password.trim),
                if (keyf.isFile) Some(keyf.getAbsolutePath) else None)) map { r =>
                keymanager.loadKey()
                keymanager.localKey match {
                  case Left(error) =>
                    UiBus.post {
                      Toast.makeText(
                        this, error.toString, Toast.LENGTH_SHORT).show()
                      finish()
                    }
                  case Right(k) =>
                    val encdb = KeyManager.encrypt(k, db.getAbsolutePath)
                    val encpw = KeyManager.encrypt(k, password.trim)
                    val enckeyf = KeyManager.encrypt(k, keyfilepath)
                    val verifier = KeyManager.encrypt(k, KeyManager.VERIFIER)

                    settings.set(Settings.VERIFY_DATA, verifier)
                    settings.set(Settings.PASSWORD, encpw)
                    settings.set(Settings.KEYFILE_PATH, enckeyf)
                    settings.set(Settings.DATABASE_FILE, encdb)
                    settings.set(Settings.KEYBOARD_TIMEOUT,
                      findView(TR.timeout).getCheckedRadioButtonId match {
                        case R.id.timeout_60 => 60
                        case R.id.timeout_45 => 45
                        case R.id.timeout_30 => 30
                        case R.id.timeout_15 => 15
                      })
                    settings.set(Settings.PIN_TIMEOUT,
                      findView(TR.service_timeout).getCheckedRadioButtonId match {
                        case R.id.svctimeout_60 => 60
                        case R.id.svctimeout_15 => 15
                        case R.id.svctimeout_5  => 5
                        case R.id.svctimeout_1  => 1
                      })
                    settings.set(Settings.PASSWORD_OVERRIDE,
                      findView(TR.password_override).isChecked)
                    UiBus.post {
                      success(R.string.settings_saved)
                      setResult(Activity.RESULT_OK)
                      if (getIntent.hasExtra(EXTRA_FOR_RESULT))
                        finish()
                    }
                }
              } recover { case e: Exception =>
                UiBus.post {  error(e.getMessage) }
                RichLogger.e("failed to load database", e)
              }
              UiBus.post { findView(TR.progress2).setVisibility(View.GONE) }
            }
          }
        }
      } else {
        error(R.string.database_no_blank)
      }
    }

    if (!settings.get(Settings.FIRST_RUN)) async {
      val ckey = keymanager.loadKey()
      (Option(settings.get(Settings.DATABASE_FILE))
        , Option(settings.get(Settings.PASSWORD))
        , Option(settings.get(Settings.KEYFILE_PATH))
        , Option(settings.get(Settings.VERIFY_DATA))) match {
        case (Some(encdb), Some(encpw), Some(enckeyf), Some(encverifier)) =>
          keymanager.getConfig match {
            case Left(error) =>
              UiBus.post {
                Toast.makeText(this,
                  R.string.decrypt_saved_failed, Toast.LENGTH_SHORT).show()
                finish()
              }
            case Right((db,pw,keyf)) =>
              UiBus.post {
                findView(TR.file_name).setText(db)
                findView(TR.password).setText(pw)
                findView(TR.key_file_name).setText(keyf)
                success(R.string.ready_to_use)
                findView(TR.save).setEnabled(false)
              }
          }
        case _ =>
      }
      if (ckey != null) UiBus.post {
        findView(TR.progress2).setVisibility(View.GONE)
      }
    }
  }


  override def onActivityResult(request: Int, result: Int, data: Intent) {
    request match {
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
        } else finish()
      case REQUEST_SIGN_IN =>
        if (result != Activity.RESULT_OK)
          finish()
        else {
          keymanager.makeApiClient()
        }
      case BROWSE_DATABASE =>
        if (result == Activity.RESULT_OK) {
          val uri = data.getData
          if (uri.getScheme == "content") {
            val progress = ProgressDialog.show(this,
              "Downloading", "Please Wait", false, true)
            var canceled = false
            progress.onCancel {
              canceled = true
            }
            async {
              val c = getContentResolver.query(uri, null, null, null, null)
              var name = uri.getLastPathSegment
              var size = 100
              val cName = c.getColumnIndex(OpenableColumns.DISPLAY_NAME)
              val cSize = c.getColumnIndex(OpenableColumns.SIZE)
              if (cName != -1) {
                Stream.continually(c.moveToNext) takeWhile identity foreach { _ =>
                  if (!c.isNull(cName)) {
                    name = c.getString(cName)
                    size = c.getInt(cSize) // oh well, overflow don't care
                  }
                }
              }
              c.close()
              UiBus.post {
                progress.setMax(size)
              }
              val external = getExternalFilesDir(null)
              val input = getContentResolver.openInputStream(uri)
              val dest = new java.io.File(external, name)
              val out = new FileOutputStream(dest)

              UiBus.post { findView(TR.file_name).setText(dest.getAbsolutePath) }
              try {
                var total = 0
                val buf = Array.ofDim[Byte](32768)
                Stream.continually(input.read(buf, 0, 32768)) takeWhile (
                  _ != -1 && !canceled) foreach { read =>
                  total += read
                  out.write(buf, 0, read)
                  UiBus.post { progress.setProgress(total) }
                }
                if (canceled)
                  dest.delete()
              } finally {
                input.close()
                out.close()
              }
              UiBus.post { progress.dismiss() }
            }
          } else {
            findView(TR.file_name).setText(uri.getPath)
          }
        }
      case BROWSE_KEYFILE =>
        if (result == Activity.RESULT_OK)
          findView(TR.key_file_name).setText(data.getData.getPath)
      case REQUEST_PIN => // for change pin
        if (result == Activity.RESULT_OK)
          startActivityForResult(new Intent(this, classOf[PINSetupActivity]), RequestCodes.REQUEST_SETUP_PIN)
      case REQUEST_SETUP_PIN => finish()
    }
  }
}
