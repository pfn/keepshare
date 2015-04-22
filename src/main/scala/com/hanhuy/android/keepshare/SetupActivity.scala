package com.hanhuy.android.keepshare

import android.preference.{ListPreference, CheckBoxPreference, Preference}
import android.preference.Preference.{OnPreferenceChangeListener, OnPreferenceClickListener}
import android.support.v7.app.ActionBarActivity
import android.text.method.PasswordTransformationMethod
import android.util.AttributeSet
import android.view.View.OnFocusChangeListener
import com.hanhuy.android.common._
import AndroidConversions._

import collection.JavaConversions._

import android.app.{AlertDialog, ProgressDialog, Activity}
import android.os.Bundle
import android.content.{Context, ComponentName, ActivityNotFoundException, Intent}
import android.view.{ViewGroup, View, MenuItem, Menu}
import android.widget._
import java.io.{FileOutputStream, File}
import android.text.{InputType, Editable, TextWatcher}
import android.view.inputmethod.InputMethodManager
import android.provider.{DocumentsContract, OpenableColumns}

import scala.concurrent.Future
import scala.util.Try
import Futures._

object RequestCodes {

  val REQUEST_ACCOUNT_PICKER = 1
  val REQUEST_AUTHORIZATION  = 2
  val BROWSE_DATABASE        = 3
  val BROWSE_KEYFILE         = 4
  val REQUEST_SETUP          = 5
  val REQUEST_PIN            = 6
  val REQUEST_SIGN_IN        = 7
  val REQUEST_SETUP_PIN      = 8
  val REQUEST_PIN_ENTRY      = 9

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
class SetupActivity extends ActionBarActivity with TypedViewHolder with EventBus.RefOwner {
  implicit val TAG = LogcatTag("SetupActivity")
  import KeyManager._
  import RequestCodes._

  lazy val fragment = getFragmentManager.findFragmentById(
    R.id.setup_fragment).asInstanceOf[SetupFragment]
  lazy val flipper = findView(TR.flipper)
  lazy val settings = Settings(this)
  lazy val keymanager = new KeyManager(this, settings)

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.setup, menu)
    getMenuInflater.inflate(R.menu.main, menu)
    if (settings.get(Settings.NEEDS_PIN)) {
      menu.findItem(R.id.menu_setup_pin).setVisible(false)
    } else {
      menu.findItem(R.id.menu_change_pin).setVisible(false)
    }
    if (settings.get(Settings.FIRST_RUN)) {
      menu.findItem(R.id.menu_change_pin).setVisible(false)
      menu.findItem(R.id.menu_setup_pin).setVisible(false)
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

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.setup_subtitle))
    setContentView(R.layout.setup)


    val connect = findView(TR.connect)
    if (settings.get(Settings.FIRST_RUN)) {
      settings.clear()
      KeyManager.clear()
      flipper.setDisplayedChild(1)
    }
    def onNext(): Unit = {
      findView(TR.progress).setVisibility(View.VISIBLE)
      keymanager.fetchCloudKey() onSuccessMain { case _ =>
        settings.set(Settings.FIRST_RUN, false)
        supportInvalidateOptionsMenu()
        flipper.setDisplayedChild(0)
        findView(TR.save).setEnabled(true)
        findView(TR.progress2).setVisibility(View.GONE)
      }
    }
    connect onClick {
      connect.setEnabled(false)
      onNext()
      // go to next step!
    }

    def browseHandler(id: Int) = { () =>
      val intent = new Intent(if (kitkatAndNewer)
        Intent.ACTION_OPEN_DOCUMENT else Intent.ACTION_GET_CONTENT)
      intent.addCategory(Intent.CATEGORY_OPENABLE)
      intent.setType(if (kitkatAndNewer) "application/*" else "file/*")

      try {
        startActivityForResult(intent, id match {
          case 1 => BROWSE_DATABASE
          case 2 => BROWSE_KEYFILE
        })
      } catch {
        case e: ActivityNotFoundException =>
          Toast.makeText(this,
            R.string.must_install_explorer,
            Toast.LENGTH_SHORT).show()
      }
    }

    fragment datafileOnClick browseHandler(1)
    fragment keyfileOnClick browseHandler(2)

    val watcher = (p: Preference, a: Any) => {
      findView(TR.save).setEnabled(true)
      findView(TR.error_text).setVisibility(View.GONE)
      findView(TR.success_text).setVisibility(View.GONE)
      true
    }
    import SetupFragment._
    fragment.onPreferenceChange(watcher)
    findView(TR.save) onClick { view: View =>
      findView(TR.error_text).setVisibility(View.GONE)
      findView(TR.success_text).setVisibility(View.GONE)
      view.setEnabled(false)
      val database = fragment.datafile

      val keyfile = fragment.keyfile
      if (database.trim != "") {

        val keyf = new File(keyfile.trim).getAbsoluteFile
        if (keyfile.trim != "") {
          if (!keyf.exists) {
            error(R.string.keyfile_no_exist)
          }
        }
        val password = fragment.password
        if (password.trim == "") {
          error(R.string.password_no_blank)
        } else {

          val db = new File(database.trim).getAbsoluteFile
          if (!db.isFile) {
            error(R.string.database_no_exist)
          } else {
            findView(TR.progress2).setVisibility(View.VISIBLE)
            val keyfilepath = if (keyf.isFile) keyf.getAbsolutePath else ""
            val f = Future {
              Database.open(
                db.getAbsolutePath,
                Option(password.trim),
                if (keyf.isFile) Some(keyf.getAbsolutePath) else None)
            } flatMap { _ => keymanager.localKey }

            f onSuccessMain {
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
                  settings.set(Settings.KEYBOARD_TIMEOUT, fragment.kbtimeo)
                  settings.set(Settings.PIN_TIMEOUT, fragment.pintimeo)
                  settings.set(Settings.PASSWORD_OVERRIDE, fragment.kboverride)
                  UiBus.post {
                    success(R.string.settings_saved)
                    setResult(Activity.RESULT_OK)
                    if (getIntent.hasExtra(EXTRA_FOR_RESULT))
                      finish()
                  }
            }

            f onFailureMain { case e =>
              UiBus.post { error("Unable to open database") }
              RichLogger.e("failed to load database", e)
            }

            f onCompleteMain { _ =>
              findView(TR.progress2).setVisibility(View.GONE)
            }
          }
        }
      } else {
        error(R.string.database_no_blank)
      }
    }

    if (!settings.get(Settings.FIRST_RUN)) {
      (Option(settings.get(Settings.DATABASE_FILE))
        , Option(settings.get(Settings.PASSWORD))
        , Option(settings.get(Settings.KEYFILE_PATH))
        , Option(settings.get(Settings.VERIFY_DATA))) match {
        case (Some(encdb), Some(encpw), Some(enckeyf), Some(encverifier)) =>
          keymanager.config onSuccessMain {
            case Left(error) =>
              UiBus.post {
                Toast.makeText(this,
                  R.string.decrypt_saved_failed, Toast.LENGTH_SHORT).show()
                finish()
              }
            case Right((db,pw,keyf)) =>
              UiBus.post {
                fragment.datafile = db
                fragment.password = pw
                fragment.keyfile = keyf
                success(R.string.ready_to_use)
                findView(TR.save).setEnabled(false)
              }
          }
        case _ =>
      }
      keymanager.fetchCloudKey() onSuccessMain { case _ =>
        findView(TR.progress2).setVisibility(View.GONE)
      }
    }
    if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty) {
      startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
        RequestCodes.REQUEST_PIN_ENTRY)
    }
  }


  override def onActivityResult(request: Int, result: Int, data: Intent) {
    request match {
      case REQUEST_AUTHORIZATION =>
        if (result == Activity.RESULT_OK) {
          flipper.setDisplayedChild(0)
          data.getStringExtra(EXTRA_STATE) match {
            case STATE_LOAD => KeyManager.clear()
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
        if (result == Activity.RESULT_OK)
          setDataPath(data, fragment.datafile_=)
      case BROWSE_KEYFILE =>
        if (result == Activity.RESULT_OK)
          setDataPath(data, fragment.keyfile_=)
      case REQUEST_PIN => // for change pin
        if (result == Activity.RESULT_OK)
          startActivityForResult(new Intent(this, classOf[PINSetupActivity]), RequestCodes.REQUEST_SETUP_PIN)
      case REQUEST_PIN_ENTRY =>
        if (result != Activity.RESULT_OK)
          finish()
      case REQUEST_SETUP_PIN => finish()
    }
  }

  def setDataPath(data: Intent, setProperty: String => Unit): Unit = {
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
        var lastModified = Option.empty[Long]
        val cName = c.getColumnIndex(OpenableColumns.DISPLAY_NAME)
        val cSize = c.getColumnIndex(OpenableColumns.SIZE)
        if (cName != -1) {
          Stream.continually(c.moveToNext) takeWhile identity foreach { _ =>
            if (!c.isNull(cName)) {
              name = c.getString(cName)
              size = c.getInt(cSize) // oh well, overflow don't care
              if (kitkatAndNewer) {
                val cLastModified = c.getColumnIndex(
                  DocumentsContract.Document.COLUMN_LAST_MODIFIED)
                if (cLastModified != -1 && !c.isNull(cLastModified))
                  lastModified = Some(c.getLong(cLastModified))
              }
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

        UiBus.post { setProperty(dest.getAbsolutePath) }
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
      setProperty(uri.getPath)
    }
  }
  ServiceBus += {
    case PINServiceExit  => finish()
  }
}

object SetupFragment {
  implicit class OnPreferenceChange(val pref: Preference) extends AnyVal {
    def onPreferenceChange(f: (Preference, Any) => Boolean) = pref.setOnPreferenceChangeListener(new OnPreferenceChangeListener {
      override def onPreferenceChange(preference: Preference, newValue: Any) = f(preference, newValue)
    })
  }
  implicit class OnPreferenceClick(val pref: Preference) extends AnyVal {
    def onPreferenceClick(f: Preference => Boolean) = pref.setOnPreferenceClickListener(new OnPreferenceClickListener {
      override def onPreferenceClick(preference: Preference) = f(preference)
    })
  }
  implicit class OnFocusChangeView(val view: View) extends AnyVal {
    def onFocusChange[A](f: Boolean => A) = view.setOnFocusChangeListener(new OnFocusChangeListener {
      override def onFocusChange(v: View, hasFocus: Boolean) = f(hasFocus)
    })
    def onFocusChange[A](f: (View, Boolean) => A) = view.setOnFocusChangeListener(new OnFocusChangeListener {
      override def onFocusChange(v: View, hasFocus: Boolean) = f(v, hasFocus)
    })
  }

  implicit class OnTextChange(val text: TextView) extends AnyVal {
    def onTextChanged[A](f: CharSequence => A) = text.addTextChangedListener(new TextWatcher {
      override def beforeTextChanged(s: CharSequence, start: Int, count: Int, after: Int) = ()
      override def onTextChanged(s: CharSequence, start: Int, before: Int, count: Int) = f(s)
      override def afterTextChanged(s: Editable) = ()
    })
  }
}
class SetupFragment extends android.preference.PreferenceFragment {
  import SetupFragment._
  lazy val settings = Settings(getActivity)
  lazy val df = findPreference("database_file").asInstanceOf[BrowsableTextPreference]
  lazy val kf = findPreference("database_key").asInstanceOf[BrowsableTextPreference]
  lazy val p = findPreference("database_pass").asInstanceOf[TextPreference]
  lazy val kt = findPreference("keyboard_enable").asInstanceOf[CheckBoxPreference]
  lazy val ae = findPreference("accessibility_enable").asInstanceOf[CheckBoxPreference]
  lazy val ktimeout = findPreference("keyboard_timeout").asInstanceOf[ListPreference]
  lazy val ptimeout = findPreference("pin_timeout").asInstanceOf[ListPreference]
  lazy val pwoverride = findPreference("keyboard_override").asInstanceOf[CheckBoxPreference]
  private var _onPrefChange = Option.empty[(Preference, Any) => Any]

  def onPrefChange(p: Preference, value: Any) = {
    _onPrefChange foreach(_(p,value))
    true
  }
  def prefChanged(p: Preference, value: Any) = _onPrefChange foreach (_(p, value))

  def onPreferenceChange[A](fn: (Preference, Any) => A) = _onPrefChange = Option(fn)
  def kboverride = pwoverride.isChecked
  def kboverride_=(b: Boolean) = pwoverride.setChecked(b)
  def password = p.text
  def password_=(s: String) = p.text = s
  def keyfile = kf.text
  def keyfile_=(s: String) = kf.text = s
  def datafile = df.text
  def datafile_=(s: String) = df.text = s
  def pintimeo = ptimeout.getValue.toInt
  def pintimeo_=(i: Int) = {
    ptimeout.setSummary(i.toString)
    ptimeout.setValue(i.toString)
  }
  def kbtimeo = ktimeout.getValue.toInt
  def kbtimeo_=(i: Int) = {
    ktimeout.setSummary(i.toString)
    ktimeout.setValue(i.toString)
  }

  def datafileOnClick[A](f: () => A) = df.onClick(f)
  def keyfileOnClick[A](f: () => A) = kf.onClick(f)

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    addPreferencesFromResource(R.xml.setup)
    p.password = true
  }

  override def onStart() = {
    super.onStart()
    ae.setIntent(new Intent(android.provider.Settings.ACTION_ACCESSIBILITY_SETTINGS))
    df.onPreferenceChange(onPrefChange)
    kf.onPreferenceChange(onPrefChange)
    p.onPreferenceChange(onPrefChange)
    kt.setIntent(Intent.makeMainActivity(
      new ComponentName("com.android.settings",
        "com.android.settings.LanguageSettings")))
    import android.provider.Settings.Secure
    val services = Option(Secure.getString(getActivity.getContentResolver, Secure.ENABLED_ACCESSIBILITY_SERVICES))
    ae.setChecked(services exists (_ contains "com.hanhuy.android.keepshare"))

    val imm = getActivity.systemService[InputMethodManager]
    val list = imm.getEnabledInputMethodList
    val enabled = list exists (
      _.getPackageName == "com.hanhuy.android.keepshare")
    kt.setChecked(enabled)
    pwoverride.setChecked(
      settings.get(Settings.PASSWORD_OVERRIDE))

    pwoverride.onPreferenceChange { (pref, b) =>
      prefChanged(pref, b)
      p.text = ""
      true
    }

    ktimeout.setValue(settings.get(Settings.KEYBOARD_TIMEOUT).toString)
    ktimeout.setSummary(settings.get(Settings.KEYBOARD_TIMEOUT).toString)
    ptimeout.setValue(settings.get(Settings.PIN_TIMEOUT).toString)
    ptimeout.setSummary(settings.get(Settings.PIN_TIMEOUT).toString)

    ptimeout.onPreferenceChange { (pref, v) =>
      prefChanged(pref, v)
      ptimeout.setSummary(v.toString)
      p.text = ""
      true
    }
    ktimeout.onPreferenceChange { (pref, v) =>
      prefChanged(pref, v)
      ktimeout.setSummary(v.toString)
      p.text = ""
      true
    }
  }

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    super.onViewCreated(view, savedInstanceState)
    // TextPreference has a divider between them??
    val list = view.findViewById(android.R.id.list).asInstanceOf[ListView]
    list.setDividerHeight(0)
  }
}

class TextPreference(ctx: Context, attrs: AttributeSet, res: Int)
  extends android.preference.Preference(ctx, attrs, res) {
  import SetupFragment._
  import com.rengwuxian.materialedittext.MaterialEditText
  def this(ctx: Context, attrs: AttributeSet) = this(ctx, attrs, 0)
  def this(ctx: Context) = this(ctx, null, 0)
  private var view = Option.empty[EditText]
  private var _text: String = ""
  private var ispassword = false
  private var wasfocused = false
  def password_=(b: Boolean) = ispassword = b
  def password = ispassword
  def text = view map (_.getText.toString) getOrElse _text
  def text_=(s: String) = {
    _text = s
    view foreach { e =>
      e.setText(s)
      e.setSelection(s.length)
    }
  }

  setLayoutResource(R.layout.edittext)
  setSelectable(true)

  override def onCreateView(parent: ViewGroup) = {
    super.onCreateView(parent)
  }

  override def getView(convertView: View, parent: ViewGroup) = {
    super.getView(null, parent)
  }

  override def onBindView(view: View) = {
    super.onBindView(view)
    val edit = view.asInstanceOf[ViewGroup]
      .getChildAt(0).asInstanceOf[MaterialEditText]
    this.view = Option(edit)
    edit.setInputType(InputType.TYPE_TEXT_FLAG_NO_SUGGESTIONS |
      InputType.TYPE_TEXT_VARIATION_URI)
    edit.setTransformationMethod(if (ispassword)
      PasswordTransformationMethod.getInstance else null)
    edit.setFloatingLabelText(getTitle)
    if (wasfocused) {
      edit.requestFocus()
    }
    edit.setHint(getTitle)
    edit.setText(_text)
    edit.setSelection(_text.length)
    edit.onTextChanged { s =>
      if (callChangeListener(s))
        _text = edit.getText
    }
    edit.onFocusChange { f =>
      if (f) wasfocused = true
      edit.setSelection(edit.getText.length)
    }
    wasfocused = false
  }
}
class BrowsableTextPreference(ctx: Context, attrs: AttributeSet, res: Int)
  extends TextPreference(ctx, attrs, res) {
  def this(ctx: Context, attrs: AttributeSet) = this(ctx, attrs, 0)
  def this(ctx: Context) = this(ctx, null, 0)
  setLayoutResource(R.layout.browsable_edittext)
  private var _onClick = Option.empty[() => Any]

  def onClick[A](f: () => A) = _onClick = Option(f)

  override def onCreateView(parent: ViewGroup) = {
    val v = super.onCreateView(parent)
    v.asInstanceOf[ViewGroup].getChildAt(1).onClick({
      _onClick foreach (_())
    })
    v
  }
}
