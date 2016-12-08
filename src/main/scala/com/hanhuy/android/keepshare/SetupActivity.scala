package com.hanhuy.android.keepshare

import android.annotation.TargetApi
import android.content.res.ColorStateList
import android.net.Uri
import android.preference.{CheckBoxPreference, ListPreference, Preference, PreferenceGroup}
import android.support.design.widget.TextInputLayout
import android.support.v7.app.AppCompatActivity
import android.support.v7.widget.AppCompatEditText
import android.text.method.PasswordTransformationMethod
import android.util.AttributeSet
import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.android.common._
import com.hanhuy.keepassj._

import collection.JavaConversions._
import android.app.{Activity, AlertDialog, ProgressDialog}
import android.os.Bundle
import android.content._
import android.view._
import android.widget._
import java.io.{File, FileInputStream, FileOutputStream, IOException}

import android.text.InputType
import android.view.inputmethod.InputMethodManager
import android.provider.{DocumentsContract, OpenableColumns}
import Futures._
import ManagedResource._

import scala.concurrent.Future

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
  val SETUP_DATABASE         = 10
  val REQUEST_SETUP_ACTIVITY = 11 // mostly ignores except for a finish intent

  val RESULT_FINISH = -1337

  val EXTRA_FOR_RESULT = "com.hanhuy.android.keepshare.extra.FOR_RESULT"

  val EXTRA_DATABASE = "com.hanhuy.android.keepshare.extra.DATABASE"
  val EXTRA_KEYFILE  = "com.hanhuy.android.keepshare.extra.KEY_FILE"
  val EXTRA_PASSWORD = "com.hanhuy.android.keepshare.extra.PASSWORD"
}
object SetupActivity {
  lazy val intent = {
    val intent = Intent.makeMainActivity(
      new ComponentName(Application.instance, classOf[SetupActivity]))
    intent.putExtra(RequestCodes.EXTRA_FOR_RESULT, true)
    intent
  }
}
class SetupActivity extends AppCompatActivity with EventBus.RefOwner with PermissionManager with DialogManager {
  val log = Logcat("SetupActivity")
  import KeyManager._
  import RequestCodes._

  private[this] var dbCredentials = DatabaseSetupModel.empty
  lazy val fragment = getFragmentManager.findFragmentById(
    R.id.setup_fragment).asInstanceOf[SetupFragment]
  lazy val settings = Settings(this)
  lazy val keymanager = new KeyManager(this, settings)

  lazy val views: TypedViewHolder.setup = TypedViewHolder.setContentView(this, TR.layout.setup)

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.main, menu)
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
            KeyManager.resetHardwareKey()
            KeyManager.clear()
            setResult(RequestCodes.RESULT_FINISH)
            finish()
          })
          .setNegativeButton(android.R.string.no, null)
          .show()
        true
      case _ => super.onOptionsItemSelected(item)
    }
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setTitle(getTitle + getString(R.string.setup_subtitle))
    KeyManager.initHardwareKey()
    if (settings.get(Settings.FIRST_RUN)) {
      settings.clear()
      KeyManager.clear()
      if (KeyManager.hasHardwareKey) {
        views.hardware_keys.setVisibility(View.VISIBLE)
      }
      views.flipper.setDisplayedChild(1)
    }

    def onNext(): Unit = {
      if (views.hardware_keys.isChecked) {
        settings.set(Settings.FIRST_RUN, false)
        supportInvalidateOptionsMenu()
        views.flipper.setDisplayedChild(0)
        fragment.hk.setChecked(true)
        settings.set(Settings.HARDWARE_KEY_ENABLE, true)
      } else {
        val rp = requestPermission(android.Manifest.permission.GET_ACCOUNTS,
          R.string.require_get_accounts, views.flipper)
        rp onFailureMain { case _ => finish() }
        rp onSuccessMain { case _ =>

          settings.set(Settings.HARDWARE_KEY_ENABLE, false)
          views.progress.setVisibility(View.VISIBLE)
          fragment.hk.setChecked(false)
          val f = keymanager.init()

          f onSuccessMain { case _ =>
            settings.set(Settings.FIRST_RUN, false)
            supportInvalidateOptionsMenu()
            views.flipper.setDisplayedChild(0)
            views.progress2.setVisibility(View.GONE)
          }

          f onFailureMain { case e =>
            views.connect.setEnabled(true)
            Toast.makeText(this, "Failed to setup secret key: " + e.getMessage, Toast.LENGTH_LONG).show()
            Application.logException("onNext keymanager.init: " + e.getMessage, e)
          }
        }
      }
    }
    views.connect onClick0 {
      views.connect.setEnabled(false)
      onNext()
      // go to next step!
    }

    fragment databaseOnClick { () =>
      setupDatabase()
    }

    def setupDatabase(): Unit = {
      val intent = new Intent(this, classOf[DatabaseSetupActivity])
      intent.putExtra(EXTRA_DATABASE, dbCredentials.db.getOrElse(""))
      intent.putExtra(EXTRA_PASSWORD, dbCredentials.password.getOrElse(""))
      intent.putExtra(EXTRA_KEYFILE, dbCredentials.keyfile.getOrElse(""))
      startActivityForResult(intent, SETUP_DATABASE)
    }
    fragment pinSetupOnClick { () =>
      if (!settings.get(Settings.NEEDS_PIN)) {
        if (keymanager.ready) {
          startActivityForResult(new Intent(this, classOf[PINSetupActivity]),
            RequestCodes.REQUEST_SETUP_PIN)
        } else {
          Toast.makeText(this,
            R.string.setup_required_for_pin, Toast.LENGTH_SHORT).show()
        }
      } else {
        val intent = new Intent(this, classOf[PINEntryActivity])
        intent.putExtra(PINEntryActivity.EXTRA_NO_FINGERPRINT, true)
        startActivityForResult(intent, RequestCodes.REQUEST_PIN)
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
              Toast.makeText(this,
                R.string.decrypt_saved_failed, Toast.LENGTH_SHORT).show()
              finish()
            case Right((db,pw,keyf)) =>
              dbCredentials = DatabaseSetupModel(db, pw, keyf)
              fragment.databaseReady(true)
          }
        case _ =>
      }
      keymanager.init() onCompleteMain { case _ =>
        views.progress2.setVisibility(View.GONE)
      }
    }
    if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty) {
      startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
        RequestCodes.REQUEST_PIN_ENTRY)
    }
    if (Option(getIntent).exists(_.getBooleanExtra(EXTRA_FOR_RESULT, false)) && !settings.get(Settings.FIRST_RUN))
      setupDatabase()
  }


  override def onActivityResult(request: Int, result: Int, data: Intent) {
    request match {
      case REQUEST_AUTHORIZATION =>
        if (result == Activity.RESULT_OK) {
          views.flipper.setDisplayedChild(0)
          data.getStringExtra(EXTRA_STATE) match {
            case STATE_LOAD => KeyManager.clear()
            case STATE_SAVE => keymanager.init()
            case _ =>
          }
        } else finish()
      case REQUEST_SIGN_IN =>
        if (result != Activity.RESULT_OK)
          finish()
        else {
          keymanager.makeApiClient()
        }
      case REQUEST_PIN => // for change pin
        if (result == Activity.RESULT_OK)
          startActivityForResult(new Intent(this, classOf[PINSetupActivity]), RequestCodes.REQUEST_SETUP_PIN)
      case REQUEST_PIN_ENTRY =>
        if (result != Activity.RESULT_OK)
          finish()
      case REQUEST_SETUP_PIN => // do nothing
      case SETUP_DATABASE =>
        val intent = Option(data)
        def extra(k: String): String = intent.map(_.getStringExtra(k)).orNull
        if (result == Activity.RESULT_OK) {
          dbCredentials = DatabaseSetupModel(
            extra(EXTRA_DATABASE), extra(EXTRA_PASSWORD), extra(EXTRA_KEYFILE))
          fragment.databaseReady(dbCredentials.ready)

          if (dbCredentials.ready) {
            for {
              encdb <- keymanager.encryptWithLocalKey(dbCredentials.db.getOrElse(""))
              encpw <- keymanager.encryptWithLocalKey(dbCredentials.password.getOrElse(""))
              enckeyf <- keymanager.encryptWithLocalKey(dbCredentials.keyfile.getOrElse(""))
              verifier <- keymanager.encryptWithLocalKey(KeyManager.VERIFIER)
            } yield {
              val r = for {
                db <- encdb.right
                pw <- encpw.right
                keyf <- enckeyf.right
                ver <- verifier.right
              } yield {
                settings.set(Settings.VERIFY_DATA, ver)
                settings.set(Settings.PASSWORD, pw)
                settings.set(Settings.KEYFILE_PATH, keyf)
                settings.set(Settings.DATABASE_FILE, db)
                setResult(Activity.RESULT_OK)
              }

              r.left.foreach { error =>
                Toast.makeText(
                  this, error.toString, Toast.LENGTH_SHORT).show()
                Application.logException("Setup failure: " + error.toString, error match {
                  case e: Exception => e
                  case _ => new Exception(error.toString)
                })
                finish()
              }
            }
          }
        }
    }
  }

  override def onDestroy() = {
    dismissAllDialogs()
    super.onDestroy()
  }

  ServiceBus += {
    case PINServiceExit  => finish()
  }
}

class SetupFragment extends android.preference.PreferenceFragment {
  val log = Logcat("SetupFragment")
  lazy val settings = Settings(getActivity)
  lazy val dbs = findPreference("database_info")//.asInstanceOf[DatabasePreference]
  lazy val kt = findPreference("keyboard_enable").asInstanceOf[CheckBoxPreference]
  lazy val ae = findPreference("accessibility_enable").asInstanceOf[CheckBoxPreference]
  lazy val ktimeout = findPreference("keyboard_timeout").asInstanceOf[ListPreference]
  lazy val ptimeout = findPreference("pin_timeout").asInstanceOf[ListPreference]
  lazy val pwoverride = findPreference("password_override").asInstanceOf[CheckBoxPreference]
  lazy val fp = findPreference("fingerprint_enable").asInstanceOf[CheckBoxPreference]
  lazy val hk = findPreference("hardware_key_enable").asInstanceOf[CheckBoxPreference]
  lazy val fpm = FingerprintManager(getActivity, settings)
  lazy val pin = findPreference("database_pin")
  lazy val secopts = findPreference("security_options").asInstanceOf[PreferenceGroup]
  private var _onPrefChange = Option.empty[(Preference, Any) => Any]


  def databaseReady(ready: Boolean): Unit = {
    if (ready)
      dbs.setSummary(R.string.has_been_configured)
    else
      dbs.setSummary(R.string.has_not_configured)
  }
  def onPrefChange(p: Preference, value: Any) = {
    _onPrefChange foreach(_(p,value))
    true
  }
  def prefChanged(p: Preference, value: Any) = _onPrefChange foreach (_(p, value))

  def onPreferenceChange[A](fn: (Preference, Any) => A) = _onPrefChange = Option(fn)
  def kboverride = pwoverride.isChecked
  def kboverride_=(b: Boolean) = pwoverride.setChecked(b)
  def pintimeo = ptimeout.getValue.toInt
  def pintimeo_=(i: Int) = {
    ptimeout.setSummary(i.toString)
    ptimeout.setValue(i.toString)
  }
  def fpenabled = fp.isChecked
  def kbtimeo = ktimeout.getValue.toInt
  def kbtimeo_=(i: Int) = {
    ktimeout.setSummary(i.toString)
    ktimeout.setValue(i.toString)
  }

  def databaseOnClick[A](f: () => A) = dbs.onPreferenceClick0 {
    f()
    true
  }
  def pinSetupOnClick[A](f: () => A) = pin.onPreferenceClick0 {
    f()
    true
  }

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    addPreferencesFromResource(R.xml.setup)
    if (fpm.fpm.isEmpty)
      secopts.removePreference(fp)

    if (!KeyManager.hasHardwareKey)
      secopts.removePreference(hk)
  }

  override def onStart() = {
    super.onStart()
    ae.setIntent(new Intent(android.provider.Settings.ACTION_ACCESSIBILITY_SETTINGS))
    kt.setIntent(new Intent(android.provider.Settings.ACTION_INPUT_METHOD_SETTINGS))
    import android.provider.Settings.Secure
    val services = Option(Secure.getString(getActivity.getContentResolver, Secure.ENABLED_ACCESSIBILITY_SERVICES))
    ae.setChecked((services exists (_ contains (getActivity.getPackageName + "/"))) && AccessibilityService.running)

    val imm = getActivity.systemService[InputMethodManager]
    val list = imm.getEnabledInputMethodList
    val enabled = list exists (_.getPackageName == getActivity.getPackageName)
    kt.setChecked(enabled)
    fp.setChecked(settings.get(Settings.FINGERPRINT_ENABLE))
    fp.onPreferenceChange { (pref, b) =>
      prefChanged(pref, b)
      true
    }
    pwoverride.setChecked(settings.get(Settings.PASSWORD_OVERRIDE))

    pwoverride.onPreferenceChange { (pref, b) =>
      prefChanged(pref, b)
      true
    }

    ktimeout.setValue(settings.get(Settings.KEYBOARD_TIMEOUT).toString)
    ktimeout.setSummary(settings.get(Settings.KEYBOARD_TIMEOUT).toString)
    ptimeout.setValue(settings.get(Settings.PIN_TIMEOUT).toString)
    ptimeout.setSummary(settings.get(Settings.PIN_TIMEOUT).toString)

    ptimeout.onPreferenceChange { (pref, v) =>
      prefChanged(pref, v)
      ptimeout.setSummary(v.toString)
      settings.set(Settings.PIN_TIMEOUT, v.toString.toInt)
      true
    }
    ktimeout.onPreferenceChange { (pref, v) =>
      prefChanged(pref, v)
      ktimeout.setSummary(v.toString)
      settings.set(Settings.KEYBOARD_TIMEOUT, v.toString.toInt)
      true
    }

    hk.setChecked(settings.get(Settings.HARDWARE_KEY_ENABLE))
    hk.onPreferenceChange { (_, value) =>

      /*
      val enablehk = value.asInstanceOf[Boolean]

      hk.setEnabled(false)
      if (enablehk != settings.get(Settings.HARDWARE_KEY_ENABLE)) {
        // ugh, this whole thing is loaded with yuck
        val setup = getActivity.asInstanceOf[SetupActivity]
        val wp = if (enablehk) Future.successful(())
        else setup.requestPermission(android.Manifest.permission.GET_ACCOUNTS,
          R.string.require_get_accounts, setup.views.flipper)
        val km = setup.keymanager
        val pinVerifier = settings.get(Settings.PIN_VERIFIER)

        val pv = if (pinVerifier != "") {
          km.decryptWithExternalKeyToString(pinVerifier).flatMap {
            case Right(v) => Future.successful(Right(Some(v)))
            case Left(l) => Future.successful(Left(l))
          }
        } else {
          Future.successful(Right(None))
        }
        wp.onFailureMain { case _ =>
          hk.setChecked(true)
          hk.setEnabled(true)
        }
        val f = for {
          verifier <- pv
          _ <- wp
          _ <- Future(settings.set(Settings.HARDWARE_KEY_ENABLE, enablehk))
          prt <- km.protectLocalKey()
        } yield {
          val r = for {
            lk <- prt.right
            vo <- verifier.right
          } yield {
            vo.map(km.encryptWithExternalKey).map(_.onCompleteMain { x =>
              x.foreach { newverifier =>
                newverifier.right.foreach { nv =>
                  settings.set(Settings.PIN_VERIFIER, nv)
                  hk.setChecked(enablehk)
                }
              }
              settings.set(Settings.LOCAL_KEY, lk)
              hk.setEnabled(true)
            })
            if (vo.isEmpty) UiBus.run {
              hk.setEnabled(true)
              hk.setChecked(enablehk)
            }
          }
          r.left.foreach { l =>
            Application.logException("hk switch error: " + l, new Exception("switching fail"))
            log.e("Failed to switch: " + l)
            Toast.makeText(setup, "Failed to switch key mode: " + l, Toast.LENGTH_LONG).show()
          }
        }
        f.onFailureMain { case e =>
          Toast.makeText(setup, "Failed to switch key mode: " + e, Toast.LENGTH_LONG).show()
          log.e("Failed to switch", e)
          Application.logException("hk switch exception", e)
        }

      }
      */
      false
    }
  }


  override def onResume() = {
    super.onResume()
    if (settings.get(Settings.NEEDS_PIN)) {
      pin.setTitle(R.string.change_pin)
      pin.setSummary(null)
      fp.setEnabled(true)
    } else {
      fp.setEnabled(false)
      pin.setTitle(R.string.setup_pin)
      pin.setSummary(R.string.setup_pin_summary)
    }
  }

  override def onViewCreated(view: View, savedInstanceState: Bundle) = {
    super.onViewCreated(view, savedInstanceState)
    // TextPreference has a divider between them??
    val list = view.findViewById(android.R.id.list).asInstanceOf[ListView]
    list.setDividerHeight(0)
  }
}

class DatabasePreference(ctx: Context, attrs: AttributeSet, res: Int)
  extends android.preference.Preference(ctx, attrs, res) {
  def this(ctx: Context, attrs: AttributeSet) = this(ctx, attrs, 0)
  def this(ctx: Context) = this(ctx, null, 0)
}

object DatabaseSetupModel {
  def empty = DatabaseSetupModel(None, None, None)
  def filterEmpty(s: String) = Option(s) map (_.trim) filterNot (_.isEmpty)
  def apply(db: String, pw: String, kf: String): DatabaseSetupModel = DatabaseSetupModel(
    filterEmpty(db), filterEmpty(pw), filterEmpty(kf))
}
case class DatabaseSetupModel(db: Option[String], password: Option[String], keyfile: Option[String]) {
  def ready = db.nonEmpty && (password.nonEmpty || keyfile.nonEmpty)
}

class DatabaseSetupActivity extends AppCompatActivity with DialogManager with PermissionManager with ActivityResultManager {
  val log = Logcat("DatabaseSetupActivity")
  private[this] var model = DatabaseSetupModel.empty
  private[this] val modelSubject = Var(model)
  val modelChange: Obs[DatabaseSetupModel] = modelSubject
  import iota._
  import RequestCodes._
  import RelativeLayout._
  import ViewGroup.LayoutParams._
  type LP = RelativeLayout.LayoutParams
  val keepassSetupInfo =
    """
      |KeePass Database Setup. Enter your database unlock information below
    """.stripMargin

  @TargetApi(21)
  def tint[A <: View] = condK[A](v(21) ? kestrel { v =>
    val tint = new ColorStateList(Array(Array(0)), Array(resolveAttr(R.attr.colorPrimary, _.data)))
    v.setBackgroundTintList(tint)
  })

  lazy val setupLayout = l[RelativeLayout](
    l[ScrollView](
    l[RelativeLayout](
      w[TextView] >>= k.text(keepassSetupInfo) >>= id(Id.text) >>= lpK(MATCH_PARENT, WRAP_CONTENT)(margins(all = 8.dp)),
      l[TextInputLayout](
        IO(df) >>= id(Id.database_file) >>= k.hint("Database File") >>= lp(MATCH_PARENT, WRAP_CONTENT) >>= k.singleLine(true) >>= tint
      ) >>= id(Id.database_file_layout) >>= lpK(MATCH_PARENT, WRAP_CONTENT) { p: LP =>
        p.addRule(BELOW, Id.text)
        p.addRule(LEFT_OF, Id.database_file_browse)
        margins(left = 8.dp, right = 8.dp)(p)
      },
      w[Button] >>= id(Id.database_file_browse) >>= k.text(R.string.browse) >>= lpK(WRAP_CONTENT, WRAP_CONTENT) { p: LP =>
        p.addRule(ALIGN_PARENT_RIGHT, 1)
        p.addRule(ALIGN_TOP, Id.database_file_layout)
      } >>= hook0.onClick(IO(browseHandler(BROWSE_DATABASE, if (v(19)) "application/*" else "file/*"))),
      l[TextInputLayout](
        IO(dp) >>= id(Id.password) >>= k.hint("Password") >>= tint >>= lp(MATCH_PARENT, WRAP_CONTENT)
          >>= k.inputType(InputType.TYPE_TEXT_VARIATION_PASSWORD) >>= kestrel { e =>
          e.setSingleLine(true)
          e.setTransformationMethod(PasswordTransformationMethod.getInstance)
        }
      ) >>= id(Id.password_layout) >>= lpK(MATCH_PARENT, WRAP_CONTENT) { p: LP =>
        p.addRule(BELOW, Id.database_file_layout)
        margins(left = 8.dp, right = 8.dp)(p)
      },
      l[TextInputLayout](
        IO(dk) >>= id(Id.database_key) >>= k.hint("Key File") >>= lp(MATCH_PARENT, WRAP_CONTENT) >>= kestrel(_.setSingleLine(true)) >>= tint
      ) >>= id(Id.database_key_layout) >>= lpK(MATCH_PARENT, WRAP_CONTENT) { p: LP =>
        p.addRule(BELOW, Id.password_layout)
        p.addRule(LEFT_OF, Id.database_key_browse)
        margins(left = 8.dp, right = 8.dp)(p)
      },
      w[Button] >>= id(Id.database_key_browse) >>= k.text(R.string.browse) >>= lpK(WRAP_CONTENT, WRAP_CONTENT) { p: LP =>
        p.addRule(BELOW, Id.database_key_layout)
        p.addRule(ALIGN_PARENT_RIGHT, 1)
        p.addRule(ALIGN_TOP, Id.database_key_layout)
      } >>= hook0.click(IO(browseHandler(BROWSE_KEYFILE, "*/*")))
    ) >>= padding(all = 16.dp)) >>= lpK(MATCH_PARENT, WRAP_CONTENT) { p: LP =>
      p.addRule(ALIGN_PARENT_TOP, 1)
      p.addRule(ABOVE, Id.save)
      p.alignWithParent = true
    },
    IO(errorText) >>= id(Id.error_text) >>= k.gravity(Gravity.CENTER) >>=
      kestrel(_.setTextColor(0xffff0000)) >>= lpK(MATCH_PARENT, WRAP_CONTENT){ p: LP =>
      p.addRule(ABOVE, Id.save)
      p.alignWithParent = true
      margins(all = 8.dp)(p)
    },
    IO(progressBar) >>= lpK(WRAP_CONTENT, WRAP_CONTENT) { p: LP =>
      p.addRule(CENTER_HORIZONTAL, 1)
      p.addRule(ABOVE, Id.save)
      margins(all = 8.dp)(p)
    } >>= k.visibility(View.GONE) >>= id(Id.progress),
    IO(saveButton) >>= id(Id.save) >>= k.text(R.string.save) >>= lpK(MATCH_PARENT, WRAP_CONTENT) { p: LP =>
      p.addRule(ALIGN_PARENT_BOTTOM, 1)
      margins(all = 8.dp)(p)
    } >>= hook0.click(save())
  ) >>= k.backgroundResource(android.R.drawable.picture_frame)

  lazy val progressBar = new ProgressBar(this, null, android.R.attr.progressBarStyleSmall)
  lazy val dk: AppCompatEditText = new AppCompatEditText(this)
  lazy val df: AppCompatEditText = new AppCompatEditText(this)
  lazy val dp: AppCompatEditText = new AppCompatEditText(this)
  lazy val saveButton = new Button(this)
  lazy val errorText = new TextView(this)

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    getSupportActionBar.setDisplayHomeAsUpEnabled(true)
    setContentView(setupLayout.perform())
    dp.onTextChanged { s =>
      model = model.copy(password = s.? map (_.toString.trim) filterNot (_.isEmpty))
      modelSubject() = model
    }
    df.onTextChanged { s =>
      model = model.copy(db = s.? map (_.toString.trim) filterNot (_.isEmpty))
      modelSubject() = model
    }
    dk.onTextChanged { s =>
      model = model.copy(keyfile = s.? map (_.toString.trim) filterNot (_.isEmpty))
      modelSubject() = model
    }
    modelChange.subscribe { m =>
      saveButton.setVisibility(if (m.ready) View.VISIBLE else View.GONE)
      saveButton.setEnabled(m.ready)
      if (m.ready)
        errorText.setVisibility(View.GONE)
    }
    val intent = Option(getIntent)
    intent.flatMap(i => Option(i.getStringExtra(EXTRA_DATABASE))).foreach(df.setText)
    intent.flatMap(i => Option(i.getStringExtra(EXTRA_PASSWORD))).foreach(dp.setText)
    intent.flatMap(i => Option(i.getStringExtra(EXTRA_KEYFILE))).foreach(dk.setText)
    setResult(Activity.RESULT_CANCELED)
    requestPermission(android.Manifest.permission.WRITE_EXTERNAL_STORAGE,
      R.string.require_external_storage, saveButton) onFailureMain { case _ => finish() }
  }


  override def onCreateOptionsMenu(menu: Menu) = {
    val b = super.onCreateOptionsMenu(menu)
    if (v(19) && Database.writeSupported) {
      getMenuInflater.inflate(R.menu.db_setup, menu)
      true
    } else b
  }

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
      case R.id.create_database =>
        new CreateDatabaseFragment().show(getFragmentManager, "create-database")
        true
      case _ =>
        super.onOptionsItemSelected (item)
    }
  }

  def createNewDatabase(newpw: String): Unit = {
    val intent = new Intent(Intent.ACTION_CREATE_DOCUMENT)
    intent.addCategory(Intent.CATEGORY_OPENABLE)
    intent.putExtra(Intent.EXTRA_TITLE, getString(R.string.database_new_file))
    intent.setType("application/vnd.keepass")
    (for {
      i <- requestActivityResult(intent)
      p <- Database.resolvePath(i.getData.toString)
    } yield {
      val db = new PwDatabase
      val key = new CompositeKey
      key.AddUserKey(new KcpPassword(newpw))
      db.New(IOConnectionInfo.FromPath(p), key)
      val root = db.getRootGroup
      root.setName(getString(R.string.passwords))
      root.Touch(true)
      val entry = new PwEntry(true, true)
      entry.getStrings.Set(PwDefs.TitleField, new ProtectedString(false, getString(R.string.database_password)))
      entry.getStrings.Set(PwDefs.PasswordField, new ProtectedString(true, newpw))
      entry.getStrings.Set(PwDefs.NotesField, new ProtectedString(false, getString(R.string.password_reminder_note)))

      root.getEntries.Add(entry)
      entry.setParentGroup(root)
      db.Save(null)
      for {
        in  <- using(new FileInputStream(p))
        out <- using(Application.instance.getContentResolver.openOutputStream(Uri.parse(i.getData.toString)))
      } {
        val buf = Array.ofDim[Byte](32768)
        Stream.continually(in.read(buf, 0, 32768)) takeWhile (_ != -1) foreach { read =>
          out.write(buf, 0, read)
        }
      }
      (i,p,newpw)
    }) onCompleteMain {
      case util.Success((i,path, pw)) =>
        dp.setText(pw)
        setDataPath(i, df.setText)
      case util.Failure(e) if e != ActivityResultCancel =>
        Toast.makeText(this, "Failed: " + e, Toast.LENGTH_LONG).show()
        log.e("Failed to create database", e)
      case util.Failure(_) =>
    }
  }

  def save(): IO[Unit] = IO {
    hideIME()
    saveButton.setEnabled(false)
    val database = model.db.getOrElse("")
    val keyfile = model.keyfile.getOrElse("")

    val settings = Settings(this)
    val keymanager = new KeyManager(this, settings)

    val keyf = new File(keyfile).getAbsoluteFile
    val password = model.password.getOrElse("")
    if (keyfile != "" && !keyfile.startsWith("content:") && !keyf.isFile) {
      error(R.string.keyfile_no_exist)
    } else  if (password == "" && keyfile == "") {
      error(R.string.password_no_blank)
    } else {

      val db = new File(database).getAbsoluteFile
      if (!database.startsWith("content:") && !db.isFile) {
        error(R.string.database_no_exist)
      } else {
        progressBar.setVisibility(View.VISIBLE)
        val f = Database.open(database, Option(password),
          Option(keyfile) filterNot (_.isEmpty))

        f onSuccessMain { case _ =>
            val intent = Option(getIntent) getOrElse new Intent
            intent.putExtra(EXTRA_DATABASE, model.db.orNull)
            intent.putExtra(EXTRA_PASSWORD, model.password.orNull)
            intent.putExtra(EXTRA_KEYFILE, model.keyfile.orNull)
            setResult(Activity.RESULT_OK, intent)
            finish()
        }

        f onFailureMain { case e =>
          error("Unable to open database: " + e.getMessage)
          saveButton.setEnabled(true)
          Application.logException("onFailureMain opening database", e)
          log.e("failed to load database", e)
        }

        f onCompleteMain { _ =>
          saveButton.setEnabled(true)
          progressBar.setVisibility(View.GONE)
        }
      }
    }

    ()
  }

  def browseHandler(id: Int, mimeType: String) = {
    val intent = new Intent(if (kitkatAndNewer)
      Intent.ACTION_OPEN_DOCUMENT else Intent.ACTION_GET_CONTENT)
    intent.addCategory(Intent.CATEGORY_OPENABLE)
    intent.setType(mimeType)

    try {
      startActivityForResult(intent, id)
    } catch {
      case e: ActivityNotFoundException =>
        Toast.makeText(this, if (iota.v(19))
          R.string.android_saf_required else R.string.must_install_explorer,
          Toast.LENGTH_SHORT).show()
      case e: Exception =>
        Toast.makeText(this,
          "Unable to open file: " + e.getMessage, Toast.LENGTH_LONG).show()
    }
  }

  override def onActivityResult(requestCode: Int, resultCode: Int, data: Intent) = {
    super.onActivityResult(requestCode, resultCode, data)
    requestCode match {
      case BROWSE_DATABASE =>
        if (resultCode == Activity.RESULT_OK)
          setDataPath(data, df.setText)
      case BROWSE_KEYFILE =>
        if (resultCode == Activity.RESULT_OK)
          setDataPath(data, dk.setText)
      case _ =>
        if (resultCode != Activity.RESULT_OK)
          finish()
    }
  }
  def setDataPath(data: Intent, setProperty: String => Unit): Unit = {
    val uri = data.getData
    if (uri == null) {
      Toast.makeText(this, "File explorer returned no result", Toast.LENGTH_LONG).show()
    } else if (uri.getScheme == "content") {
      val progress = ProgressDialog.show(this,
        "Downloading", "Please Wait", false, true)
      showingDialog(progress)
      var canceled = false
      progress.onCancel0 {
        canceled = true
      }
      Future {
        var size = -1
        var name = uri.getLastPathSegment
        for {
          c <- using(getContentResolver.query(uri, null, null, null, null))
        } {
          var lastModified = Option.empty[Long]
          val cName = c.getColumnIndex(OpenableColumns.DISPLAY_NAME)
          val cSize = c.getColumnIndex(OpenableColumns.SIZE)
          if (cName != -1) {
            Stream.continually(c.moveToNext) takeWhile identity foreach { _ =>
              if (!c.isNull(cName)) {
                name = c.getString(cName)
                if (!c.isNull(cSize)) {
                  size = c.getInt(cSize) // oh well, overflow don't care
                }
                if (kitkatAndNewer) {
                  val cLastModified = c.getColumnIndex(
                    DocumentsContract.Document.COLUMN_LAST_MODIFIED)
                  if (cLastModified != -1 && !c.isNull(cLastModified))
                    lastModified = Some(c.getLong(cLastModified))
                }
              }
            }
          }
        }
        UiBus.post {
          if (size != -1) {
            progress.setMax(size)
            progress.setIndeterminate(false)
          }
        }
        val external = getExternalFilesDir(null)
        val dest = new java.io.File(external, if (kitkatAndNewer)
          KeyManager.sha1(uri.toString.getBytes("utf-8")) else name)
        @TargetApi(19)
        def takePermissions() {
          if (kitkatAndNewer)
            getContentResolver.takePersistableUriPermission(uri,
              Intent.FLAG_GRANT_WRITE_URI_PERMISSION |
                Intent.FLAG_GRANT_READ_URI_PERMISSION)
        }
        takePermissions()

        UiBus.post { setProperty(if (kitkatAndNewer) uri.toString else dest.getAbsolutePath) }
        try {
          for {
            input <- using(getContentResolver.openInputStream(uri))
            out <- using(new FileOutputStream(dest))
          } {
            var total = 0
            val buf = Array.ofDim[Byte](32768)
            Stream.continually(input.read(buf, 0, 32768)) takeWhile (
              _ != -1 && !canceled) foreach { read =>
              total += read
              out.write(buf, 0, read)
              UiBus.post {
                progress.setProgress(total)
              }
            }
            if (canceled)
              dest.delete()
          }
        } catch {
          case e: IOException =>
            UiBus.post {
              Toast.makeText(this, "Failed to open file: " + e.getMessage, Toast.LENGTH_LONG).show()
            }
        }
        UiBus.post {
          dismissDialog(progress)
        }
      }
    } else {
      setProperty(uri.getPath)
    }
  }

  override def onDestroy() = {
    super.onDestroy()
    dismissAllDialogs()
  }
  def error(error: String) {
    errorText.setVisibility(View.VISIBLE)
    errorText.setText(error)
    saveButton.setEnabled(false)
    saveButton.setVisibility(View.GONE)
  }
  def error(err: Int): Unit = error(getString(err))

  def hideIME(): Unit =
    Option(getCurrentFocus).foreach(f => systemService[InputMethodManager].hideSoftInputFromWindow(f.getWindowToken, 0))
}

class CreateDatabaseFragment extends AlertDialogFragment {
  import iota._
  import ViewGroup.LayoutParams._
  def title = getString(R.string.create_database)
  def activity = getActivity.asInstanceOf[DatabaseSetupActivity]
  lazy val password = new StandardEditView(getActivity)
  lazy val confirm = new StandardEditView(getActivity)
  val passwordTweaks: Kestrel[StandardEditView] = kestrel { v =>
    v.hint = getString(R.string.password)
    v.first = true
    v.password = true
    v.errors = true
    v.icon = R.drawable.ic_lock_outline_black_36dp
  }
  override def positiveLabel = getString(R.string.create)

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, savedInstanceState: Bundle) = {
    password.textfield.onTextChanged(_ => password.error = null)
    confirm.textfield.onTextChanged(_ => confirm.error = null)
    (l[LinearLayout](
      IO(password) >>= passwordTweaks >>= id(Id.password) >>= lp(MATCH_PARENT, WRAP_CONTENT),
      IO(confirm) >>= passwordTweaks >>= id(Id.confirm_password) >>=
        kestrel(_.hint = getString(R.string.confirm_password)) >>= lp(MATCH_PARENT, WRAP_CONTENT)
    ) >>= k.orientation(LinearLayout.VERTICAL) >>= padding(all = 16.dp)).perform()
  }

  override def onStart(): Unit = {
    super.onStart()
    getDialog.asInstanceOf[android.app.AlertDialog].getButton(DialogInterface.BUTTON_POSITIVE).onClick0 {
      password.error = if (password.text.length < 8)
        getString(R.string.password_min_length)
      else null
      confirm.error = if (password.text != confirm.text && password.error == null)
        getString(R.string.confirm_password_match)
      else null

      if (password.error == null && confirm.error == null) {
        getDialog.dismiss()
        activity.createNewDatabase(password.text)
      }
    }
  }
}
