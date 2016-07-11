package com.hanhuy.android.keepshare

import android.app.{AlertDialog, Fragment, FragmentTransaction, Activity}
import android.content.{Context, Intent}
import android.graphics.BitmapFactory
import android.graphics.drawable.{LayerDrawable, BitmapDrawable}
import android.os.Bundle
import android.support.v7.app.ActionBar
import android.support.v7.widget.Toolbar
import android.text.{Spanned, SpannableString}
import android.text.method.{LinkMovementMethod, PasswordTransformationMethod}
import android.text.style.ForegroundColorSpan
import android.text.util.Linkify
import android.util.AttributeSet
import android.view._
import android.widget.{TextView, FrameLayout}
import com.hanhuy.android.common.Futures
import com.hanhuy.keepassj._

import Futures._
import EntryViewActivity._
import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.keepassj.spr.{SprEngine, SprCompileFlags, SprContext}
import scala.collection.JavaConverters._
import TypedResource._

import scala.concurrent.Promise

/**
 * @author pfnguyen
 */
object EntryViewActivity {
  val TOTP_MODE = "KeepShare-TimeOtpView"
  val HOTP_MODE = "KeepShare-HmacOtpView"
  val EXTRA_CREATE = "keepshare.extra.CREATE"
  val EXTRA_CREATE_DATA = "keepshare.extra.CREATE_DATA"
  val EXTRA_ENTRY_ID = "keepshare.extra.ENTRY_ID"
  val EXTRA_HISTORY_IDX = "keepshare.extra.HISTORY_IDX"
  val STATE_IS_EDITING = "keepshare.isEditing"

  def show(a: Activity, e: String): Unit = {
    val intent = new Intent(a, classOf[EntryViewActivity])
    intent.putExtra(EXTRA_ENTRY_ID, e)
    a.startActivity(intent)
    PINHolderService.ping()
  }
  def show(a: Activity, e: PwEntry): Unit = {
    val intent = new Intent(a, classOf[EntryViewActivity])
    intent.putExtra(EXTRA_ENTRY_ID, e.getUuid.ToHexString)
    a.startActivity(intent)
    PINHolderService.ping()
  }
  def show(a: Activity, e: PwEntry, historyIndex: Int): Unit = {
    val intent = new Intent(a, classOf[EntryViewActivity])
    intent.putExtra(EXTRA_ENTRY_ID, e.getUuid.ToHexString)
    intent.putExtra(EXTRA_HISTORY_IDX, historyIndex)
    a.startActivity(intent)
    PINHolderService.ping()
  }
  def create(a: Activity, g: Option[PwGroup], data: Option[EntryCreateData] = None): Unit = {
    val intent = new Intent(a, classOf[EntryViewActivity])
    intent.putExtra(EXTRA_CREATE, true)
    intent.putExtra(EXTRA_CREATE_DATA, data.orNull)
    intent.putExtra(BrowseActivity.EXTRA_GROUP_ID, g.map(_.getUuid.ToHexString).orNull)
    a.startActivity(intent)
    PINHolderService.ping()
  }

  sealed trait CharType
  case object Uppercase extends CharType
  case object Lowercase extends CharType
  case object Symbol extends CharType
  case object Digit extends CharType
  case class Spans(chartype: CharType, start: Int, end: Int)
  def colorPassword(p: String): CharSequence = {
    val (spans,last) = p.zipWithIndex.foldLeft((List.empty[Spans],Spans(Lowercase, 0, 0))) { case ((a,span), (c,i)) =>
      val chartype = if (c.isDigit) Digit
      else if (c.isLower) Lowercase
      else if (c.isUpper) Uppercase
      else Symbol
      if (span.chartype == chartype) {
        (a,span)
      } else {
        (span.copy(end = i) :: a,Spans(chartype, i, i))
      }
    }
    val s = new SpannableString(p)
    (last.copy(end = p.length) :: spans).foreach { span =>
      if (span.end > 0) {
        val fg = span.chartype match {
          case Uppercase => new ForegroundColorSpan(0xff5C6BC0) // indigo 400
          case Lowercase => new ForegroundColorSpan(0xff424242) // grey 800
          case Symbol    => new ForegroundColorSpan(0xff009688) // teal 500
          case Digit     => new ForegroundColorSpan(0xff9C27B0) // purple 500
        }
        s.setSpan(fg, span.start, span.end, Spanned.SPAN_INCLUSIVE_EXCLUSIVE)
      }
    }
    s
  }

  case class EntryCreateData(title: Option[String] = None,
                             username: Option[String] = None,
                             password: Option[String] = None,
                             url: Option[String] = None,
                             notes: Option[String] = None,
                             fields: List[(Boolean,String,String)] = Nil)
}
class EntryViewActivity extends AuthorizedActivity {
  private var pwentry = Option.empty[PwEntry]
  private var isEditing = false
  private var isCreating = false
  lazy val editBar: TypedViewHolder.entry_edit_action_bar =
    TypedViewHolder.inflate(getLayoutInflater, TR.layout.entry_edit_action_bar, null, false)

  def confirmPrompt[A](onCancel: => A): Unit = {
    val edited = Option(getFragmentManager.findFragmentByTag("editor")) collect {
      case editor: EntryEditFragment => editor.baseModel.isEmpty || editor.baseModel.exists(_.needsBackup(editor.model))
    }
    if (edited.getOrElse(false)) {
      new AlertDialog.Builder(this)
        .setTitle(R.string.cancel)
        .setMessage(R.string.discard_confirm)
        .setNegativeButton(R.string.discard, () => onCancel)
        .setPositiveButton(R.string.keep_editing, null)
        .show()
    } else onCancel

  }
  override def onBackPressed() = {
    if (isEditing && !isCreating) {
      confirmPrompt { editing(false) }
    } else {
      if (isCreating) {
        confirmPrompt {
          super.onBackPressed()
          overridePendingTransition(
            android.R.anim.slide_in_left, android.R.anim.slide_out_right)
        }
      } else {
        super.onBackPressed()
        overridePendingTransition(
          android.R.anim.slide_in_left, android.R.anim.slide_out_right)
      }
    }
  }

  lazy val authenticatedPromise = Promise[Unit]()

  lazy val views: TypedViewHolder.entry_view = TypedViewHolder.setContentView(this, TR.layout.entry_view)

  override def onCreate(savedInstanceState: Bundle) = {
    if (!BuildConfig.DEBUG) {
      getWindow.setFlags(WindowManager.LayoutParams.FLAG_SECURE,
        WindowManager.LayoutParams.FLAG_SECURE)
    }
    super.onCreate(savedInstanceState)
    for {
      intent <- Option(getIntent)
      entry <- Option(intent.getStringExtra(EXTRA_ENTRY_ID))
    } {
      val uuid = new PwUuid(KeyManager.bytes(entry))
      database map { db =>
        db -> db.getRootGroup.FindEntry(uuid, true)
      } onSuccessMain { case (db, e) =>
        showEntry(e, db)
        if (Option(savedInstanceState) exists (_.getBoolean(STATE_IS_EDITING, false))) {
          editing(true)
        }
      }
    }
    authenticatedPromise.future.onSuccessMain { case _ =>
      views.fab.onClick0 {
        editing(true)
      }
      editBar.cancel.onClick0 {
        if (isCreating)
          finish()
        editing(false)
      }
      editBar.save.onClick0 {
        val f = Option(getFragmentManager.findFragmentByTag("editor"))
        f foreach { case editor: EntryEditFragment =>
          def setField(e: PwEntry, field: String, value: Option[String], isPassword: Boolean): Unit = {
            value.foreach { s =>
              e.getStrings.Set(field, new ProtectedString(false, s))
            }
          }
          def copyFromModel(e: PwEntry, needMove: Boolean): Unit = {
            val custom = e.getStrings.asScala.map { case entry => entry.getKey } filterNot PwDefs.IsStandardField
            custom foreach e.getStrings.Remove

            setField(e, PwDefs.TitleField, editor.model.title, false)
            setField(e, PwDefs.UserNameField, editor.model.username, false)
            setField(e, PwDefs.PasswordField, editor.model.password, true)
            setField(e, PwDefs.NotesField, editor.model.notes, false)
            setField(e, PwDefs.UrlField, editor.model.url, false)
            e.setIconId(PwIcon.values()(Database.Icons.indexOf(editor.model.icon)))
            editor.model.fields foreach { case (k, v) => e.getStrings.Set(k, v) }

            if (needMove) {
              Option(e.getParentGroup) foreach (_.getEntries.Remove(e))
              Database.rootGroup foreach { root =>
                val group = root.FindGroup(editor.model.group, true)
                group.getEntries.Add(e)
                e.setParentGroup(group)
              }
            }
          }
          if (isCreating) {
            val e = new PwEntry(true, true)
            copyFromModel(e, true)
            showEntry(e, Database.pwdatabase)
          } else {
            val needBackup = editor.baseModel.exists(_.needsBackup(editor.model))
            val needMove = editor.baseModel.exists(_.group != editor.model.group)
            pwentry foreach { e =>
              if (needBackup) e.CreateBackup(null)
              copyFromModel(e, needMove)
              e.Touch(true, false)
              showEntry(e, Database.pwdatabase)
            }
          }
        }
        editing(false)
        DatabaseSaveService.save()
      }

      getSupportActionBar.setCustomView(editBar.rootView, new ActionBar.LayoutParams(
        ViewGroup.LayoutParams.MATCH_PARENT,
        ViewGroup.LayoutParams.MATCH_PARENT))

      for {
        intent <- Option(getIntent)
      } {
        if (intent.getBooleanExtra(EXTRA_CREATE, false)) {
          creating(Option(intent.getStringExtra(BrowseActivity.EXTRA_GROUP_ID)),
            Option(intent.getSerializableExtra(EntryViewActivity.EXTRA_CREATE_DATA).asInstanceOf[EntryCreateData]))
        }
      }
    }
  }
  override def onAuthenticated(): Unit = authenticatedPromise.trySuccess()

  def saveGeneratedPassword(pw: CharSequence): Unit = {
    Option(getFragmentManager.findFragmentByTag("editor")) foreach { case editor: EntryEditFragment =>
        editor.password.text = pw
    }
  }
  def creating(parent: Option[String], data: Option[EntryCreateData] = None): Unit = {
    updating(true, EntryEditFragment.create(parent, data))
    isCreating = true
    editBar.title.setText("Create entry")
  }
  def editing(b: Boolean): Unit = {
    pwentry foreach { pwe =>
      updating(b, if (b) EntryEditFragment.edit(pwe) else null)
      editBar.title.setText("Update entry")
    }
  }

  def updating(b: Boolean, f: Fragment) {
    getSupportActionBar.setHomeButtonEnabled(!b)
    getSupportActionBar.setDisplayShowHomeEnabled(!b)
    getSupportActionBar.setDisplayHomeAsUpEnabled(!b)
    getSupportActionBar.setDisplayShowTitleEnabled(!b)
    getSupportActionBar.setDisplayShowCustomEnabled(b)
    if (b) {
      editBar.rootView.getParent match {
        case t: Toolbar => t.setContentInsetsAbsolute(0, 0)
        case _ =>
      }
      views.fab.hide()
      if (getFragmentManager.findFragmentByTag("editor") == null)
        getFragmentManager.beginTransaction()
          .add(R.id.content, f, "editor")
          .setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
          .addToBackStack("edit")
          .commit()
    } else {
      isCreating = false
      views.fab.show()
      getFragmentManager.popBackStack()
    }
    isEditing = b
    invalidateOptionsMenu()
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    if (!isEditing) {
      getMenuInflater.inflate(R.menu.entry_view, menu)
      pwentry foreach { e =>
        val item = menu.findItem(R.id.toggle_otp)
        val hasOTP = e.getStrings.GetKeys.asScala.exists(_ startsWith "HmacOtp-Secret")
        if (!hasOTP) {
          menu.removeItem(R.id.toggle_otp)
        }
        item.setChecked(e.getStrings.GetKeys.asScala.contains(TOTP_MODE))
      }
      super.onCreateOptionsMenu(menu)
    }
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case android.R.id.home =>
      onBackPressed()
      true
    case R.id.load_keyboard =>
      pwentry foreach (ShareActivity.selectHandler(this, settings, _))
      true
    case R.id.toggle_otp =>
      val otpmode = !item.isChecked
      pwentry foreach { e =>
        item.setChecked(otpmode)
        if (otpmode) {
          OtpFragment.show(getFragmentManager, e)
          e.getStrings.Set(TOTP_MODE, new ProtectedString(false, "true"))
        } else {
          e.getStrings.Remove(TOTP_MODE)
        }
        e.Touch(true, false)
        DatabaseSaveService.save()
      }
      true
    case _ => super.onOptionsItemSelected(item)
  }

  private def showEntry(e: PwEntry, db: PwDatabase): Unit = {
    val histIdx = for {
      intent <- Option(getIntent)
      extra  <- Option(intent.getIntExtra(EXTRA_HISTORY_IDX, -1)) if extra != -1
    } yield extra
    val entry = histIdx map { i =>
      val hist = e.getHistory
      val c = hist.getUCount
      hist.GetAt(if (i >= c) c - 1 else i)
    } getOrElse e
    pwentry = Some(entry)

    val ab = getSupportActionBar
    val strings = entry.getStrings
    ab.setTitle(strings.ReadSafe(PwDefs.TitleField))
//    if (PwUuid.Zero == entry.getCustomIconUuid) {
      val bm = BitmapFactory.decodeResource(getResources,
        if (histIdx.nonEmpty) R.drawable.ic_history_black_36dp
        else Database.Icons(entry.getIconId.ordinal))
      val bd = new BitmapDrawable(getResources, bm)
      bd.setGravity(Gravity.CENTER)
      val layers = new LayerDrawable(Array(bd, getResources.getDrawable(R.drawable.logo_frame)))
      ab.setIcon(layers)
      ab.setDisplayShowHomeEnabled(true)
      ab.setDisplayHomeAsUpEnabled(true)
//    }

    val fieldlist = views.field_list
    fieldlist.removeAllViews()

    var first = true
    if (Option(strings.Get(PwDefs.UserNameField)) exists (_.Length > 0)) {
      val userfield = new StandardFieldView(this)
      userfield.hint = "Username"
      userfield.text = Database.getField(entry, PwDefs.UserNameField) getOrElse ""
      userfield.icon = R.drawable.ic_account_box_black_36dp
      userfield.first = first
      first = false
      fieldlist.addView(userfield)
    }

    if (Option(strings.Get(PwDefs.PasswordField)) exists (_.Length > 0)) {
      val passfield = new StandardFieldView(this)
      passfield.first = first
      first = false
      passfield.hint = "Password"
      passfield.text = Database.getField(entry, PwDefs.PasswordField).fold("": CharSequence)(colorPassword)
      passfield.icon = R.drawable.ic_lock_outline_black_36dp
      passfield.password = true
      fieldlist.addView(passfield)
    }

    if (Option(strings.Get(PwDefs.UrlField)) exists (_.Length > 0)) {
      val linkfield = new StandardFieldView(this)
      linkfield.first = first
      first = false
      linkfield.hint = "URL"
      linkfield.icon = R.drawable.ic_link_black_36dp
      linkfield.textfield.setMovementMethod(LinkMovementMethod.getInstance)
      linkfield.textfield.setLinksClickable(true)
      linkfield.textfield.setAutoLinkMask(Linkify.WEB_URLS)
      linkfield.text = strings.ReadSafeEx(PwDefs.UrlField)

      fieldlist.addView(linkfield)
    }

    if (!Database.writeSupported)
      views.fab.setVisibility(View.GONE)

    if (histIdx.isEmpty) {
      val groupfield = new GroupFieldView(this, e.getParentGroup)
      groupfield.first = first
      groupfield.setBackgroundResource(R.drawable.list_selector_background)
      first = false
      fieldlist.addView(groupfield)
    } else {
      views.fab.setVisibility(View.GONE)
    }

    if (Option(strings.Get(PwDefs.NotesField)) exists (_.Length > 0)) {
      val notesfield = new StandardFieldView(this)
      notesfield.first = first
      first = false
      notesfield.long = true
      notesfield.hint = "Notes"
      notesfield.textfield.setMinLines(8)
      notesfield.icon = R.drawable.ic_speaker_notes_black_36dp
      notesfield.textfield.setMovementMethod(LinkMovementMethod.getInstance)
      notesfield.textfield.setLinksClickable(true)
      notesfield.textfield.setAutoLinkMask(Linkify.WEB_URLS |
        Linkify.EMAIL_ADDRESSES | Linkify.PHONE_NUMBERS | Linkify.MAP_ADDRESSES)
      notesfield.text = strings.ReadSafeEx(PwDefs.NotesField)
      fieldlist.addView(notesfield)
    }

    val sprcontext = new SprContext(e, db, SprCompileFlags.All.flags)
    strings.asScala map { e => (e.getKey,e.getValue) } foreach { case (k,v) =>
      if (!PwDefs.IsStandardField(k)) {
        val field = new CustomField(this)
        field.first = first
        first = false
        field.hint = k
        val fvalue = SprEngine.Compile(v.ReadString, sprcontext)
        field.text = if (v.isProtected) EntryViewActivity.colorPassword(fvalue) else fvalue
        field.password = v.isProtected
        fieldlist.addView(field)
      }
    }

    val fmt = android.text.format.DateFormat.getMediumDateFormat(this)
    val fmt2 = android.text.format.DateFormat.getTimeFormat(this)
    val ctime, mtime = new CustomField(this)
    mtime.icon = R.drawable.ic_access_time_black_36dp
    mtime.iconfield.setContentDescription("Access Times")
    mtime.hint = "Last Modified"
    mtime.text = fmt.format(entry.getLastModificationTime) + " " + fmt2.format(entry.getLastModificationTime)
    mtime.inputlayout.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].bottomMargin = (getResources.getDisplayMetrics.density * 2).toInt
    mtime.textfield.setTextIsSelectable(false)
    ctime.first = true
    ctime.iconfield.setVisibility(View.INVISIBLE)
    ctime.hint = "Created On"
    ctime.textfield.setTextIsSelectable(false)
    ctime.inputlayout.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].topMargin = (getResources.getDisplayMetrics.density * 2).toInt
    ctime.text = fmt.format(entry.getCreationTime) + " " + fmt2.format(entry.getCreationTime)
    entry.getCreationTime
    fieldlist.addView(mtime)
    fieldlist.addView(ctime)

    if (histIdx.isEmpty) {
      val history = entry.getHistory.asScala.toList.zipWithIndex.sortBy(
        _._1.getLastModificationTime).reverseMap { case (h, i) =>
        val field = new CustomField(this)
        field.hint = fmt.format(h.getLastModificationTime) + " " + fmt2.format(h.getLastModificationTime)
        field.text = Database.getField(h, PwDefs.TitleField) getOrElse "<no title>"
        field.inputlayout.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].topMargin = (getResources.getDisplayMetrics.density * 1).toInt
        field.inputlayout.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].bottomMargin = (getResources.getDisplayMetrics.density * 1).toInt
        field.textfield.setTextIsSelectable(false)
        import iota.std.Contexts._
        val bg = iota.resolveAttr(android.R.attr.selectableItemBackground, _.resourceId)
        field.setBackgroundResource(bg)
        field.onClick0(EntryViewActivity.show(this, h, i))
        field.iconfield.setVisibility(View.GONE)
        field.icon = R.drawable.ic_history_black_36dp
        field.first = true
        field
      }

      (history.take(1).map { f =>
        f.first = false
        f.inputlayout.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams].topMargin = (getResources.getDisplayMetrics.density * 12).toInt
        f.iconfield.setVisibility(View.VISIBLE)
        f.iconfield.setContentDescription("History")
        f
      } ::: history.drop(1)) foreach fieldlist.addView
    }
    if (e.getStrings.GetKeys.asScala.contains(TOTP_MODE))
      OtpFragment.show(getFragmentManager, e)
  }

  override def onSaveInstanceState(outState: Bundle) = {
    outState.putBoolean(STATE_IS_EDITING, isEditing)
    super.onSaveInstanceState(outState)
  }
}

class StandardFieldView(c: Context, attrs: AttributeSet) extends FrameLayout(c, attrs) with TypedFindView {
  def this(c: Context) = this(c, null)
  lazy val textfield = findView(TR.field)
  lazy val iconfield = findView(TR.field_icon)
  lazy val divider = findView(TR.field_divider)
  lazy val visibility = {
    val cb = findView(TR.field_visibility)
    cb.onCheckedChange((_, b) =>
      textfield.setTransformationMethod(if (b)
        null else PasswordTransformationMethod.getInstance)
    )
    cb
  }

  // this can't be converted into TypedViewHolder: there is no "inheritance"
  // structure between layout files and their associated view holders
  def inflate() = LayoutInflater.from(c).inflate(R.layout.standard_field, this, true)

  inflate()
  setMinimumHeight((getResources.getDisplayMetrics.density * 48).toInt)
  setSaveEnabled(false)
  setSaveFromParentEnabled(false)

  private[this] var _long: Boolean = false
  def long: Boolean = _long
  def long_=(value: Boolean): Unit = {
    if (value) {
      textfield.setSingleLine(false)
      textfield.setMinLines(8)
      textfield.setGravity(Gravity.TOP | Gravity.LEFT)
      (textfield.getLayoutParams match {
        case f: FrameLayout.LayoutParams => f
        case _ =>
          textfield.getParent.asInstanceOf[ViewGroup].getLayoutParams.asInstanceOf[FrameLayout.LayoutParams]
      }).gravity = Gravity.TOP | Gravity.LEFT
      iconfield.getLayoutParams.asInstanceOf[FrameLayout.LayoutParams].gravity = Gravity.TOP | Gravity.LEFT
    } else {
      textfield.setSingleLine(true)
      textfield.setMinLines(1)
      textfield.setGravity(Gravity.CENTER | Gravity.LEFT)
      (textfield.getLayoutParams match {
        case f: FrameLayout.LayoutParams => f
        case _ =>
          textfield.getParent.asInstanceOf[ViewGroup].getLayoutParams.asInstanceOf[FrameLayout.LayoutParams]
      }).gravity = Gravity.CENTER | Gravity.LEFT
      iconfield.getLayoutParams.asInstanceOf[FrameLayout.LayoutParams].gravity = Gravity.CENTER | Gravity.LEFT
    }

    _long = value
  }

  private[this] var _first: Boolean = false
  def first: Boolean = _first
  def first_=(value: Boolean): Unit = {
    divider.setVisibility(if (value) View.GONE else View.VISIBLE)
    _first = value
  }

  private[this] var _icon: Int = 0
  def icon: Int = _icon
  def icon_=(value: Int): Unit = {
    iconfield.setImageResource(value)
    _icon = value
  }

  def hint = textfield.getHint
  def hint_=(s: String) = textfield.setHint(s)
  def text_=(s: CharSequence) = textfield.setText(s, TextView.BufferType.SPANNABLE)
  def text = textfield.getText.toString

  private[this] var _password: Boolean = false
  def password: Boolean = _password
  def password_=(value: Boolean): Unit = {
    textfield.setTransformationMethod(if (value)
      PasswordTransformationMethod.getInstance else null)
    visibility.setVisibility(if (value) View.VISIBLE else View.GONE)
    _password = value
  }
}

class GroupFieldView(a: AuthorizedActivity, g: PwGroup) extends StandardFieldView(a, null) {
  override def inflate() = a.getLayoutInflater.inflate(R.layout.group_field, this, true)
  lazy val imagefield = findView(TR.field_image)

  text = g.getName
  textfield.setTextIsSelectable(false)

//  textfield onClick openGroup()
//  imagefield onClick openGroup()
  this onClick0 openGroup()

//  if (PwUuid.Zero == g.getCustomIconUuid) {
    imagefield.setImageResource(
      Database.Icons(g.getIconId.ordinal))
//  }

  private def openGroup(): Unit = {
    BrowseActivity.browse(a, g)
    a.overridePendingTransition(
      android.R.anim.slide_in_left, android.R.anim.slide_out_right)
  }
}

class CustomField(a: AuthorizedActivity) extends StandardFieldView(a, null) {
  override def inflate() = a.getLayoutInflater.inflate(R.layout.custom_field, this, true)
  override lazy val textfield = findView(TR.custom_field)
  lazy val inputlayout = findView(TR.input_layout)
  override def hint = inputlayout.getHint
  override def hint_=(s: String) = {
    inputlayout.setHint(s)
  }
}
