package com.hanhuy.android.keepshare

import android.app.Fragment
import android.content.Context
import android.os.{Parcel, Parcelable, Bundle}
import android.text.TextUtils
import android.text.method.PasswordTransformationMethod
import android.util.{SparseArray, AttributeSet}
import android.view.{View, Gravity, ViewGroup, LayoutInflater}
import android.widget._

import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.keepassj.{PwDefs, PwUuid, PwEntry}

import TypedResource._
import Futures._

import rx.android.schedulers.AndroidSchedulers.mainThread
import rx.android.widget.{OnTextChangeEvent, WidgetObservable}
import rx.lang.scala.JavaConversions._
import rx.lang.scala.Subject

class AuthorizedFragment extends Fragment {
  def activity = getActivity.asInstanceOf[AuthorizedActivity]
}

/**
 * @author pfnguyen
 */
object EntryEditFragment {
  def edit(entry: PwEntry) = {
    val f = new EntryEditFragment
    val b = new Bundle
    b.putString(EntryViewActivity.EXTRA_ENTRY_ID, entry.getUuid.ToHexString)
    f.setArguments(b)
    f
  }
}
class EntryEditFragment extends AuthorizedFragment {
  private var model: EntryEditModel = EntryEditModel.blank
  private var baseModel = Option.empty[EntryEditModel]
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup,
                            savedInstanceState: Bundle) = {
    val view = inflater.inflate(TR.layout.entry_edit, container, false)

    val entryId = Option(getArguments) flatMap(a =>
      Option(a.getString(EntryViewActivity.EXTRA_ENTRY_ID)))

    val title = view.findView(TR.edit_title)
    val username = view.findView(TR.edit_username)
    val password = view.findView(TR.edit_password)
    val url = view.findView(TR.edit_url)
    val notes = view.findView(TR.edit_notes)
    val iconObservable: Subject[Int] = Subject()
    iconObservable.subscribeOn(mainThread).subscribe { icon =>
      model = model.copy(icon = icon)
      title.icon = icon
    }
    WidgetObservable.text(title.textfield).subscribe((n: OnTextChangeEvent) => {
      model = model.copy(title = Option(n.text))
    })
    WidgetObservable.text(username.textfield).subscribe((n: OnTextChangeEvent) => {
      model = model.copy(username = Option(n.text))
    })
    WidgetObservable.text(password.textfield).subscribe((n: OnTextChangeEvent) => {
      model = model.copy(password = Option(n.text))
    })
    WidgetObservable.text(url.textfield).subscribe((n: OnTextChangeEvent) => {
      model = model.copy(url = Option(n.text))
    })
    WidgetObservable.text(notes.textfield).subscribe((n: OnTextChangeEvent) => {
      model = model.copy(notes = Option(n.text))
    })

    activity.database map { db =>
      entryId map { id =>
        val uuid = new PwUuid(KeyManager.bytes(id))
        db.getRootGroup.FindEntry(uuid, true)
      }
    } onSuccessMain { case entry =>
      entry foreach { e =>
        val s = e.getStrings

        iconObservable.onNext(Database.Icons(e.getIconId.ordinal))
        title.text    = s.ReadSafe(PwDefs.TitleField)
        username.text = s.ReadSafe(PwDefs.UserNameField)
        password.text = s.ReadSafe(PwDefs.PasswordField)
        url.text      = s.ReadSafe(PwDefs.UrlField)
        notes.text    = s.ReadSafe(PwDefs.NotesField)
        baseModel     = Some(model)
      }
    }

    view.findView(TR.edit_title).iconfield.onClick {
      val panel = new GridView(activity)
      val dm = getResources.getDisplayMetrics
      val density = dm.density
      panel.setNumColumns(GridView.AUTO_FIT)
      val popup = new PopupWindow(panel)
      val adapter = new BaseAdapter {
        override def getItemId(i: Int) = getItem(i).toLong
        override def getCount = Database.Icons.size
        override def getItem(i: Int) = Database.Icons(i): java.lang.Integer

        override def getView(i: Int, v: View, viewGroup: ViewGroup) = {
          val b = if (v == null) {
            LayoutInflater.from(activity).inflate(
              TR.layout.entry_icon_picker_item, viewGroup, false)
          } else v.asInstanceOf[ImageButton]
          b.setImageResource(getItem(i))
          b.onClick {
            iconObservable.onNext(getItem(i))
            popup.dismiss()
          }
          b
        }
      }
      panel.setAdapter(adapter)
      panel.setColumnWidth((density * 48).toInt)
      popup.setBackgroundDrawable(getResources.getDrawable(android.R.drawable.picture_frame))
      popup.setHeight((density * 5 * 48).toInt)
      popup.setWidth(dm.widthPixels - (density * 64).toInt)
      popup.setFocusable(true)
      popup.showAsDropDown(title.iconfield)
    }
    view
  }
}

object StandardEditView {
  class SavedState(p: Parcelable) extends View.BaseSavedState(p) {
    var text: String = _
    var icon: Int = 0

    override def writeToParcel(dest: Parcel, flags: Int) = {
      dest.writeInt(icon)
      TextUtils.writeToParcel(text, dest, flags)
    }

    def this(p: Parcel) = {
      this(null: Parcelable)
      icon = p.readInt
      text = TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(p)
    }

    val CREATOR = new Parcelable.Creator[SavedState] {
      override def newArray(i: Int) = Array.ofDim[SavedState](i)
      override def createFromParcel(parcel: Parcel) = new SavedState(parcel)
    }
  }
}
class StandardEditView(c: Context, attrs: AttributeSet) extends FrameLayout(c, attrs) with TypedView {
  private[this] var _icon: Int = R.drawable.ic_extension_black_36dp
  def this(c: Context) = this(c, null)

  inflate()
  val a = c.obtainStyledAttributes(attrs, R.styleable.StandardEditView)
  first = a.getBoolean(R.styleable.StandardEditView_first, false)
  long = a.getBoolean(R.styleable.StandardEditView_longform, false)
  password = a.getBoolean(R.styleable.StandardEditView_android_password, false)
  icon = a.getResourceId(R.styleable.StandardEditView_android_icon,
    R.drawable.ic_extension_black_36dp)
  hint = a.getString(R.styleable.StandardEditView_android_hint)
  text = a.getString(R.styleable.StandardEditView_android_text)
  a.recycle()

  setMinimumHeight((getResources.getDisplayMetrics.density * 48).toInt)

  override def onSaveInstanceState() = {
    val ss = new StandardEditView.SavedState(super.onSaveInstanceState())
    ss.text = text
    ss.icon = icon
    ss
  }
  override def onRestoreInstanceState(state: Parcelable) = {
    state match {
      case s: StandardEditView.SavedState =>
        super.onRestoreInstanceState(s.getSuperState)
        text = s.text
        icon = s.icon
    }
  }
  override def dispatchRestoreInstanceState(container: SparseArray[Parcelable]) =
    dispatchThawSelfOnly(container)
  override def dispatchSaveInstanceState(container: SparseArray[Parcelable]) =
    dispatchFreezeSelfOnly(container)

  lazy val textfield = findView(TR.custom_field)
  lazy val iconfield = findView(TR.field_icon)
  lazy val divider = findView(TR.field_divider)
  lazy val visibility = {
    val cb = findView(TR.field_visibility)
    cb.onCheckedChanged(b =>
      textfield.setTransformationMethod(if (b)
        null else PasswordTransformationMethod.getInstance)
    )
    cb
  }

  def inflate() = LayoutInflater.from(c).inflate(R.layout.edit_field, this, true)

  private[this] var _long: Boolean = false
  def long: Boolean = _long
  def long_=(value: Boolean): Unit = {
    if (value) {
      textfield.setSingleLine(false)
      textfield.setMinLines(8)
      textfield.setGravity(Gravity.TOP | Gravity.LEFT)
      textfield.getLayoutParams.asInstanceOf[FrameLayout.LayoutParams].gravity = Gravity.TOP | Gravity.LEFT
      iconfield.getLayoutParams.asInstanceOf[FrameLayout.LayoutParams].gravity = Gravity.TOP | Gravity.LEFT
    } else {
      textfield.setSingleLine(true)
      textfield.setMinLines(1)
      textfield.setGravity(Gravity.CENTER | Gravity.LEFT)
      textfield.getLayoutParams.asInstanceOf[FrameLayout.LayoutParams].gravity = Gravity.CENTER | Gravity.LEFT
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

  def icon: Int = _icon
  def icon_=(value: Int): Unit = {
    iconfield.setImageResource(value)
    _icon = value
  }

  def hint = textfield.getHint
  def hint_=(s: String) = {
    textfield.setFloatingLabelText(s)
    textfield.setHint(s)
  }
  def text_=(s: String) = textfield.setText(s)
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

object EntryEditModel {
  def blank = EntryEditModel(R.drawable.i00_password,
    None, None, None, None, None, Map.empty)
}
case class EntryEditModel(icon: Int, title: Option[String],
                          username: Option[String], password: Option[String],
                          url: Option[String], notes: Option[String],
                          fields: Map[String,String])
