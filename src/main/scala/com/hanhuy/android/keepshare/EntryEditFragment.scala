package com.hanhuy.android.keepshare

import android.app.{AlertDialog, Fragment}
import android.content.Context
import android.os.{Parcel, Parcelable, Bundle}
import android.text.TextUtils
import android.util.{SparseArray, AttributeSet}
import android.view.{View, ViewGroup, LayoutInflater}
import android.widget._

import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.keepassj._

import TypedResource._
import Futures._

import rx.android.schedulers.AndroidSchedulers.mainThread
import rx.android.widget.{OnTextChangeEvent, WidgetObservable}
import rx.lang.scala.JavaConversions._
import rx.lang.scala.{Observable, Subscription, Subject}

import scala.annotation.tailrec
import scala.collection.JavaConverters._

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
  setRetainInstance(true)
  private var model: EntryEditModel = EntryEditModel.blank
  private var baseModel = Option.empty[EntryEditModel]
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup,
                            savedInstanceState: Bundle) = {
    val view = inflater.inflate(TR.layout.entry_edit, container, false)

    val entryId = Option(getArguments) flatMap(a =>
      Option(a.getString(EntryViewActivity.EXTRA_ENTRY_ID)))

    val fieldlist = view.findView(TR.field_list)
    val newfield = view.findView(TR.new_field_button)
    val group = view.findView(TR.edit_group)
    val title = view.findView(TR.edit_title)
    val username = view.findView(TR.edit_username)
    val password = view.findView(TR.edit_password)
    val url = view.findView(TR.edit_url)
    val notes = view.findView(TR.edit_notes)
    val iconObservable: Subject[Int] = Subject()
    val groupObservable: Observable[PwGroup] = Observable.create { obs =>
      group.onGroupChange(g => obs.onNext(g))
      Subscription(group.onGroupChange(null))
    }
    iconObservable.subscribeOn(mainThread).subscribe { icon =>
      model = model.copy(icon = icon)
      title.icon = icon
    }
    groupObservable.subscribeOn(mainThread).subscribe { g =>
      model = model.copy(group = g.getUuid)
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

        group.group = e.getParentGroup
        if (model == EntryEditModel.blank) {
          iconObservable.onNext(Database.Icons(e.getIconId.ordinal))
          title.text = s.ReadSafe(PwDefs.TitleField)
          username.text = s.ReadSafe(PwDefs.UserNameField)
          password.text = s.ReadSafe(PwDefs.PasswordField)
          url.text = s.ReadSafe(PwDefs.UrlField)
          notes.text = s.ReadSafe(PwDefs.NotesField)
          model = model.copy(fields = s.asScala map { e =>
            (e.getKey,e.getValue)
          } filterNot (f => PwDefs.IsStandardField(f._1)) toMap)

          baseModel = Some(model)
        }
      }
      model.fields foreach { case (k, v) =>
        val field = new StandardEditView(activity, null)
        field.hint = k
        field.text = v.ReadString()
        field.password = v.isProtected
        field.iconfield.onClick {
          showFieldOptions("Update", "Update field options", Option(field), v.isProtected) { (n, c) =>
            model = model.copy(fields = (model.fields - k)
              .updated(n.getText.toString, new ProtectedString(c.isChecked, v.ReadString())))
            field.hint = n.getText.toString
          }
        }
        WidgetObservable.text(field.textfield).subscribe((n: OnTextChangeEvent) => {
          model = model.copy(fields = model.fields.updated(k, new ProtectedString(v.isProtected, n.text)))
        })
        fieldlist.addView(field)
      }
    }

    def showFieldOptions[A](action: String, msg: String, field: Option[StandardEditView], password: Boolean)(f: (EditText,CheckBox) => A): Unit = {
      val view = LayoutInflater.from(activity).inflate(
        TR.layout.edit_field_options, null, false)
      val checkbox = view.findView(TR.is_password)
      val name = view.findView(TR.field_name)

      field foreach (f => name.setText(f.hint))
      checkbox.setChecked(password)

      val builder = new AlertDialog.Builder(activity)
        .setPositiveButton(action, () => { f(name, checkbox); () })
        .setNegativeButton("Cancel", null)
        .setTitle(msg)
        .setView(view)

      (field map { f =>
        builder.setNeutralButton("Delete", () => {
          model = model.copy(fields = model.fields - f.hint)
          fieldlist.removeView(f)
          ()
        })
      } getOrElse builder).create().show()
    }

    newfield onClick showFieldOptions("Create", "Create new field", None, false) { (n,c) =>
      if (n.getText.toString.nonEmpty) {
        model = model.copy(fields = model.fields.updated(
          n.getText.toString, new ProtectedString(c.isChecked, "")))

        val field = new StandardEditView(activity, null)
        field.hint = n.getText.toString
        field.password = c.isChecked
        field.iconfield.onClick {
          showFieldOptions("Update", "Update field options", Option(field), field.password) { (n, c) =>
            model = model.copy(fields = (model.fields - field.hint)
              .updated(n.getText.toString, new ProtectedString(c.isChecked, field.text)))
            field.hint = n.getText.toString
          }
        }
        WidgetObservable.text(field.textfield).subscribe((n: OnTextChangeEvent) => {
          model = model.copy(fields = model.fields.updated(field.hint, new ProtectedString(c.isChecked, n.text)))
        })
        fieldlist.addView(field)
      }
      ()
    }

    title.iconfield.onClick {
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
class StandardEditView(c: Context, attrs: AttributeSet) extends StandardFieldView(c, attrs) {
  override def inflate() = LayoutInflater.from(c).inflate(R.layout.edit_field, this, true)
  override lazy val textfield = findView(TR.custom_field)
  def this(c: Context) = this(c, null)

  val a = c.obtainStyledAttributes(attrs, R.styleable.StandardEditView)
  first = a.getBoolean(R.styleable.StandardEditView_first, false)
  long = a.getBoolean(R.styleable.StandardEditView_longform, false)
  password = a.getBoolean(R.styleable.StandardEditView_android_password, false)
  icon = a.getResourceId(R.styleable.StandardEditView_android_icon,
    R.drawable.ic_extension_black_36dp)
  hint = a.getString(R.styleable.StandardEditView_android_hint)
  text = a.getString(R.styleable.StandardEditView_android_text)
  a.recycle()

  setSaveEnabled(true)
  setSaveFromParentEnabled(true)

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

  override def hint_=(s: String) = {
    textfield.setFloatingLabelText(s)
    textfield.setHint(s)
  }
}

object GroupEditView {
  class SavedState(p: Parcelable) extends View.BaseSavedState(p) {
    var uuid: String = _

    override def writeToParcel(dest: Parcel, flags: Int) = {
      TextUtils.writeToParcel(uuid, dest, flags)
    }

    def this(p: Parcel) = {
      this(null: Parcelable)
      uuid = TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(p)
    }

    val CREATOR = new Parcelable.Creator[SavedState] {
      override def newArray(i: Int) = Array.ofDim[SavedState](i)
      override def createFromParcel(parcel: Parcel) = new SavedState(parcel)
    }
  }
}
class GroupEditView(c: Context, attrs: AttributeSet) extends StandardFieldView(c, attrs) {
  override def inflate() = LayoutInflater.from(c).inflate(R.layout.group_field, this, true)
  lazy val imagefield = findView(TR.field_image)

  val a = c.obtainStyledAttributes(attrs, R.styleable.StandardEditView)
  first = a.getBoolean(R.styleable.StandardEditView_first, false)
  a.recycle()

  private[this] var groupId = Option.empty[PwUuid]
  private[this] var _group: PwGroup = _
  def group = _group
  def group_=(g: PwGroup) = {
    _group = groupId flatMap { id =>
      Option(rootGroup(g).FindGroup(id, true)) } getOrElse g

    groupId = None
    text = _group.getName
    //  if (PwUuid.Zero == g.getCustomIconUuid) {
    imagefield.setImageResource(Database.Icons(_group.getIconId.ordinal))
    //  }
  }

  private[this] var groupChangeListener = Option.empty[PwGroup => Any]
  def onGroupChange[A](f: PwGroup => A): Unit = {
    groupChangeListener = Option(f)
  }

  textfield.setTextIsSelectable(false)

  setSaveEnabled(true)
  setSaveFromParentEnabled(true)

  this onClick showGroupsList()

  @tailrec
  final def rootGroup(g: PwGroup): PwGroup =
    if (g.getParentGroup == null) g else rootGroup(g.getParentGroup)

  override def onSaveInstanceState() = {
    val ss = new GroupEditView.SavedState(super.onSaveInstanceState())
    ss.uuid = group.getUuid.ToHexString
    ss
  }
  override def onRestoreInstanceState(state: Parcelable) = {
    state match {
      case s: GroupEditView.SavedState =>
        super.onRestoreInstanceState(s.getSuperState)
        groupId = Some(new PwUuid(KeyManager.bytes(s.uuid)))
        Option(group) foreach { group = _ }
    }
  }
  def showGroupsList(): Unit = {
    import BrowseActivity.groupSort
    val panel = new ListView(c)
    val dm = getResources.getDisplayMetrics
    val density = dm.density
    val popup = new PopupWindow(panel)
    val root = rootGroup(group)
    val groups = root +: root.GetGroups(true).asScala.toVector.sorted
    val adapter = new BaseAdapter {
      override def getItemId(i: Int) = Database.getId(groups(i))
      override def getCount = groups.size
      override def getItem(i: Int) = groups(i)

      override def getView(i: Int, v: View, viewGroup: ViewGroup) = {
        val b = if (v == null) {
          LayoutInflater.from(c).inflate(TR.layout.browse_pwgroup_item, viewGroup, false)
        } else v.asInstanceOf[ViewGroup]
        val icon = b.findView(TR.entry_image)
        icon.setImageResource(Database.Icons(getItem(i).getIconId.ordinal))
        b.findView(TR.name).setText(getItem(i).getName)
        b.onClick {
          group = getItem(i)
          groupChangeListener foreach (_(group))
          popup.dismiss()
        }
        b
      }
    }
    panel.setAdapter(adapter)
    popup.setBackgroundDrawable(getResources.getDrawable(android.R.drawable.picture_frame))
    popup.setHeight((density * 5 * 48).toInt)
    popup.setWidth(dm.widthPixels - (density * 64).toInt)
    popup.setFocusable(true)
    popup.showAsDropDown(this)
  }
}

object EntryEditModel {
  def blank = EntryEditModel(R.drawable.i00_password,
    None, None, None, None, None, PwUuid.Zero, Map.empty)
}
case class EntryEditModel(icon: Int, title: Option[String],
                          username: Option[String], password: Option[String],
                          url: Option[String], notes: Option[String], group: PwUuid,
                          fields: Map[String,ProtectedString])
