package com.hanhuy.android.keepshare

import android.app.{Activity, AlertDialog, Fragment}
import android.content.Context
import android.os.{Parcel, Parcelable, Bundle}
import android.text.{InputType, TextUtils}
import android.util.{SparseArray, AttributeSet}
import android.view.{View, ViewGroup, LayoutInflater}
import android.widget._
import com.hanhuy.android.common._

import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.android.keepshare.EntryViewActivity.EntryCreateData
import com.hanhuy.keepassj._

import TypedResource._
import Futures._

import scala.annotation.tailrec
import scala.language.postfixOps
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
  def create(parent: Option[String], data: Option[EntryViewActivity.EntryCreateData] = None) = {
    val f = new EntryEditFragment
    val b = new Bundle
    f.setArguments(b)
    b.putBoolean(EntryViewActivity.EXTRA_CREATE, true)
    b.putString(BrowseActivity.EXTRA_GROUP_ID, parent.orNull)
    b.putSerializable(EntryViewActivity.EXTRA_CREATE_DATA, data.orNull)
    f
  }
  def iconPicker[A](activity: Activity, anchor: View, onClick: Int => A): Unit = {
    val panel = new GridView(activity)
    val dm = activity.getResources.getDisplayMetrics
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
        b.onClick0 {
          onClick(getItem(i))
          popup.dismiss()
        }
        b
      }
    }
    panel.setAdapter(adapter)
    panel.setColumnWidth((density * 48).toInt)
    popup.setBackgroundDrawable(activity.getResources.getDrawable(android.R.drawable.picture_frame))
    popup.setHeight((density * 5 * 48).toInt)
    popup.setWidth(dm.widthPixels - (density * 64).toInt)
    popup.setFocusable(true)
    popup.showAsDropDown(anchor)
  }

  private lazy val recycleBinId = Database.recycleBinId getOrElse PwUuid.Zero
  @tailrec
  final def inRecycleBin(g: PwGroup): Boolean =
    if (g.getUuid == recycleBinId) true
    else if (g.getParentGroup == null) false
    else inRecycleBin(g.getParentGroup)
}

class EntryEditFragment extends AuthorizedFragment {
  setRetainInstance(true)
  var model: EntryEditModel = EntryEditModel.blank
  var baseModel = Option.empty[EntryEditModel]

  def password = getView.getTag(R.layout.entry_edit).asInstanceOf[TypedViewHolder.entry_edit].edit_password

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup,
                            savedInstanceState: Bundle) = {
    val views: TypedViewHolder.entry_edit = TypedViewHolder.inflate(inflater, TR.layout.entry_edit, container, false)
    views.rootView.setTag(R.layout.entry_edit, views)

    val entryId = Option(getArguments) flatMap(a =>
      Option(a.getString(EntryViewActivity.EXTRA_ENTRY_ID)))
    val createData = Option(getArguments) flatMap(a =>
      Option(a.getSerializable(EntryViewActivity.EXTRA_CREATE_DATA)
        .asInstanceOf[EntryCreateData]))
    val groupId = getArguments.? flatMap(_.getString(BrowseActivity.EXTRA_GROUP_ID).?)

    val iconObservable: Var[Int] = Var(views.edit_title.icon)
    iconObservable.subscribe { icon =>
      model = model.copy(icon = icon)
      views.edit_title.icon = icon
    }
    views.edit_group.groupChange.subscribe { g =>
      model = model.copy(group = g.getUuid)
    }
    views.edit_title.textfield.onTextChanged(s =>
      model = model.copy(title = s.? map (_.toString))
    )
    views.edit_username.textfield.onTextChanged(s =>
      model = model.copy(username = s.? map (_.toString))
    )
    views.edit_password.textfield.onTextChanged(s =>
      model = model.copy(password = s.? map (_.toString))
    )
    views.edit_url.textfield.onTextChanged(s =>
      model = model.copy(url = s.? map (_.toString))
    )
    views.edit_notes.textfield.onTextChanged(s =>
      model = model.copy(notes = s.? map (_.toString))
    )

    activity.database map { db =>
      db -> (entryId map { id =>
        val uuid = new PwUuid(KeyManager.bytes(id))
        db.getRootGroup.FindEntry(uuid, true)
      })
    } onSuccessMain { case (db,entry) =>
      entry foreach { e =>
        val s = e.getStrings

        if (model == EntryEditModel.blank) {
          iconObservable() = Database.Icons(e.getIconId.ordinal)
          views.edit_title.text = s.ReadSafe(PwDefs.TitleField)
          views.edit_username.text = s.ReadSafe(PwDefs.UserNameField)
          views.edit_password.text = s.ReadSafe(PwDefs.PasswordField)
          views.edit_url.text = s.ReadSafe(PwDefs.UrlField)
          views.edit_notes.text = s.ReadSafe(PwDefs.NotesField)
          model = model.copy(fields = s.asScala map { e =>
            (e.getKey,e.getValue)
          } filterNot (f => PwDefs.IsStandardField(f._1)) toMap)

          views.delete.onClick0 {
            if (EntryEditFragment.inRecycleBin(e.getParentGroup)) {
              val t = getString(R.string.delete_name, s.ReadSafe(PwDefs.TitleField))
              new AlertDialog.Builder(activity)
                .setTitle(t)
                .setMessage(R.string.delete_permanently)
                .setPositiveButton(android.R.string.ok, () => {
                  Database.delete(e)
                  DatabaseSaveService.save()
                  activity.finish()
                })
                .setNegativeButton(android.R.string.cancel, null)
                .show()
            } else {
              val group = e.getParentGroup
              Database.delete(e)
              DatabaseSaveService.save()
              BrowseActivity.SnackbarSender.enqueue(getString(R.string.delete_entry, s.ReadSafe(PwDefs.TitleField)), getString(R.string.undo)) { a =>
                Database.recycleBin.foreach(_.getEntries.Remove(e))
                group.getEntries.Add(e)
                e.setParentGroup(group)
                e.Touch(true, false)
                DatabaseSaveService.save()
                a.navigateTo(Option(group.getUuid))
              }
              activity.finish()
            }
          }

          model = model.copy(group = e.getParentGroup.getUuid)
          baseModel = Some(model)
        }
        views.edit_group.group = e.getParentGroup
      }

      createData foreach { d =>
        if (model == EntryEditModel.blank) {
          views.edit_title.text = d.title.getOrElse("")
          views.edit_username.text = d.username.getOrElse("")
          views.edit_password.text = d.password.getOrElse("")
          views.edit_url.text = d.url.getOrElse("")
          views.edit_notes.text = d.notes.getOrElse("")
          model = model.copy(fields = d.fields map { case ((pw, k, v)) =>
              k -> new ProtectedString(pw, v)
          } toMap)
        }
      }
      if (getArguments.?.exists(_.getBoolean(EntryViewActivity.EXTRA_CREATE, false))) {
        groupId map { id =>
          val uuid = new PwUuid(KeyManager.bytes(id))
          db.getRootGroup.FindGroup(uuid, true)
        } orElse db.getRootGroup.? foreach { grp =>
          iconObservable() = Database.Icons(grp.getIconId.ordinal)
          views.edit_group.group = grp
          views.delete.setVisibility(View.GONE)
        }
      }
      model.fields foreach { case (k, v) =>
        val field = new StandardEditView(activity, null)
        field.hint = k
        field.text = v.ReadString()
        field.password = v.isProtected
        field.iconfield.onClick0 { handleFieldUpdate(field, k) }
        field.textfield.onTextChanged(s =>
          model = model.copy(fields = model.fields.updated(k, new ProtectedString(v.isProtected, s.toString)))
        )
        views.field_list.addView(field)
      }
    }

    def handleFieldUpdate(field: StandardEditView, name: String): Unit = {
      showFieldOptions("Update", "Update field options", Option(field), field.password) { (n, c) =>
        model = model.copy(fields = (model.fields - name)
          .updated(n.getText.toString, new ProtectedString(c.isChecked, field.text)))
        field.hint = n.getText.toString
        field.password = c.isChecked
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
          model = model.copy(fields = model.fields - f.hint.toString)
          views.field_list.removeView(f)
          ()
        })
      } getOrElse builder).create().show()
    }

    views.new_field_button onClick0 showFieldOptions("Create", "Create new field", None, false) { (n,c) =>
      if (n.getText.toString.nonEmpty) {
        model = model.copy(fields = model.fields.updated(
          n.getText.toString, new ProtectedString(c.isChecked, "")))

        val field = new StandardEditView(activity, null)
        field.hint = n.getText.toString
        field.password = c.isChecked
        field.iconfield.onClick0 { handleFieldUpdate(field, field.hint.toString) }
        field.textfield.onTextChanged(s =>
          model = model.copy(fields = model.fields.updated(field.hint.toString, new ProtectedString(c.isChecked, s.toString)))
        )
        views.field_list.addView(field)
        UiBus.post {
          views.scroll.scrollTo(0, views.field_list.getHeight)
        }
      }
      ()
    }

    views.edit_title.iconfield.onClick0 {
      EntryEditFragment.iconPicker(activity, views.edit_title.iconfield, iconObservable.update)
    }
    views.edit_password.iconfield.onClick0 {
      new PasswordGeneratorFragment().show(getFragmentManager, "password-generator")
    }
    views.rootView
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
      text = TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(p).toString
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
  lazy val inputlayout = findView(TR.input_layout)
  def this(c: Context) = this(c, null)

  val a = c.obtainStyledAttributes(attrs, R.styleable.StandardEditView)
  first = a.getBoolean(R.styleable.StandardEditView_first, false)
  // vis password workaround for swiftkey stupidity
  textfield.setInputType(a.getInt(R.styleable.StandardEditView_android_inputType,
    InputType.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD | InputType.TYPE_TEXT_VARIATION_NORMAL))
  long = a.getBoolean(R.styleable.StandardEditView_longform, false)
  password = a.getBoolean(R.styleable.StandardEditView_android_password, false)
  icon = a.getResourceId(R.styleable.StandardEditView_android_icon,
    R.drawable.ic_extension_black_36dp)
  hint = a.getString(R.styleable.StandardEditView_android_hint)
  text = a.getString(R.styleable.StandardEditView_android_text)
  a.recycle()

  setSaveEnabled(true)
  setSaveFromParentEnabled(true)

  def errors = inputlayout.isErrorEnabled
  def errors_=(b: Boolean) = inputlayout.setErrorEnabled(b)
  def error = inputlayout.getError
  def error_=(s: CharSequence) = inputlayout.setError(s)

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

  override def hint = inputlayout.getHint
  override def hint_=(s: String) = inputlayout.setHint(s)
}

object GroupEditView {
  class SavedState(p: Parcelable) extends View.BaseSavedState(p) {
    var uuid: String = _

    override def writeToParcel(dest: Parcel, flags: Int) = {
      TextUtils.writeToParcel(uuid, dest, flags)
    }

    def this(p: Parcel) = {
      this(null: Parcelable)
      uuid = TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(p).toString
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
    groupSubject() = _group
    //  if (PwUuid.Zero == g.getCustomIconUuid) {
    imagefield.setImageResource(Database.Icons(_group.getIconId.ordinal))
    //  }
  }

  private[this] val groupSubject = Var(_group)
  val groupChange: Obs[PwGroup] = groupSubject

  textfield.setTextIsSelectable(false)

  setSaveEnabled(true)
  setSaveFromParentEnabled(true)

  this onClick0 showGroupsList()

  @tailrec
  final def rootGroup(g: PwGroup): PwGroup =
    if (g.getParentGroup == null) g else rootGroup(g.getParentGroup)

  override def onSaveInstanceState() = {
    val ss = new GroupEditView.SavedState(super.onSaveInstanceState())
    if (group != null)
      ss.uuid = group.getUuid.ToHexString
    ss
  }
  override def onRestoreInstanceState(state: Parcelable) = {
    state match {
      case s: GroupEditView.SavedState =>
        super.onRestoreInstanceState(s.getSuperState)
        if (s.uuid != null) {
          groupId = Some(new PwUuid(KeyManager.bytes(s.uuid)))
          Option(group) foreach { group = _ }
        }
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
          val l = LayoutInflater.from(c).inflate(TR.layout.browse_pwgroup_item, viewGroup, false)
          l.findView(TR.folder_image).setVisibility(View.GONE)
          l
        } else v.asInstanceOf[ViewGroup]
        val icon = b.findView(TR.entry_image)
        icon.setImageResource(Database.Icons(getItem(i).getIconId.ordinal))
        b.findView(TR.name).setText(getItem(i).getName)
        b.onClick0 {
          group = getItem(i)
          groupSubject() = group
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
  def blank = EntryEditModel(R.mipmap.i00_password,
    None, None, None, None, None, PwUuid.Zero, Map.empty)
}
case class EntryEditModel(icon: Int, title: Option[String],
                          username: Option[String], password: Option[String],
                          url: Option[String], notes: Option[String], group: PwUuid,
                          fields: Map[String,ProtectedString]) {

  def needsBackup(other: EntryEditModel): Boolean = {
    val pairs = this.productIterator.toList zip other.productIterator.toList
    // don't need a backup if the icon or parent group changes
    val diffs = pairs.zipWithIndex.collect {
      case ((a,b),i) if a != b && !Set(0,6)(i) => i
    }
    diffs.nonEmpty
  }
}
