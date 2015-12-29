package com.hanhuy.android.keepshare

import android.app.{Activity, AlertDialog, Fragment}
import android.content.Context
import android.os.{Parcel, Parcelable, Bundle}
import android.text.{InputType, TextUtils}
import android.util.{SparseArray, AttributeSet}
import android.view.{View, ViewGroup, LayoutInflater}
import android.widget._
import com.hanhuy.android.common.{Futures, UiBus}

import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.keepassj._

import TypedResource._
import Futures._
import Rx._

import rx.android.schedulers.AndroidSchedulers.mainThread
import rx.android.widget.WidgetObservable
import rx.lang.scala.{Observable, Subscription, Subject}

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
  def create(parent: String) = {
    val f = new EntryEditFragment
    val b = new Bundle
    f.setArguments(b)
    b.putString(BrowseActivity.EXTRA_GROUP_ID, parent)
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

  override def onCreateView(inflater: LayoutInflater, container: ViewGroup,
                            savedInstanceState: Bundle) = {
    val view = inflater.inflate(TR.layout.entry_edit, container, false)

    val entryId = Option(getArguments) flatMap(a =>
      Option(a.getString(EntryViewActivity.EXTRA_ENTRY_ID)))
    val groupId = Option(getArguments) flatMap(a =>
      Option(a.getString(BrowseActivity.EXTRA_GROUP_ID)))

    val fieldlist = view.findView(TR.field_list)
    val newfield = view.findView(TR.new_field_button)
    val group = view.findView(TR.edit_group)
    val title = view.findView(TR.edit_title)
    val username = view.findView(TR.edit_username)
    val password = view.findView(TR.edit_password)
    val url = view.findView(TR.edit_url)
    val notes = view.findView(TR.edit_notes)
    val iconObservable: Subject[Int] = Subject()
    iconObservable.observeOn(mainThread).subscribe { icon =>
      model = model.copy(icon = icon)
      title.icon = icon
    }
    group.groupChange.observeOn(mainThread).subscribe { g =>
      model = model.copy(group = g.getUuid)
    }
    WidgetObservable.text(title.textfield).asScala.subscribe(n =>
      model = model.copy(title = Option(n.text) map (_.toString))
    )
    WidgetObservable.text(username.textfield).asScala.subscribe(n =>
      model = model.copy(username = Option(n.text) map (_.toString))
    )
    WidgetObservable.text(password.textfield).asScala.subscribe(n =>
      model = model.copy(password = Option(n.text) map (_.toString))
    )
    WidgetObservable.text(url.textfield).asScala.subscribe(n =>
      model = model.copy(url = Option(n.text) map (_.toString))
    )
    WidgetObservable.text(notes.textfield).asScala.subscribe(n =>
      model = model.copy(notes = Option(n.text) map (_.toString))
    )

    activity.database map { db =>
      groupId map { id =>
        val uuid = new PwUuid(KeyManager.bytes(id))
        db.getRootGroup.FindGroup(uuid, true)
      }
    } onSuccessMain { case g =>
      g foreach { grp =>
        iconObservable.onNext(Database.Icons(grp.getIconId.ordinal))
        group.group = grp
        view.findView(TR.delete).setVisibility(View.GONE)
      }
    }
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

          view.findView(TR.delete).onClick0 {
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

          baseModel = Some(model)
        }
      }
      model.fields foreach { case (k, v) =>
        val field = new StandardEditView(activity, null)
        field.hint = k
        field.text = v.ReadString()
        field.password = v.isProtected
        field.iconfield.onClick0 { handleFieldUpdate(field, k) }
        WidgetObservable.text(field.textfield).asScala.subscribe(n =>
          model = model.copy(fields = model.fields.updated(k, new ProtectedString(v.isProtected, n.text.toString)))
        )
        fieldlist.addView(field)
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
          fieldlist.removeView(f)
          ()
        })
      } getOrElse builder).create().show()
    }

    newfield onClick0 showFieldOptions("Create", "Create new field", None, false) { (n,c) =>
      if (n.getText.toString.nonEmpty) {
        model = model.copy(fields = model.fields.updated(
          n.getText.toString, new ProtectedString(c.isChecked, "")))

        val field = new StandardEditView(activity, null)
        field.hint = n.getText.toString
        field.password = c.isChecked
        field.iconfield.onClick0 { handleFieldUpdate(field, field.hint.toString) }
        WidgetObservable.text(field.textfield).asScala.subscribe(n =>
          model = model.copy(fields = model.fields.updated(field.hint.toString, new ProtectedString(c.isChecked, n.text.toString)))
        )
        fieldlist.addView(field)
        UiBus.post {
          view.findView(TR.scroll).scrollTo(0, fieldlist.getHeight)
        }
      }
      ()
    }

    title.iconfield.onClick0 {
      EntryEditFragment.iconPicker(activity, title.iconfield, iconObservable.onNext)
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
    inputlayout.setHint(s)
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
    groupSubject.onNext(_group)
    //  if (PwUuid.Zero == g.getCustomIconUuid) {
    imagefield.setImageResource(Database.Icons(_group.getIconId.ordinal))
    //  }
  }

  private[this] val groupSubject = Subject[PwGroup]()
  val groupChange: Observable[PwGroup] = groupSubject

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
          groupSubject.onNext(group)
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
