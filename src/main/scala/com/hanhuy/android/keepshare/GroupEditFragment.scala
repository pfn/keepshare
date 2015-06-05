package com.hanhuy.android.keepshare

import android.app.AlertDialog
import android.os.Bundle
import android.view.{LayoutInflater, View, ViewGroup}
import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.android.keepshare.Futures._
import com.hanhuy.android.keepshare.TypedResource._
import com.hanhuy.keepassj._
import rx.android.schedulers.AndroidSchedulers.mainThread
import rx.android.widget.WidgetObservable
import rx.lang.scala.{Observable, Subject, Subscription}

import scala.collection.JavaConverters._

import Rx._


object GroupEditFragment {
  def edit(group: PwGroup) = {
    val f = new GroupEditFragment
    val b = new Bundle
    b.putString(BrowseActivity.EXTRA_GROUP_ID, group.getUuid.ToHexString)
    f.setArguments(b)
    f
  }
  def create(parent: PwGroup) = {
    val f = new GroupEditFragment
    val b = new Bundle
    f.setArguments(b)
    b.putString(BrowseActivity.EXTRA_GROUP_ID, parent.getUuid.ToHexString)
    b.putBoolean(EntryViewActivity.EXTRA_CREATE, true)
    f
  }
}
class GroupEditFragment extends AuthorizedFragment {
  setRetainInstance(true)
  var model: GroupEditModel = GroupEditModel.blank
  var baseModel = Option.empty[GroupEditModel]
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup,
                            savedInstanceState: Bundle) = {
    val view = inflater.inflate(TR.layout.group_edit, container, false)

    val groupId = Option(getArguments) flatMap(a =>
      Option(a.getString(BrowseActivity.EXTRA_GROUP_ID)))

    val creating = Option(getArguments) map (
      _.getBoolean(EntryViewActivity.EXTRA_CREATE, false)) exists identity

    val group = view.findView(TR.edit_group)
    val title = view.findView(TR.edit_title)
    val notes = view.findView(TR.edit_notes)
    val iconObservable: Subject[Int] = Subject()
    val groupObservable: Observable[PwGroup] = Observable.create { obs =>
      group.onGroupChange(g => obs.onNext(g))
      Subscription(group.onGroupChange(null))
    }
    iconObservable.observeOn(mainThread).subscribe { icon =>
      model = model.copy(icon = icon)
      title.icon = icon
    }
    groupObservable.observeOn(mainThread).subscribe { g =>
      model = model.copy(group = g.getUuid)
    }
    WidgetObservable.text(title.textfield).asScala.subscribe(n =>
      model = model.copy(title = Option(n.text) map (_.toString))
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
        if (creating) {
          group.group = grp
          view.findView(TR.delete).setVisibility(View.GONE)
          iconObservable.onNext(Database.Icons(grp.getIconId.ordinal))
        } else {
          if (grp.getParentGroup == null) {
            group.setVisibility(View.GONE)
            title.first = true
            view.findView(TR.delete).setVisibility(View.GONE)
          } else
            group.group = grp.getParentGroup
          if (model == GroupEditModel.blank) {
            iconObservable.onNext(Database.Icons(grp.getIconId.ordinal))
            title.text = grp.getName
            notes.text = grp.getNotes

            view.findView(TR.delete).onClick0 {
              val t = getString(R.string.delete_name, grp.getName)
              val msg = if (EntryEditFragment.inRecycleBin(grp.getParentGroup))
                R.string.delete_permanently else R.string.move_to_recycle

              new AlertDialog.Builder(activity)
                .setTitle(t)
                .setMessage(msg)
                .setPositiveButton(android.R.string.ok, () => {
                  Database.delete(grp)
                  DatabaseSaveService.save()
                  activity.getFragmentManager.popBackStack()
                })
                .setNegativeButton(android.R.string.cancel, null)
                .show()
            }

            baseModel = Some(model)
          }
        }
      }
    }

    title.iconfield.onClick0 {
      EntryEditFragment.iconPicker(activity, title.iconfield, iconObservable.onNext)
    }
    view
  }
}

object GroupEditModel {
  def blank = GroupEditModel(R.drawable.i00_password,
    None, None, PwUuid.Zero)
}
case class GroupEditModel(icon: Int, title: Option[String],
                          notes: Option[String], group: PwUuid)
