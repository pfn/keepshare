package com.hanhuy.android.keepshare

import java.net.URI

import android.app.Activity
import android.content.{Intent, Context}
import android.database.Cursor
import android.os.Bundle
import android.support.v7.app.ActionBarActivity
import android.view.{View, ViewGroup}
import android.widget.{BaseAdapter, CursorAdapter}
import com.hanhuy.android.common.{ServiceBus, UiBus}

import com.hanhuy.android.common.AndroidConversions._

import TypedResource._
import com.hanhuy.keepassj.PwDefs

/**
 * @author pfnguyen
 */
class AccessibilitySearchActivity extends Activity with TypedViewHolder {

  val _implicit: RichActivity = this
  import _implicit._

  lazy val settings = new Settings(this)

  def init() {
    setContentView(R.layout.accessibility_search_activity)
    val extras = getIntent.getExtras
    val (windowId, packageName, url) = {
      (extras.getInt(AccessibilityService.EXTRA_WINDOWID, -1),
        extras.getString(AccessibilityService.EXTRA_PACKAGE),
        extras.getString(AccessibilityService.EXTRA_URI))
    }

    findView(TR.cancel) onClick {
      finish()
    }

    findView(TR.subject).setText(url)

    async {
      val uri = new URI(url)
      val uris = ShareActivity.goUp(uri) map (_.toString)
      val subhosts = ShareActivity.subhosts(uri.getHost)
      val results = ShareActivity.queryDatabase(this, settings, uris ++ subhosts)

      results map { result =>

        UiBus.post {
          findView(TR.flipper).showNext()
          val list = findView(TR.list)
          list.setEmptyView(findView(TR.empty))

            val adapter = new BaseAdapter {

              override def getCount = result.size

              override def getItemId(i: Int) = Database.getId(getItem(i))

              override def getView(i: Int, view: View, c: ViewGroup) = {
                val row = Option(view) getOrElse getLayoutInflater.inflate(R.layout.pwitem, c, false)
                row.findView(TR.name).setText(
                  Database.getField(getItem(i), PwDefs.TitleField) orNull)
                row.findView(TR.username).setText(
                  Database.getField(getItem(i), PwDefs.UserNameField) orNull)
                row
              }

              override def getItem(i: Int) = result(i)

            }
            val onClickHandler = { pos: Int =>
              val cursor = adapter.getItem(pos).asInstanceOf[Cursor]
              findView(TR.continu).setEnabled(true)
              findView(TR.continu).onClick {
                ServiceBus.send(AccessibilityFillEvent(
                  packageName, windowId, url,
                  Database.getField(result(pos), PwDefs.UserNameField) orNull,
                  Database.getField(result(pos), PwDefs.PasswordField) orNull))
                finish()
              }
            }
            list.onItemClick(onClickHandler)
            list.setAdapter(adapter)
            if (adapter.getCount < 2) {
              findView(TR.select_prompt).setVisibility(View.GONE)
              if (adapter.getCount == 1) {
                list.setItemChecked(0, true)
                onClickHandler(0)
                findView(TR.continu).setEnabled(true)
              }
            }
        }
      }
    }
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    if (settings.get(Settings.FIRST_RUN)) {
      startActivityForResult(
        SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else {
      if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty)
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
      else
        init()
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    val success = request match {
      case RequestCodes.REQUEST_SETUP => result == Activity.RESULT_OK
      case RequestCodes.REQUEST_PIN => result == Activity.RESULT_OK
    }
    if (success) init() else finish()
  }
}
