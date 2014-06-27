package com.hanhuy.android.keepshare

import java.net.URI

import android.app.Activity
import android.content.{Intent, Context}
import android.database.Cursor
import android.os.Bundle
import android.view.{View, ViewGroup}
import android.widget.CursorAdapter
import com.hanhuy.android.common.{ServiceBus, UiBus, RichActivity}
import com.keepassdroid.provider.Contract

import com.hanhuy.android.common.AndroidConversions._

import TypedResource._

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
      val cursor = ShareActivity.queryDatabase(this, settings, uris ++ subhosts)

      if (cursor != null) {

        UiBus.post {
          findView(TR.flipper).showNext()
          val list = findView(TR.list)
          list.setEmptyView(findView(TR.empty))

          if (!cursor.isClosed) {

            val adapter = new CursorAdapter(this, cursor, false) {
              def newView(p1: Context, cursor: Cursor, c: ViewGroup) = {
                val view = getLayoutInflater.inflate(R.layout.pwitem, c, false)
                view.findView(TR.name).setText(
                  cursor.getString(cursor.getColumnIndex(Contract.TITLE)))
                view.findView(TR.username).setText(
                  cursor.getString(cursor.getColumnIndex(Contract.USERNAME)))
                view
              }

              def bindView(view: View, c: Context, cursor: Cursor) {
                view.findView(TR.name).setText(
                  cursor.getString(cursor.getColumnIndex(Contract.TITLE)))
                view.findView(TR.username).setText(
                  cursor.getString(cursor.getColumnIndex(Contract.USERNAME)))
              }
            }
            val onClickHandler = { pos: Int =>
              val cursor = adapter.getItem(pos).asInstanceOf[Cursor]
              findView(TR.continu).setEnabled(true)
              findView(TR.continu).onClick {
                ServiceBus.send(AccessibilityFillEvent(
                  packageName, windowId, url,
                  cursor.getString(cursor.getColumnIndex(Contract.USERNAME)),
                  cursor.getString(cursor.getColumnIndex(Contract.PASSWORD))))
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
          } else {
            findView(TR.select_prompt).setVisibility(View.GONE)
          }
        }
      }
    }
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    if (settings.get(Settings.GOOGLE_USER) == null) {
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
