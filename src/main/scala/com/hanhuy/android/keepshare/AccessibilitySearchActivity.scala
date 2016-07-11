package com.hanhuy.android.keepshare

import java.net.URI

import android.app.{Activity, SearchManager}
import android.content.{ComponentName, Intent}
import android.os.Bundle
import android.view.inputmethod.InputMethodManager
import android.view.{View, ViewGroup}
import android.widget.SearchView.{OnQueryTextListener, OnSuggestionListener}
import android.widget.{AdapterView, BaseAdapter, RelativeLayout}
import com.hanhuy.android.common.{Futures, ServiceBus, UiBus}
import com.hanhuy.android.extensions._
import com.hanhuy.android.common._

import language.postfixOps
import TypedResource._
import com.hanhuy.keepassj.{PwDefs, PwEntry, PwUuid}
import Futures._

import scala.concurrent.Future

/**
 * @author pfnguyen
 */
class AccessibilitySearchActivity extends Activity {

  lazy val settings = Settings(this)

  lazy val views: TypedViewHolder.accessibility_search_activity = TypedViewHolder.setContentView(this, TR.layout.accessibility_search_activity)

  def init() {
    val extras = getIntent.getExtras
    val (windowId, packageName, url) = {
      (extras.getInt(AccessibilityService.EXTRA_WINDOWID, -1),
        extras.getString(AccessibilityService.EXTRA_PACKAGE),
        extras.getString(AccessibilityService.EXTRA_URI))
    }

    views.cancel onClick0 {
      finish()
    }

    views.subject.setText(url)
    views.search.setSearchableInfo(
      this.systemService[SearchManager].getSearchableInfo(
        new ComponentName(this, classOf[SearchableActivity])))
    views.search.setOnQueryTextListener(new OnQueryTextListener {
      override def onQueryTextSubmit(s: String) = {
        Future {
          Database.search(s)
        } onSuccessMain { case r =>
          Option(getCurrentFocus) foreach { focused =>
            AccessibilitySearchActivity.this.systemService[InputMethodManager].hideSoftInputFromWindow(focused.getWindowToken, 0)
          }
          r foreach { result =>
            showResults(result, windowId, packageName, url)
          }
        }
        true
      }
      override def onQueryTextChange(s: String) = {
        views.subject.setVisibility(View.GONE)
        true
      }
    })
    views.search.setOnSuggestionListener(new OnSuggestionListener {
      override def onSuggestionClick(i: Int) = {
        val c = views.search.getSuggestionsAdapter.getCursor
        c.move(i)
        val idx = c.getColumnIndex(SearchManager.SUGGEST_COLUMN_INTENT_EXTRA_DATA)
        val uuid = new PwUuid(KeyManager.bytes(c.getString(idx)))
        Database.rootGroup foreach { root =>
          selectItem(root.FindEntry(uuid, true), windowId, packageName, url)
        }
        true
      }
      override def onSuggestionSelect(i: Int) = onSuggestionClick(i)
    })

    Future {
      val uri = new URI(url)
      val uris = ShareActivity.goUp(uri) map (_.toString)
      val subhosts = ShareActivity.subhosts(uri.getHost)
      val results = ShareActivity.queryDatabase(this, settings, uris ++ subhosts)

      results map { result =>

        UiBus.post {
          views.flipper.showNext()
          views.list.setEmptyView(views.empty)
          showResults(result, windowId, packageName, url)
        }
      }
    }
  }

  def showResults(result: List[PwEntry], windowId: Int, packageName: String, url: String): Unit = {
    val adapter = new BaseAdapter {
      override def getCount = result.size
      override def getItemId(i: Int) = Database.getId(getItem(i))
      override def getItem(i: Int) = result(i)

      override def getView(i: Int, view: View, c: ViewGroup) = {
        val row: TypedViewHolder.pwitem = Option(view.asInstanceOf[RelativeLayout]).map(new TypedViewHolder.pwitem(_)).getOrElse {
          TypedViewHolder.inflate(getLayoutInflater, TR.layout.pwitem, c, false)
        }
        row.name.setText(
          Database.getField(getItem(i), PwDefs.TitleField) orNull)
        row.username.setText(
          Database.getField(getItem(i), PwDefs.UserNameField) orNull)
        row.rootView
      }
    }
    val onClickHandler = { (av: AdapterView[_], v: View, pos: Int, id: Long) =>
      views.continu.setEnabled(true)
      views.continu.onClick0 {
        selectItem(result(pos), windowId, packageName, url)
      }
    }

    views.list.onItemClick(onClickHandler)
    views.list.setAdapter(adapter)
    if (adapter.getCount < 2) {
      views.select_prompt.setVisibility(View.GONE)
      if (adapter.getCount == 1) {
        views.list.setItemChecked(0, true)
        onClickHandler(null, null, 0, 0)
        views.continu.setEnabled(true)
      }
    }
  }

  def selectItem(entry: PwEntry, windowId: Int, packageName: String, url: String): Unit = {
    ServiceBus.send(AccessibilityFillEvent(
      packageName, windowId, url,
      Database.getField(entry, PwDefs.UserNameField) orNull,
      Database.getField(entry, PwDefs.PasswordField) orNull))
    finish()
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
      case RequestCodes.REQUEST_SIGN_IN => result == Activity.RESULT_OK
      case RequestCodes.REQUEST_SETUP => result == Activity.RESULT_OK
      case RequestCodes.REQUEST_PIN => result == Activity.RESULT_OK
    }
    if (success) init() else finish()
  }
}
