package com.hanhuy.android.keepshare

import java.net.URI

import android.app.{SearchManager, Activity}
import android.content.{ComponentName, Intent}
import android.os.Bundle
import android.view.inputmethod.InputMethodManager
import android.view.{View, ViewGroup}
import android.widget.SearchView.{OnSuggestionListener, OnQueryTextListener}
import android.widget.{AdapterView, BaseAdapter}
import com.hanhuy.android.common.{ServiceBus, UiBus}

import com.hanhuy.android.extensions._
import com.hanhuy.android.common.AndroidConversions._

import language.postfixOps

import TypedResource._
import com.hanhuy.keepassj.{PwEntry, PwUuid, PwDefs}
import Futures._

import scala.concurrent.Future

/**
 * @author pfnguyen
 */
class AccessibilitySearchActivity extends Activity with TypedViewHolder {

  lazy val settings = new Settings(this)

  def init() {
    setContentView(R.layout.accessibility_search_activity)
    val extras = getIntent.getExtras
    val (windowId, packageName, url) = {
      (extras.getInt(AccessibilityService.EXTRA_WINDOWID, -1),
        extras.getString(AccessibilityService.EXTRA_PACKAGE),
        extras.getString(AccessibilityService.EXTRA_URI))
    }

    findView(TR.cancel) onClick0 {
      finish()
    }

    val subject = findView(TR.subject)
    subject.setText(url)
    val search = findView(TR.search)
    search.setSearchableInfo(
      this.systemService[SearchManager].getSearchableInfo(
        new ComponentName(this, classOf[SearchableActivity])))
    search.setOnQueryTextListener(new OnQueryTextListener {
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
        subject.setVisibility(View.GONE)
        true
      }
    })
    search.setOnSuggestionListener(new OnSuggestionListener {
      override def onSuggestionClick(i: Int) = {
        val c = search.getSuggestionsAdapter.getCursor
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
        val row = Option(view) getOrElse getLayoutInflater.inflate(R.layout.pwitem, c, false)
        row.findView(TR.name).setText(
          Database.getField(getItem(i), PwDefs.TitleField) orNull)
        row.findView(TR.username).setText(
          Database.getField(getItem(i), PwDefs.UserNameField) orNull)
        row
      }
    }
    val onClickHandler = { (av: AdapterView[_], v: View, pos: Int, id: Long) =>
      findView(TR.continu).setEnabled(true)
      findView(TR.continu).onClick0 {
        selectItem(result(pos), windowId, packageName, url)
      }
    }

    val list = findView(TR.list)
    list.onItemClick(onClickHandler)
    list.setAdapter(adapter)
    if (adapter.getCount < 2) {
      findView(TR.select_prompt).setVisibility(View.GONE)
      if (adapter.getCount == 1) {
        list.setItemChecked(0, true)
        onClickHandler(null, null, 0, 0)
        findView(TR.continu).setEnabled(true)
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
