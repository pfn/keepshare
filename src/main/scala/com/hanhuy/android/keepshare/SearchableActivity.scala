package com.hanhuy.android.keepshare

import AndroidConversions._


import android.app.{ProgressDialog, SearchManager, Activity}
import android.os.Bundle
import android.content._
import android.widget._
import android.database.{AbstractCursor, Cursor}
import com.keepassdroid.provider.Contract
import android.net.Uri
import android.provider.BaseColumns
import android.view.{MenuItem, View, ViewGroup, Menu}

class SearchableActivity extends Activity {
  import RichLogger._
  implicit val TAG = LogcatTag("SearchableActivity")
  val _implicit: RichActivity = this
  import _implicit._

  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  lazy val empty = findViewById(android.R.id.empty).asInstanceOf[TextView]
  lazy val list = findViewById(android.R.id.list).asInstanceOf[ListView]

  private var searchView = Option.empty[SearchView]
  private var queryInput = Option.empty[String]

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.searchable_activity)
    list.setEmptyView(empty)

    if (settings.get(Settings.GOOGLE_USER) == null) {
      startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (!km.ready) {
      if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty)
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
      else
        startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (KeyManager.cloudKey == null) {
      val p = ProgressDialog.show(this, getString(R.string.loading),
        getString(R.string.please_wait), true, false)
      async {
        km.accountName = settings.get(Settings.GOOGLE_USER)
        km.loadKey()
        km.getConfig match {
          case Left(x) =>
            startActivityForResult(
              SetupActivity.intent, RequestCodes.REQUEST_SETUP)
          case _ =>
        }
        UiBus.post {
          p.dismiss()
          handleIntent(getIntent)
        }
      }
    }
  }

  override def onNewIntent(intent: Intent) {
    super.onNewIntent(intent)
    setIntent(intent)
  }

  override def onResume() {
    super.onResume()
    handleIntent(getIntent)
  }

  private def handleIntent(intent: Intent) {
    if (Intent.ACTION_SEARCH == intent.getAction) {
      queryInput = Option(intent.getStringExtra(SearchManager.QUERY)) orElse
        Option(intent.getCharSequenceExtra(SearchManager.USER_QUERY)) map (
        _.toString)
      v("extras: " + intent.getExtras)
      queryInput foreach { q =>
        doSearch(q, Option(
          intent.getStringExtra(SearchManager.EXTRA_DATA_KEY)) map (_.toLong))
        searchView foreach { _.setQuery(q, false) }
      }
    } else {
      empty.setText(R.string.no_search_query)
    }
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.searchable, menu)

    searchView = Option(menu.findItem(R.id.menu_search)
      .getActionView.asInstanceOf[SearchView])
    searchView foreach { search =>
      search.setIconifiedByDefault(getResources.getBoolean(R.bool.is_phone))
      search.setSearchableInfo(
        systemService[SearchManager].getSearchableInfo(getComponentName))
      queryInput foreach { search.setQuery(_, false) }
    }
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
      case R.id.menu_setup =>
        startActivity(new Intent(this, classOf[SetupActivity]))
        true
      case _ => super.onOptionsItemSelected(item)
    }
  }

  private def doSearch(query: String, id: Option[Long]) {
    v("Query is: " + query)
    v("id is: " + id)
    if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty) {
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
      return
    }

    val pd = ProgressDialog.show(this,
      getString(R.string.searching),
      getString(R.string.running_search), true, false)
    async {
      val cursor = ShareActivity.queryDatabase(this, settings, query :: Nil)
      if (cursor != null && !cursor.isClosed) {
        UiBus.post {
          var selected = -1
          Stream.continually(cursor.moveToNext) takeWhile (
            _ && selected == -1) foreach { _ =>
              if (id exists (_ == cursor.getLong(0)))
                selected = cursor.getPosition()
            }
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
          list.setAdapter(adapter)
          if (selected != -1) {
            list.setItemChecked(selected, true)
            list.smoothScrollToPosition(selected)
          }
          list.onItemClick { pos =>
            ShareActivity.selectHandler(
              this, settings, adapter.getItem(pos).asInstanceOf[Cursor])
          }
          if (adapter.getCount == 0)
            empty.setText(R.string.no_search_results)
        }
      } else UiBus.post {
        empty.setText(R.string.no_search_results)
      }
      UiBus.post {
        pd.dismiss()
      }
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {

    val success = request match {
      case RequestCodes.REQUEST_SETUP =>
        if (result == Activity.RESULT_OK) {
          handleIntent(getIntent)
          true
        } else {
          false
        }
      case RequestCodes.REQUEST_PIN => result == Activity.RESULT_OK
    }
    if (!success)
      finish()
  }
}

class SearchProvider extends ContentProvider {
  import RichLogger._
  implicit val TAG = LogcatTag("SearchProvider")
  lazy val settings = Settings(getContext)

  // noops
  def delete(p1: Uri, p2: String, p3: Array[String]) = 0
  def insert(p1: Uri, p2: ContentValues) = null
  def update(p1: Uri, p2: ContentValues, p3: String, p4: Array[String]) = 0

  def getType(p1: Uri) = SearchManager.SUGGEST_MIME_TYPE

  def onCreate() = true

  def query(u: Uri, proj: Array[String], arg: String,
            args: Array[String], order: String) = {

    v("uri is: " + u)
    val q = Uri.decode(u.getLastPathSegment).trim
    val cursor = ShareActivity.queryDatabase(getContext, settings, q :: Nil)

    new AbstractCursor {
      // SUGGEST_COLUMN_INTENT_DATA_ID is necessary for global search to work
      def getColumnNames = Array(
        BaseColumns._ID,
        SearchManager.SUGGEST_COLUMN_TEXT_1,
        SearchManager.SUGGEST_COLUMN_TEXT_2,
        SearchManager.SUGGEST_COLUMN_INTENT_DATA_ID,
        SearchManager.SUGGEST_COLUMN_INTENT_EXTRA_DATA)

      private lazy val columnMap = Map(
        0 -> cursor.getColumnIndex(Contract._ID),
        1 -> cursor.getColumnIndex(Contract.TITLE),
        2 -> cursor.getColumnIndex(Contract.USERNAME),
        3 -> cursor.getColumnIndex(Contract._ID),
        4 -> cursor.getColumnIndex(Contract._ID))

      override def getType(column: Int) = column match {
        case 0 => Cursor.FIELD_TYPE_INTEGER
        case 1 => Cursor.FIELD_TYPE_STRING
        case 2 => Cursor.FIELD_TYPE_STRING
        case 3 => Cursor.FIELD_TYPE_INTEGER
        case 4 => Cursor.FIELD_TYPE_INTEGER
      }

      def getCount            = if (cursor.isClosed) 0 else cursor.getCount
      def getInt(col: Int)    = cursor.getInt(columnMap(col))
      def getDouble(col: Int) = cursor.getDouble(columnMap(col))
      def getFloat(col: Int)  = cursor.getFloat(columnMap(col))
      def getLong(col: Int)   = cursor.getLong(columnMap(col))
      def getShort(col: Int)  = cursor.getShort(columnMap(col))
      def getString(col: Int) = cursor.getString(columnMap(col))
      def isNull(col: Int)    = cursor.isNull(columnMap(col))

      override def onMove(oldPosition: Int, newPosition: Int) =
        cursor.moveToPosition(newPosition)
    }
  }
}
