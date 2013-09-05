package com.hanhuy.android.keepshare

import AndroidConversions._

import android.app.{ListActivity, SearchManager, Activity}
import android.os.Bundle
import android.content._
import android.widget.{TextView, CursorAdapter}
import android.view.{View, ViewGroup}
import android.database.{CursorWindow, AbstractCursor, Cursor}
import com.keepassdroid.provider.Contract
import android.net.Uri
import android.provider.BaseColumns

class SearchableActivity extends ListActivity {
  import RichLogger._
  implicit val TAG = LogcatTag("SearchableActivity")

  lazy val settings = Settings(this)
  lazy val km = new KeyManager(this, settings)
  lazy val empty = findViewById(android.R.id.empty).asInstanceOf[TextView]

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.searchable_activity)
    handleIntent(getIntent)
  }

  override def onNewIntent(intent: Intent) {
    super.onNewIntent(intent)
    setIntent(intent)
    handleIntent(intent)
  }

  private def handleIntent(intent: Intent) {
    if (Intent.ACTION_SEARCH == intent.getAction) {
      val query = Option(intent.getStringExtra(SearchManager.QUERY)) getOrElse
        intent.getStringExtra(SearchManager.USER_QUERY)
      v("extras: " + intent.getExtras)
      doSearch(query)
    }
  }

  private def doSearch(query: String) {
    v("Query is: " + query)
    async {
      val cursor = ShareActivity.queryDatabase(this, settings, query :: Nil)
      if (cursor != null && !cursor.isClosed) {
        UiBus.post {
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
          setListAdapter(adapter)
          getListView.onItemClick { pos =>
            ShareActivity.selectHandler(
              this, settings, adapter.getItem(pos).asInstanceOf[Cursor])
          }
          if (adapter.getCount == 0)
            empty.setText(R.string.no_search_results)
        }
      } else UiBus.post {
        empty.setText(R.string.no_search_results)
      }
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    if (request == RequestCodes.REQUEST_SETUP && result == Activity.RESULT_OK) {
      handleIntent(getIntent)
    } else
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
