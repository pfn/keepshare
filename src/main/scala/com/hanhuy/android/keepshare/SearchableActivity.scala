package com.hanhuy.android.keepshare

import android.view.inputmethod.InputMethodManager
import com.hanhuy.android.common._
import com.hanhuy.android.extensions._
import TypedResource._

import android.app.{Dialog, ProgressDialog, SearchManager, Activity}
import android.os.Bundle
import android.content._
import android.widget._
import android.database.{AbstractCursor, Cursor}
import android.net.Uri
import android.provider.BaseColumns
import android.view.{MenuItem, View, ViewGroup, Menu}
import com.hanhuy.keepassj.{PwUuid, PwDefs}

import Futures._

import scala.language.postfixOps
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class SearchableActivity extends AuthorizedActivity {
  val log = Logcat("SearchableActivity")

  private var currentDialog = Option.empty[Dialog]
  lazy val empty = findViewById(android.R.id.empty).asInstanceOf[TextView]
  lazy val list = findViewById(android.R.id.list).asInstanceOf[ListView]

  private var searchView = Option.empty[SearchView]
  private var queryInput = Option.empty[String]

  override def onDestroy() = {
    currentDialog foreach (_.dismiss())
    currentDialog = None
    super.onDestroy()
  }

  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.searchable_activity)
    list.setEmptyView(empty)
    getSupportActionBar.setDisplayHomeAsUpEnabled(true)
  }

  override def onNewIntent(intent: Intent) {
    super.onNewIntent(intent)
    setIntent(intent)
    searchView foreach { s =>
      UiBus.post {
        s.setIconified(true)
      }
    }
  }

  override def onResume() {
    super.onResume()
    handleIntent(getIntent)
  }

  private def handleIntent(intent: Intent) {
    if (km.ready) {
      if (Intent.ACTION_SEARCH == intent.getAction) {
        queryInput = Option(intent.getStringExtra(SearchManager.QUERY)) orElse
          Option(intent.getCharSequenceExtra(SearchManager.USER_QUERY)) map (
          _.toString)
        log.v("extras: " + intent.getExtras)
        queryInput foreach { q =>
          doSearch(q, Option(
            intent.getStringExtra(SearchManager.EXTRA_DATA_KEY)))
          searchView foreach { s =>
            s.setQuery(q, false)
            this.systemService[InputMethodManager].hideSoftInputFromWindow(s.getWindowToken, 0)
          }
        }
      } else {
        if (!settings.get(Settings.NEEDS_PIN) || PINHolderService.instance.isDefined)
        BrowseActivity.open(this)
        overridePendingTransition(0, 0)
      }
    }
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    super.onCreateOptionsMenu(menu)
    getMenuInflater.inflate(R.menu.searchable, menu)

    searchView = Option(menu.findItem(R.id.menu_search)
      .getActionView.asInstanceOf[SearchView])
    searchView foreach { search =>
      search.setIconifiedByDefault(getResources.getBoolean(R.bool.is_phone))
      search.setSearchableInfo(
        this.systemService[SearchManager].getSearchableInfo(getComponentName))
      queryInput foreach { search.setQuery(_, false) }
    }
    true
  }

  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case android.R.id.home =>
      BrowseActivity.open(this)
      true
    case _ => super.onOptionsItemSelected(item)
  }

  private def doSearch(query: String, id: Option[String]) {
    log.v("Query is: " + query)
    log.v("id is: " + id)
    if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty) {
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
      return
    }

    currentDialog = Some(ProgressDialog.show(this,
      getString(R.string.searching),
      getString(R.string.running_search), true, false))
    val results = ShareActivity.queryDatabase(this, settings, query :: Nil)
    results onSuccessMain { case result =>
      val selected = (for {
        i <- id
        f <- result.zipWithIndex find { case (e, _) =>
          i == e.getUuid.ToHexString
        }
      } yield f._2) getOrElse -1
      val adapter = new BaseAdapter {

        override def getCount = result.size

        override def getItemId(i: Int) = Database.getId(getItem(i))

        override def getView(i: Int, view: View, c: ViewGroup) = {
          val row = Option(view) getOrElse {
            getLayoutInflater.inflate(R.layout.pwitem, c, false)
          }

          if (PwUuid.Zero == getItem(i).getCustomIconUuid)
            row.findView(TR.entry_image).setImageResource(Database.Icons(getItem(i).getIconId.ordinal))
          row.findView(TR.name).setText(
            Database.getField(getItem(i), PwDefs.TitleField) orNull)
          row.findView(TR.username).setText(
            Database.getField(getItem(i), PwDefs.UserNameField) orNull)
          row
        }

        override def getItem(i: Int) = result(i)

      }
      list.setAdapter(adapter)
      list.onItemClick { (_,_,pos,_)=>
        EntryViewActivity.show(this, adapter.getItem(pos))
        overridePendingTransition(R.anim.slide_in_right,
          R.anim.slide_out_left)
      }
      currentDialog foreach (_.dismiss())
      currentDialog = None
      if (selected != -1) {
        UiBus.post {
          list.setItemChecked(selected, true)
          list.smoothScrollToPosition(selected)
        }
      }
    }
    results onFailureMain { case e =>
      currentDialog foreach (_.dismiss())
      currentDialog = None
    }
  }

  override def onActivityResult(request: Int, result: Int, data: Intent) {
    super.onActivityResult(request, result, data)

    request match {
      case RequestCodes.REQUEST_SETUP =>
        if (result == Activity.RESULT_OK)
          handleIntent(getIntent)
      case _ =>
    }
  }
}

class SearchProvider extends ContentProvider {
  val log = Logcat("SearchProvider")
  lazy val settings = Settings(getContext)

  // noops
  def delete(p1: Uri, p2: String, p3: Array[String]) = 0
  def insert(p1: Uri, p2: ContentValues) = null
  def update(p1: Uri, p2: ContentValues, p3: String, p4: Array[String]) = 0

  def getType(p1: Uri) = SearchManager.SUGGEST_MIME_TYPE

  def onCreate() = true

  def query(u: Uri, proj: Array[String], arg: String,
            args: Array[String], order: String) = {

    log.v("uri is: " + u)
    val empty = Option(u.getPath) exists(_.endsWith("/"))

    val q = Uri.decode(u.getLastPathSegment).trim
    val results = if (empty)
      None
    else {
      Try(Await.result(ShareActivity.queryDatabase(getContext, settings, q :: Nil), Duration.Inf)).toOption
    }

    new AbstractCursor {
      def toDouble(x: Either[Long,String]) = x.fold(_.toDouble, _.toDouble)
      def toFloat(x:  Either[Long,String]) = x.fold(_.toFloat,  _.toFloat)
      def toInt(x:    Either[Long,String]) = x.fold(_.toInt,    _.toInt)
      def toLong(x:   Either[Long,String]) = x.fold(identity,   _.toLong)
      def toShort(x:  Either[Long,String]) = x.fold(_.toShort,  _.toShort)
      def toString(x: Either[Long,String]) = x.fold(_.toString, identity)
      def isNull(x:   Either[Long,String]) = x.fold(_ => false, _ == null)

      var position = 0

      // SUGGEST_COLUMN_INTENT_DATA_ID is necessary for global search to work
      def getColumnNames = Array(
        BaseColumns._ID,
        SearchManager.SUGGEST_COLUMN_TEXT_1,
        SearchManager.SUGGEST_COLUMN_TEXT_2,
        SearchManager.SUGGEST_COLUMN_ICON_1,
        SearchManager.SUGGEST_COLUMN_INTENT_DATA_ID,
        SearchManager.SUGGEST_COLUMN_INTENT_EXTRA_DATA)

      def getUuid(row: Int) = Right(results map { r => Database.getUuid(r(row)) } orNull)
      def getId(row: Int) =
        Left(results map { r => Database.getId(r(row)) } getOrElse -1L)

      def getIcon(row: Int) = Left(results map (r => Database.Icons(r(row).getIconId.ordinal).toLong) getOrElse 0l)

      def getStr(field: String, row: Int) =
        Right(results flatMap { r => Database.getField(r(row), field) } orNull)

      private lazy val columnMap: Map[Int, Int => Either[Long,String]] = Map(
        0 -> getId,
        (1, getStr(PwDefs.TitleField, _: Int)),
        (2, getStr(PwDefs.UserNameField, _: Int)),
        3 -> getIcon,
        4 -> getId,
        5 -> getUuid)

      override def getType(column: Int) = column match {
        case 0 => Cursor.FIELD_TYPE_INTEGER
        case 1 => Cursor.FIELD_TYPE_STRING
        case 2 => Cursor.FIELD_TYPE_STRING
        case 3 => Cursor.FIELD_TYPE_INTEGER
        case 4 => Cursor.FIELD_TYPE_INTEGER
        case 5 => Cursor.FIELD_TYPE_STRING
      }

      def getCount            = results map (_.size) getOrElse 0
      def getInt(col: Int)    = toInt(columnMap(col)(position))
      def getDouble(col: Int) = toDouble(columnMap(col)(position))
      def getFloat(col: Int)  = toFloat(columnMap(col)(position))
      def getLong(col: Int)   = toLong(columnMap(col)(position))
      def getShort(col: Int)  = toShort(columnMap(col)(position))
      def getString(col: Int) = toString(columnMap(col)(position))
      def isNull(col: Int)    = isNull(columnMap(col)(position))

      override def onMove(oldPosition: Int, newPosition: Int) = {
        val r = newPosition < getCount && newPosition >= 0
        if (r)
          position = newPosition
        r
      }
    }
  }
}
