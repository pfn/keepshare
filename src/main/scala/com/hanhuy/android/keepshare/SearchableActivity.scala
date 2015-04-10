package com.hanhuy.android.keepshare

import android.support.v7.app.ActionBarActivity
import android.view.inputmethod.InputMethodManager
import com.hanhuy.android.common.AndroidConversions._
import com.hanhuy.android.common.{UiBus, LogcatTag}
import com.hanhuy.android.common.RichLogger._
import TypedResource._

import android.app.{ProgressDialog, SearchManager, Activity}
import android.os.Bundle
import android.content._
import android.widget._
import android.database.{AbstractCursor, Cursor}
import android.net.Uri
import android.provider.BaseColumns
import android.view.{MenuItem, View, ViewGroup, Menu}
import com.hanhuy.keepassj.PwDefs

class SearchableActivity extends ActionBarActivity {
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

    if (settings.get(Settings.FIRST_RUN)) {
      startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (!km.ready) {
      if (settings.get(Settings.NEEDS_PIN) && PINHolderService.instance.isEmpty)
        startActivityForResult(new Intent(this, classOf[PINEntryActivity]),
          RequestCodes.REQUEST_PIN)
      else
        startActivityForResult(SetupActivity.intent, RequestCodes.REQUEST_SETUP)
    } else if (KeyManager.cloudKey.isEmpty) {
      val p = ProgressDialog.show(this, getString(R.string.loading),
        getString(R.string.please_wait), true, false)
      async {
        km.loadKey()
        km.getConfig match {
          case Left(x) =>
            startActivityForResult(
              SetupActivity.intent, RequestCodes.REQUEST_SETUP)
          case _ =>
        }
        UiBus.post {
          p.dismiss()
        }
      }
    }
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
        v("extras: " + intent.getExtras)
        queryInput foreach { q =>
          doSearch(q, Option(
            intent.getStringExtra(SearchManager.EXTRA_DATA_KEY)) map (_.toLong))
          searchView foreach { s =>
            s.setQuery(q, false)
            this.systemService[InputMethodManager].hideSoftInputFromWindow(s.getWindowToken, 0)
          }
        }
      } else {
        if (!settings.get(Settings.NEEDS_PIN) || PINHolderService.instance.isDefined)
          doSearch("", None)
      }
    }
  }

  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.searchable, menu)

    if (settings.get(Settings.NEEDS_PIN))
      menu.removeItem(R.id.menu_setup_pin)
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

  override def onOptionsItemSelected(item: MenuItem) = {
    item.getItemId match {
      case R.id.menu_setup =>
        startActivity(new Intent(this, classOf[SetupActivity]))
        true
      case R.id.menu_setup_pin =>
        if (km.ready) {
          startActivity(new Intent(this, classOf[PINSetupActivity]))
        } else {
          Toast.makeText(this,
            R.string.setup_required_for_pin, Toast.LENGTH_SHORT).show()
        }
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
      val results = ShareActivity.queryDatabase(this, settings, query :: Nil)
      results map { result =>
        val selected = (for {
          i <- id
          f <- result.zipWithIndex find { case (e, _) =>
            i == Database.getId(e)
          }
        } yield f._2) getOrElse -1
        UiBus.post {
          val adapter = new BaseAdapter {

            override def getCount = result.size

            override def getItemId(i: Int) = Database.getId(getItem(i))

            override def getView(i: Int, view: View, c: ViewGroup) = {
              val row = Option(view) getOrElse {
                getLayoutInflater.inflate(R.layout.pwitem, c, false)
              }
              row.findView(TR.name).setText(
                Database.getField(getItem(i), PwDefs.TitleField) orNull)
              row.findView(TR.username).setText(
                Database.getField(getItem(i), PwDefs.UserNameField) orNull)
              row
            }

            override def getItem(i: Int) = result(i)

          }
          list.setAdapter(adapter)
          list.onItemClick { pos =>
            ShareActivity.selectHandler(
              this, settings, adapter.getItem(pos))
          }
          if (adapter.getCount == 0)
            empty.setText(R.string.no_search_results)
        }
        UiBus.post {
          pd.dismiss()
          if (selected != -1) {
            UiBus.post {
              list.setItemChecked(selected, true)
              list.smoothScrollToPosition(selected)
            }
          }
        }
      } getOrElse UiBus.post {
        empty.setText(R.string.no_search_results)
      }
      UiBus.post { pd.dismiss() }
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
      case RequestCodes.REQUEST_SIGN_IN => false
    }
    if (!success)
      finish()
  }
}

class SearchProvider extends ContentProvider {
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
    val empty = Option(u.getPath) exists(_.endsWith("/"))

    val q = Uri.decode(u.getLastPathSegment).trim
    val results = if (empty)
      None
    else
      ShareActivity.queryDatabase(getContext, settings, q :: Nil)

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
        SearchManager.SUGGEST_COLUMN_INTENT_DATA_ID,
        SearchManager.SUGGEST_COLUMN_INTENT_EXTRA_DATA)

      def getId(row: Int) =
        Left(results map { r => Database.getId(r(row)) } getOrElse -1L)

      def getStr(field: String, row: Int) =
        Right(results flatMap { r => Database.getField(r(row), field) } orNull)

      private lazy val columnMap: Map[Int, Int => Either[Long,String]] = Map(
        0 -> getId,
        (1, getStr(PwDefs.TitleField, _: Int)),
        (2, getStr(PwDefs.UserNameField, _: Int)),
        3 -> getId,
        4 -> getId)

      override def getType(column: Int) = column match {
        case 0 => Cursor.FIELD_TYPE_INTEGER
        case 1 => Cursor.FIELD_TYPE_STRING
        case 2 => Cursor.FIELD_TYPE_STRING
        case 3 => Cursor.FIELD_TYPE_INTEGER
        case 4 => Cursor.FIELD_TYPE_INTEGER
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
