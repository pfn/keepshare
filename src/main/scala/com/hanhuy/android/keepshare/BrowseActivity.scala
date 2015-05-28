package com.hanhuy.android.keepshare

import android.app.FragmentManager.OnBackStackChangedListener
import android.content.{Context, ComponentName, Intent}
import android.graphics.BitmapFactory
import android.graphics.drawable.{BitmapDrawable, LayerDrawable}
import android.os.Bundle
import android.support.v4.widget.SwipeRefreshLayout
import android.support.v7.app.ActionBar
import android.support.v7.widget.Toolbar
import android.util.AttributeSet
import android.view.animation.AccelerateDecelerateInterpolator
import com.hanhuy.android.conversions._
import com.hanhuy.android.extensions._
import com.hanhuy.android.common.AndroidConversions._

import android.app._
import android.view._
import android.widget._
import com.hanhuy.keepassj._
import com.melnykov.fab.FloatingActionButton
import io.codetail.animation.SupportAnimator.AnimatorListener
import io.codetail.animation.ViewAnimationUtils
import io.codetail.animation.SupportAnimator
import io.codetail.widget.RevealFrameLayout
import rx.android.schedulers.AndroidSchedulers.mainThread
import rx.lang.scala.JavaConversions._
import rx.lang.scala.{Subscription, Observable, Subject}

import collection.JavaConverters._
import Futures._
import BrowseActivity._

import scala.concurrent.Future
import scala.util.{Success, Failure}

import TypedResource._

/**
 * @author pfnguyen
 */
object BrowseActivity {
  val EXTRA_GROUP_ID = "keepshare.extra.GROUP_ID"
  val EXTRA_STACK = "keepshare.extra.STACK"
  def browse(a: Activity, group: PwGroup): Unit = {
    val intent = new Intent(a, classOf[BrowseActivity])
    intent.putExtra(BrowseActivity.EXTRA_GROUP_ID, KeyManager.hex(group.getUuid.getUuidBytes))
    a.startActivity(intent)
    PINHolderService.ping()
  }
  def open(a: Activity): Unit = {
    val intent = new Intent(a, classOf[BrowseActivity])
    a.startActivity(intent)
  }
  implicit val groupSort = new Ordering[PwGroup] {
    override def compare(x: PwGroup, y: PwGroup) =
      x.getName.compareToIgnoreCase(y.getName)
  }
  implicit val entrySort = new Ordering[PwEntry] {
    override def compare(x: PwEntry, y: PwEntry) =
      x.getStrings.ReadSafe(PwDefs.TitleField).compareToIgnoreCase(y.getStrings.ReadSafe(PwDefs.TitleField))
  }
}
class BrowseActivity extends AuthorizedActivity with TypedActivity with SwipeRefreshLayout.OnRefreshListener {
  lazy val list = findView(TR.list)
  lazy val refresher = findView(TR.refresher)
  private var searchView = Option.empty[SearchView]
  private var isEditing = false
  private var isCreating = false
  lazy val editBar = getLayoutInflater.inflate(
    TR.layout.entry_edit_action_bar, null, false)

  override def onCreateOptionsMenu(menu: Menu) = {
    if (!isEditing) {
      super.onCreateOptionsMenu(menu)
      getMenuInflater.inflate(R.menu.browse, menu)
      searchView = Option(menu.findItem(R.id.menu_search).getActionView.asInstanceOf[SearchView])
      searchView foreach { search =>
        search.setIconifiedByDefault(getResources.getBoolean(R.bool.is_phone))
        search.setSearchableInfo(
          this.systemService[SearchManager].getSearchableInfo(
            new ComponentName(this, classOf[SearchableActivity])))
      }

      Option(menu.findItem(R.id.database_sort)) foreach { m =>
        m.setChecked(settings.get(Settings.BROWSE_SORT_ALPHA))
      }

      if (Database.writeSupported) menu.findItem(R.id.edit_group).setVisible(true)

      database onSuccessMain { case d =>
        if (groupId.contains(d.getRecycleBinUuid) && Database.writeSupported) {
          menu.findItem(R.id.empty_recycle_bin).setVisible(true)
        }
      }
    }
    true
  }

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.browse)

    val fab = findView(TR.observable_fab)
    findView(TR.fab_close) onClick findView(TR.fab_toolbar).hide()
    findView(TR.fab_toolbar).button = fab
    findView(TR.fab_toolbar).container = findView(TR.container)

    findView(TR.create_entry) onClick {
      database.onSuccessMain { case db =>
        val root = db.getRootGroup

        EntryViewActivity.create(this,
          groupId map (root.FindGroup(_, true)) getOrElse root)
      }
    }
    findView(TR.create_group) onClick  creating()
    findView(TR.group_edit) onClick editing(true)

    getSupportActionBar.setCustomView(editBar, new ActionBar.LayoutParams(
      ViewGroup.LayoutParams.MATCH_PARENT,
      ViewGroup.LayoutParams.MATCH_PARENT))
    fab.attachToListView(list)
    if (!Database.writeSupported)
      fab.setVisibility(View.GONE)
    refresher.setOnRefreshListener(this)

    editBar.findView(TR.cancel).onClick {
      updating(false, null)
    }
    editBar.findView(TR.save).onClick {
      val f = Option(getFragmentManager.findFragmentByTag("editor"))
      f foreach { case editor: GroupEditFragment =>
        def copyFromModel(e: PwGroup, needMove: Boolean): Unit = {
          editor.model.title foreach e.setName
          editor.model.notes foreach e.setNotes
          e.setIconId(PwIcon.values()(Database.Icons.indexOf(editor.model.icon)))

          if (needMove) {
            Option(e.getParentGroup) foreach (_.getGroups.Remove(e))
            Database.rootGroup foreach { root =>
              val group = root.FindGroup(editor.model.group, true)
              group.getGroups.Add(e)
              e.setParentGroup(group)
            }
          }
        }
        if (isCreating) {
          val e = new PwGroup(true, true)
          copyFromModel(e, true)
          navigateTo(Option(e.getUuid))
        } else {
          val needMove = editor.baseModel.exists(_.group != editor.model.group)
          ((for {
            root <- Database.rootGroup
            gid  <- groupId
          } yield root.FindGroup(gid, true)) orElse Database.rootGroup) foreach { g =>
            copyFromModel(g, needMove)
            g.Touch(true, false)
            navigateTo(Option(g.getUuid))
          }
        }
      }
      editing(false)
      DatabaseSaveService.save()
    }

    getFragmentManager.addOnBackStackChangedListener(new OnBackStackChangedListener {
      override def onBackStackChanged() = {
        if (!Option(getFragmentManager.findFragmentByTag("editor")).exists(_.isVisible)) {
          editing(false)
          if (!isCreating)
            navigateTo(groupId)
        }
      }
    })
  }

  override def onRefresh() = {
    var p: Option[ProgressDialog] = None
    // because side-effects OP
    var sub: Subscription = null
    sub = DatabaseSaveService.saving.observeOn(mainThread).subscribe(b => {
      if (b) {
        p = Some(ProgressDialog.show(this,
          getString(R.string.saving_database), getString(R.string.please_wait),
          true, false))
      } else {
        p foreach { _.dismiss() }
        sub.unsubscribe()
        Database.close()
        database onSuccessMain { case db =>
          refresher.setRefreshing(false)
          navigateTo(groupId)
        }
        database onFailureMain { case t =>
          refresher.setRefreshing(false)
          Toast.makeText(this, "Unable to reload database: " + t.getMessage, Toast.LENGTH_LONG).show()
          finish()
        }
      }
    })
  }

  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case android.R.id.home =>
      onBackPressed()
      true
    case R.id.edit_group =>
      editing(true)
      true
    case R.id.empty_recycle_bin =>
      Database.emptyRecycleBin()
      list.getAdapter.asInstanceOf[GroupAdapter].notifyDataSetChanged()
      DatabaseSaveService.save()
      true
    case R.id.database_sort =>
      settings.set(Settings.BROWSE_SORT_ALPHA, !item.isChecked)
      item.setChecked(!item.isChecked)
      list.getAdapter.asInstanceOf[GroupAdapter].notifyDataSetChanged()
      true

    case _ => super.onOptionsItemSelected(item)
  }

  override def onNewIntent(intent: Intent) = {
    super.onNewIntent(intent)
    setIntent(intent)
  }


  override def onBackPressed() = {
//    navigateUp()
    if (isEditing) {
      new AlertDialog.Builder(this)
        .setTitle(R.string.cancel)
        .setMessage(R.string.discard_confirm)
        .setNegativeButton(R.string.discard, () => {
          editing(false)
        })
        .setPositiveButton(R.string.keep_editing, null)
        .show()
    } else {
      val shouldBack = searchView exists (_.isIconified)
      searchView foreach (_.setIconified(true))
      if (shouldBack) {
        super.onBackPressed()
        if (Option(getIntent) exists (_.hasExtra(EXTRA_GROUP_ID)))
          overridePendingTransition(0, 0)
      }
    }
  }

  private def navigateUp(): Unit = {
//    stack match {
//      case x :: xs =>
//        stack = xs
//        navigateTo(xs.headOption)
//      case Nil =>
//        Option(getIntent) foreach { _.putExtra(EXTRA_GROUP_ID, null: String) }
//        finish()
//    }
//    for {
//      intent <- Option(getIntent)
//      extras <- Option(intent.getExtras)
//      head   <- stack.headOption
//    } {
//      intent.putExtra(EXTRA_GROUP_ID, head.ToHexString)
//    }
  }

  private def navigateTo(groupId: Option[PwUuid]): Unit = {
    database flatMap { db =>
      if (db.IsOpen) Future.successful(db) else {
        Future.failed(KeyError.NeedLoad)
      }
    } onSuccessMain { case db =>
      val root = db.getRootGroup
      val group = groupId flatMap { id =>
        Option(root.FindGroup(id, true)) } getOrElse root
      val ab = getSupportActionBar
      ab.setSubtitle(group.getName)
//      if (PwUuid.Zero == group.getCustomIconUuid) {
        val bm = BitmapFactory.decodeResource(getResources, Database.Icons(group.getIconId.ordinal))
        val bd = new BitmapDrawable(getResources, bm)
        bd.setGravity(Gravity.CENTER)
        val layers = new LayerDrawable(Array(bd, getResources.getDrawable(R.drawable.logo_frame)))
        ab.setIcon(layers)
        ab.setDisplayShowHomeEnabled(true)
        ab.setDisplayHomeAsUpEnabled(group != root)
//      }
      val groups = group.GetGroups(false).asScala.toList
      val entries = group.GetEntries(false).asScala.toList

      val adapter = new GroupAdapter(db,
        Option(group.getParentGroup), groups, entries)
      list.setDividerHeight(0)
      list.setAdapter(adapter)
      list.onItemClick { (_:AdapterView[_],_:View,row: Int,_:Long) =>
        val item = adapter.getItem(row)
        item.left foreach { grp =>
          browse(this, grp)
          overridePendingTransition(0, 0)
        }
        item.right foreach { entry =>
          EntryViewActivity.show(this, entry)
          overridePendingTransition(R.anim.slide_in_right,
            R.anim.slide_out_left)
        }
      }
    }
    if (ready) database onFailureMain { case e =>
      Toast.makeText(this, "Failed to load database: " + e.getMessage,
        Toast.LENGTH_LONG).show()
    }
  }

//  private var stack = List.empty[PwUuid]

  def groupId = for {
    intent <- Option(getIntent)
    id     <- Option(intent.getStringExtra(BrowseActivity.EXTRA_GROUP_ID))
  } yield new PwUuid(KeyManager.bytes(id))

  private def handleIntent(): Unit = {

//    for {
//      id   <- groupId
//      head <- stack.headOption orElse Some(PwUuid.Zero)
//      root <- Database.rootGroupid orElse Some(PwUuid.Zero)
//    } {
//      if (head != id && id != root)
//        stack = id :: stack
//    }

//    navigateTo(stack.headOption)
    navigateTo(groupId)
  }

  override def onStart() = {
    super.onStart()
  }

  override def onResume() = {
    super.onResume()
    if (!isEditing)
      handleIntent()
  }

  def creating(): Unit = {
    database.map { db =>
      val root = db.getRootGroup
      groupId flatMap { id =>
        Option(root.FindGroup(id, true)) } getOrElse root
    }.onSuccessMain { case group =>
      updating(true, GroupEditFragment.create(group))
      isCreating = true
      editBar.findView(TR.title).setText("Create group")
    }
  }
  def editing(b: Boolean): Unit = {
    database.map { db =>
      val root = db.getRootGroup
      groupId flatMap { id =>
        Option(root.FindGroup(id, true)) } getOrElse root
    }.onSuccessMain { case group =>
      updating(b, if (b) GroupEditFragment.edit(group) else null)
      editBar.findView(TR.title).setText("Update group")
    }
  }

  def updating(b: Boolean, f: Fragment) {
    getSupportActionBar.setHomeButtonEnabled(!b)
    getSupportActionBar.setDisplayShowHomeEnabled(!b)
    getSupportActionBar.setDisplayHomeAsUpEnabled(!b)
    getSupportActionBar.setDisplayShowTitleEnabled(!b)
    getSupportActionBar.setDisplayShowCustomEnabled(b)
    if (b) {
      editBar.getParent match {
        case t: Toolbar => t.setContentInsetsAbsolute(0, 0)
        case _ =>
      }
      findView(TR.observable_fab).hide()
      if (getFragmentManager.findFragmentByTag("editor") == null)
        getFragmentManager.beginTransaction()
          .add(R.id.content, f, "editor")
          .setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
          .addToBackStack("edit")
          .commit()
    } else {
      isCreating = false
      findView(TR.observable_fab).show()
      getFragmentManager.popBackStack()
    }
    isEditing = b
    invalidateOptionsMenu()
  }

  override def onSaveInstanceState(outState: Bundle) = {
    super.onSaveInstanceState(outState)
//    outState.putStringArray(EXTRA_STACK,
//      stack.map (u => u.ToHexString()).toArray)
  }

  override def onRestoreInstanceState(savedInstanceState: Bundle) = {
    super.onRestoreInstanceState(savedInstanceState)
//    Option(savedInstanceState.getStringArray(EXTRA_STACK)) foreach { ss =>
//      stack = ss map (s => new PwUuid(KeyManager.bytes(s))) toList
//    }
  }

  class GroupAdapter(db: PwDatabase, parent: Option[PwGroup], groups: Seq[PwGroup], entries: Seq[PwEntry]) extends BaseAdapter {
    import TypedResource._
    var data = sortedData

    override def hasStableIds = true
    override def getItemId(position: Int) =
      data(position).fold(Database.getId, Database.getId)
    override def getCount = data.size
    override def getView(position: Int, convertView: View, parent: ViewGroup) = {
      val view = if (convertView == null) {
        getLayoutInflater.inflate(TR.layout.browse_pwgroup_item, parent, false)
      } else {
        convertView
      }
      val item = getItem(position)

      view.findView(TR.name).setText(item.fold(
        _.getName, _.getStrings.ReadSafe(PwDefs.TitleField)))
      val folder = view.findView(TR.folder_image)
      val icon = view.findView(TR.entry_image)

      item.left foreach { group =>
        folder.setImageResource(if (db.getRecycleBinUuid.Equals(group.getUuid))
          R.drawable.ic_delete_black_24dp else R.drawable.ic_folder_open_black_24dp)
        if (this.parent exists (_.getUuid.equals(group.getUuid))) {
          folder.setImageResource(R.drawable.ic_expand_less_black_24dp)
        }
        folder.setVisibility(View.VISIBLE)
//        if (PwUuid.Zero == group.getCustomIconUuid)
          icon.setImageResource(Database.Icons(group.getIconId.ordinal))
      }
      item.right foreach { entry =>
        folder.setVisibility(View.INVISIBLE)
//        if (PwUuid.Zero == entry.getCustomIconUuid)
          icon.setImageResource(Database.Icons(entry.getIconId.ordinal))
      }

      view
    }
    override def getItem(position: Int) = data(position)
    override def notifyDataSetChanged() {
      data = sortedData
      super.notifyDataSetChanged()
    }

    def sortedData: Vector[Either[PwGroup,PwEntry]] = {
      (parent map Left.apply).toVector ++ (if (settings.get(Settings.BROWSE_SORT_ALPHA)) {
        (groups.sorted map (Left(_))) ++ (entries.sorted map (Right(_)))
      } else {
        (groups map (Left(_))) ++ (entries map (Right(_)))
      })
    }
    //    override def getItemViewType(position: Int) = if (data(position).isLeft) 0 else 1
    //    override def getViewTypeCount = 2
  }
}

class FabToolbar(c: Context, attrs: AttributeSet) extends RevealFrameLayout(c, attrs) {
  lazy val screenWidth = getResources.getDisplayMetrics.widthPixels

  private[this] var showing = false
  private[this] var _button: ObservableFab = _
  def button = _button
  def button_=(b: ObservableFab) = {
    _button = b
    if (container != null)
      container.setBackgroundColor(b.getColorNormal)
    b onClick show()
    b.visibility.observeOn(mainThread).subscribe(b => if (b && showing) hide())
  }

  private[this] var _container: ViewGroup = _
  def container = _container
  def container_=(b: ViewGroup) = {
    _container = b
    if (button != null)
      container.setBackgroundColor(button.getColorNormal)
  }

  def show(): Unit = {
    container.setVisibility(View.VISIBLE)
    showing = true
    button.hide(false)
    animate(0, screenWidth, null)
  }

  def hide(): Unit = {
    showing = false
    animate(screenWidth, 0, closeListener)
  }

  def animate(sr: Float, er: Float, listener: SupportAnimator.AnimatorListener) {
    val start = math.max(0, math.abs(button.getTop - button.getBottom)) / 2
    val cx = (button.getLeft + button.getRight) / 2
    val cy = (button.getTop + button.getBottom) / 2

    val animator = ViewAnimationUtils.createCircularReveal(container, cx, cy, start, er)
    animator.setInterpolator(new AccelerateDecelerateInterpolator)
    animator.setDuration(250)
    if (listener != null) {
      animator.addListener(listener)
    }
    animator.start()
  }

  val closeListener = new AnimatorListener {
    override def onAnimationEnd() = {
      container.setVisibility(View.GONE)
      button.show(false)
    }

    override def onAnimationRepeat() = ()
    override def onAnimationStart() = ()
    override def onAnimationCancel() = ()
  }

}

class ObservableFab(c: Context, attrs: AttributeSet) extends FloatingActionButton(c, attrs) {
  private[this] val _vis: Subject[Boolean] = Subject()
  def visibility: Observable[Boolean] = _vis

  override def show(animate: Boolean) = {
    super.show(animate)
    _vis.onNext(true)
  }

  override def hide(animate: Boolean) = {
    super.hide(animate)
    _vis.onNext(false)
  }
}
