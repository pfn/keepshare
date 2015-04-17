package com.hanhuy.android.keepshare

import android.app.Activity
import android.content.Intent
import android.graphics.BitmapFactory
import android.graphics.drawable.{LayerDrawable, BitmapDrawable}
import android.os.Bundle
import android.view._
import com.hanhuy.keepassj.{PwDefs, PwUuid, PwEntry}

import Futures._
import EntryViewActivity._

/**
 * @author pfnguyen
 */
object EntryViewActivity {
  val EXTRA_ENTRY_ID = "keepshare.extra.ENTRY_ID"

  def show(a: Activity, e: PwEntry): Unit = {
    val intent = new Intent(a, classOf[EntryViewActivity])
    intent.putExtra(EXTRA_ENTRY_ID, e.getUuid.ToHexString)
    a.startActivity(intent)
  }
}
class EntryViewActivity extends AuthorizedActivity with TypedActivity {
  private var pwentry = Option.empty[PwEntry]
  override def onBackPressed() = {
    super.onBackPressed()
    overridePendingTransition(
      android.R.anim.slide_in_left, android.R.anim.slide_out_right)
  }

  override def onCreate(savedInstanceState: Bundle) = {
    getWindow.setFlags(WindowManager.LayoutParams.FLAG_SECURE,
      WindowManager.LayoutParams.FLAG_SECURE)
    super.onCreate(savedInstanceState)
    setContentView(R.layout.entry_view)

    val id = for {
      intent <- Option(getIntent)
      entry  <- Option(intent.getStringExtra(EXTRA_ENTRY_ID))
    } {
      val uuid = new PwUuid(KeyManager.bytes(entry))
      database.onSuccessMain { case db =>
        showEntry(db.getRootGroup.FindEntry(uuid, true))
      }
    }
  }


  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.entry_view, menu)
    super.onCreateOptionsMenu(menu)
  }

  override def onOptionsItemSelected(item: MenuItem) = item.getItemId match {
    case android.R.id.home =>
      onBackPressed()
      true
    case R.id.load_keyboard =>
      pwentry foreach (ShareActivity.selectHandler(this, settings, _))
      true
    case _ => super.onOptionsItemSelected(item)
  }

  private def showEntry(entry: PwEntry): Unit = {
    pwentry = Some(entry)

    val ab = getSupportActionBar
    val strings = entry.getStrings
    ab.setSubtitle(strings.ReadSafe(PwDefs.TitleField))
    if (PwUuid.Zero == entry.getCustomIconUuid) {
      val bm = BitmapFactory.decodeResource(getResources, Database.Icons(entry.getIconId.ordinal))
      val bd = new BitmapDrawable(getResources, bm)
      bd.setGravity(Gravity.CENTER)
      val layers = new LayerDrawable(Array(bd, getDrawable(R.drawable.logo_frame)))
      ab.setIcon(layers)
      ab.setDisplayShowHomeEnabled(true)
      ab.setDisplayHomeAsUpEnabled(true)
    }

    findView(TR.username).setText(strings.ReadSafe(PwDefs.UserNameField))
    findView(TR.password).setText(strings.ReadSafeEx(PwDefs.PasswordField))
    findView(TR.link).setText(strings.ReadSafeEx(PwDefs.UrlField))
    findView(TR.notes).setText(strings.ReadSafeEx(PwDefs.NotesField))
  }
}
