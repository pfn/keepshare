package com.hanhuy.android.keepshare

import java.io.FileOutputStream
import java.nio.ByteBuffer

import android.annotation.TargetApi
import android.content.Intent
import android.net.Uri
import android.provider.DocumentsContract
import com.hanhuy.android.common.ServiceBus
import com.hanhuy.keepassj._
import com.hanhuy.keepassj.spr.{SprEngine, SprContext, SprCompileFlags}

import scala.concurrent.{Promise, Future}

import Futures._

/**
 * @author pfnguyen
 */
object Database {
  def rootGroupid = database map (_.getRootGroup.getUuid)
  private var database = Option.empty[PwDatabase]
  def pwdatabase = database.get

  def isOpen = database.nonEmpty

  // this should only run on api19+
  // path is already normalized to a local path if on an earlier version
  @TargetApi(19)
  def resolvePath(f: String): Future[String] = {
    if (f startsWith "content:") Future {
      val external = Application.instance.getExternalFilesDir(null)
      val dest = new java.io.File(external, KeyManager.sha1(f.toString.getBytes("utf-8")))
      val uri = Uri.parse(f)
      val cr = Application.instance.getContentResolver

      val c = cr.query(uri, null, null, null, null)
      val cLastModified = c.getColumnIndex(
        DocumentsContract.Document.COLUMN_LAST_MODIFIED)
      var lastModified = Option.empty[Long]
      Stream.continually(c.moveToNext) takeWhile identity foreach { _ =>
        lastModified = Some(c.getLong(cLastModified))
      }
      c.close()

      if (lastModified exists (_ > dest.lastModified)) {
        val input = cr.openInputStream(uri)
        val out = new FileOutputStream(dest)
        try {
          var total = 0
          val buf = Array.ofDim[Byte](32768)
          Stream.continually(input.read(buf, 0, 32768)) takeWhile (_ != -1) foreach { read =>
            total += read
            out.write(buf, 0, read)
          }
        } finally {
          input.close()
          out.close()
        }
      }
      cr.takePersistableUriPermission(uri,
        Intent.FLAG_GRANT_READ_URI_PERMISSION |
          Intent.FLAG_GRANT_WRITE_URI_PERMISSION)
      dest.getAbsolutePath
    } else Promise.successful(f).future
  }
  def open(db: String, pw: Option[String], keyfile: Option[String]): Future[PwDatabase] = {
    close()
    resolvePath(db) map { p =>
      val pwdb = new PwDatabase
      val key = new CompositeKey

      pw find (_.nonEmpty) foreach { p => key.AddUserKey(new KcpPassword(p)) }
      keyfile find (_.nonEmpty) foreach { f => key.AddUserKey(new KcpKeyFile(f)) }

      pwdb.Open(IOConnectionInfo.FromPath(p), key, null)
      database = Some(pwdb)
      pwdb
    }
  }

  def close(): Unit = {
    ServiceBus.send(DatabaseClosed)
    database foreach { _.Close() }
    database = None
  }

  def search(q: String) = {
    database flatMap { db =>
      val p = SearchParameters.getNone
      p.setSearchInUserNames(true)
      p.setSearchInTitles(true)
      p.setSearchInUrls(true)
      p.setSearchInNotes(true)
      p.setSearchInTags(true)
      p.setSearchString(q)
      val results = new PwObjectList[PwEntry]
      db.getRootGroup.SearchEntries(p, results)
      if (results.getUCount > 0) {
        import collection.JavaConversions._
        Option(results.toList.sortBy(
          _.getStrings.ReadSafe(PwDefs.TitleField).toLowerCase))
      } else None
    }
  }

  def getField(entry: PwEntry, field: String) = database flatMap { db =>
    val ctx = new SprContext(entry, db, SprCompileFlags.All.flags)
    Option(SprEngine.Compile(entry.getStrings.ReadSafe(field), ctx))
  }

  def getId(entry: PwEntry) = {
    ByteBuffer.wrap(entry.getUuid.getUuidBytes).getLong
  }
  def getId(group: PwGroup) = {
    ByteBuffer.wrap(group.getUuid.getUuidBytes).getLong
  }

  val Icons = Vector(
    R.drawable.i00_password,
    R.drawable.i01_package_network,
    R.drawable.i02_messagebox_warning,
    R.drawable.i03_server,
    R.drawable.i04_klipper,
    R.drawable.i05_edu_languages,
    R.drawable.i06_kcmdf,
    R.drawable.i07_kate,
    R.drawable.i08_socket,
    R.drawable.i09_identity,
    R.drawable.i10_kontact,
    R.drawable.i11_camera,
    R.drawable.i12_irkickflash,
    R.drawable.i13_kgpg_key3,
    R.drawable.i14_laptop_power,
    R.drawable.i15_scanner,
    R.drawable.i16_mozilla_firebird,
    R.drawable.i17_cdrom_unmount,
    R.drawable.i18_display,
    R.drawable.i19_mail_generic,
    R.drawable.i20_misc,
    R.drawable.i21_korganizer,
    R.drawable.i22_ascii,
    R.drawable.i23_icons,
    R.drawable.i24_connect_established,
    R.drawable.i25_folder_mail,
    R.drawable.i26_filesave,
    R.drawable.i27_nfs_unmount,
    R.drawable.i28_quicktime,
    R.drawable.i29_kgpg_term,
    R.drawable.i30_konsole,
    R.drawable.i31_fileprint,
    R.drawable.i32_fsview,
    R.drawable.i33_run,
    R.drawable.i34_configure,
    R.drawable.i35_krfb,
    R.drawable.i36_ark,
    R.drawable.i37_kpercentage,
    R.drawable.i38_samba_unmount,
    R.drawable.i39_history,
    R.drawable.i40_mail_find,
    R.drawable.i41_vectorgfx,
    R.drawable.i42_kcmmemory,
    R.drawable.i43_edittrash,
    R.drawable.i44_knotes,
    R.drawable.i45_cancel,
    R.drawable.i46_help,
    R.drawable.i47_kpackage,
    R.drawable.i48_folder,
    R.drawable.i49_folder_blue_open,
    R.drawable.i50_folder_tar,
    R.drawable.i51_decrypted,
    R.drawable.i52_encrypted,
    R.drawable.i53_apply,
    R.drawable.i54_signature,
    R.drawable.i55_thumbnail,
    R.drawable.i56_kaddressbook,
    R.drawable.i57_view_text,
    R.drawable.i58_kgpg,
    R.drawable.i59_package_development,
    R.drawable.i60_kfm_home,
    R.drawable.i61_services,
    R.drawable.i62_tux,
    R.drawable.i63_feather,
    R.drawable.i64_apple,
    R.drawable.i65_w,
    R.drawable.i66_money,
    R.drawable.i67_certificate,
    R.drawable.i68_phone
  )
}
