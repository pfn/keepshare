package com.hanhuy.android.keepshare

import java.io.{FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import android.annotation.TargetApi
import android.content.Intent
import android.net.Uri
import android.os.SystemClock
import android.provider.DocumentsContract
import com.google.common.io.BaseEncoding
import com.hanhuy.android.common.{ManagedResource, Futures, ServiceBus}
import com.hanhuy.keepassj.AesEngines.KeyTransformer
import com.hanhuy.keepassj._
import com.hanhuy.keepassj.spr.{SprEventArgs, SprEngine, SprContext, SprCompileFlags}
import org.acra.ACRA
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.{SHA256Digest, SHA512Digest, SHA1Digest}
import org.bouncycastle.crypto.macs.HMac

import scala.concurrent.{Promise, Future}

import Futures._
import ManagedResource._

import scala.util.Try

/**
 * @author pfnguyen
 */
object Database {

  sealed trait Otp
  case class Totp(step: Int) extends Otp
  case class Hotp(counter: Long) extends Otp

  case class TotpData(key: Array[Byte], t0: Long, step: Int, size: Int, hmac: HMac)
  def extractTotpData(e: PwEntry): TotpData = {
    val strings = e.getStrings
    def getKey(k: String, f: String => Array[Byte]): Option[Array[Byte]] =
      Option(strings.Get(k)).map(ps => f(ps.ReadString()))
    def getValue[A](k: String, default: A)(f: String => Option[A]): A =
      Option(strings.Get(k)).flatMap(ps => f(ps.ReadString())).getOrElse(default)
    val key =
      getKey("HmacOtp-Secret",        s => s.getBytes(StrUtil.Utf8))                    orElse
        getKey("HmacOtp-Secret-Hex",    s => MemUtil.HexStringToByteArray(s.toUpperCase)) orElse
        getKey("HmacOtp-Secret-Base32", s => MemUtil.ParseBase32(s.toUpperCase))          orElse
        getKey("HmacOtp-Secret-Base64", s => BaseEncoding.base64().decode(s.toUpperCase)) getOrElse
        Array.ofDim[Byte](0)

    val hmac = new HMac(getValue[Digest]("HmacOtp-Algorithm", new SHA1Digest) {
      case "SHA256"   => Some(new SHA256Digest)
      case "SHA512"   => Some(new SHA512Digest)
      case "SHA1" | _ => Some(new SHA1Digest)
    })
    val t0 = getValue("TimeOtp-T0",     0l)(s => Try(s.toLong).toOption)
    val step = getValue("TimeOtp-Step", 30)(s => Try(s.toInt).toOption)
    val size = getValue("TimeOtp-Size",  6)(s => Try(s.toInt).toOption)
    TotpData(key, t0, step, size, hmac)
  }
  SprEngine.FilterCompile.add(new EventHandler[SprEventArgs] {
    val TOTP_PLACEHOLDER = "{TOTP}"
    override def delegate(sender: Any, e: SprEventArgs) = {
      if (e.getContext.getFlags.contains(SprCompileFlags.ExtActive)) {
        if (e.getText.toUpperCase.contains(TOTP_PLACEHOLDER)) {
          val data = extractTotpData(e.getContext.getEntry)
          val t = System.currentTimeMillis / 1000
          e.setText(StrUtil.ReplaceCaseInsensitive(e.getText, TOTP_PLACEHOLDER,
            HmacOtp.Generate(data.hmac, data.key, (t - data.t0) / data.step, data.size, false, -1)))
        }
      }
    }
  })
  lazy val writeSupported =
    Application.instance.getPackageName == "com.hanhuy.android.keepshare"

  NativeKeyTransformer.init()

  def rootGroup = database map (_.getRootGroup)
  def rootGroupId = rootGroup map (_.getUuid)
  def recycleBinId = database map (_.getRecycleBinUuid)
  def recycleBin = for {
    root <- rootGroup
    id   <- recycleBinId
    r    <- Option(root.FindGroup(id, true))
  } yield r

  private var database = Option.empty[PwDatabase]
  private var databasePath = Option.empty[String]
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

      var lastModified = Option.empty[Long]
      for {
        c <- using(cr.query(uri, null, null, null, null))
      } {
        val cLastModified = c.getColumnIndex(
          DocumentsContract.Document.COLUMN_LAST_MODIFIED)
        Stream.continually(c.moveToNext) takeWhile identity foreach { _ =>
          lastModified = Some(c.getLong(cLastModified))
        }
      }

      if (lastModified exists (_ > dest.lastModified)) {
        for {
          input <- using(cr.openInputStream(uri))
          out   <- using(new FileOutputStream(dest))
        } {
          var total = 0
          val buf = Array.ofDim[Byte](32768)
          Stream.continually(input.read(buf, 0, 32768)) takeWhile (_ != -1) foreach { read =>
            total += read
            out.write(buf, 0, read)
          }
        }
      }
      cr.takePersistableUriPermission(uri,
        Intent.FLAG_GRANT_READ_URI_PERMISSION |
          Intent.FLAG_GRANT_WRITE_URI_PERMISSION)
      dest.getAbsolutePath
    } else Promise.successful(f).future
  }
  def open(db: String, pw: Option[String], keyfile: Option[String]): Future[PwDatabase] = {
    databasePath = Option(db)
    close()

    def checkedOpen(p: String, keyf: Option[String]) = {
      val pwdb = new PwDatabase
      val key = new CompositeKey

      pw find (_.nonEmpty) foreach { p => key.AddUserKey(new KcpPassword(p)) }
      keyf find (_.nonEmpty) foreach { f => key.AddUserKey(new KcpKeyFile(f)) }

      val start = SystemClock.uptimeMillis
      try {
        pwdb.Open(IOConnectionInfo.FromPath(p), key, null)
        val end = SystemClock.uptimeMillis
        android.util.Log.v("Database", s"open time: ${end - start}ms")
        database = Some(pwdb)
        Future.successful(pwdb)
      } catch {
        case e: OutOfMemoryError =>
          ACRA.getErrorReporter.handleSilentException(e)
          Future.failed(e)
      }
    }
    for {
      p    <- resolvePath(db)
      keyf <- Futures.traverseO(keyfile)(resolvePath)
      db   <- checkedOpen(p, keyf)
    } yield db
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

  def getUuid(entry: PwEntry) = entry.getUuid.ToHexString
  def getId(entry: PwEntry) = ByteBuffer.wrap(entry.getUuid.getUuidBytes).getLong
  def getId(group: PwGroup) = ByteBuffer.wrap(group.getUuid.getUuidBytes).getLong

  def save(): Future[PwDatabase] = {
    (for {
      d    <- database
      path <- databasePath
    } yield {
      resolvePath(path) map { p =>
        val mergeIn = new PwDatabase
        try {
          mergeIn.Open(IOConnectionInfo.FromPath(p), d.getMasterKey, null)
        } catch {
          case e: OutOfMemoryError =>
            ACRA.getErrorReporter.handleSilentException(e)
            throw new IllegalArgumentException(e)
        }
        d.MergeIn(mergeIn, PwMergeMethod.Synchronize)
        mergeIn.Close()
        d.Save(null)
        if (path startsWith "content:") {
          for {
            in  <- using(new FileInputStream(p))
            out <- using(Application.instance.getContentResolver.openOutputStream(Uri.parse(path)))
          } {
            val buf = Array.ofDim[Byte](32768)
            Stream.continually(in.read(buf, 0, 32768)) takeWhile (_ != -1) foreach { read =>
              out.write(buf, 0, read)
            }
          }
        }
        d
      }
    }) getOrElse Future.failed(KeyError.NeedLoad)
  }

  def emptyRecycleBin(): Unit = {
    for {
      db  <- database
      bin <- recycleBin
    } {
      bin.DeleteAllObjects(db)
    }
  }

  def delete(group: PwGroup): Unit = {
    val now = new java.util.Date
    for {
      db <- database
      r  <- ensureRecycleBin(db)
      p  <- Option(group.getParentGroup)
    } {
      p.getGroups.Remove(group)
      if (db.isRecycleBinEnabled) {
        if (p == r || p.IsContainedIn(r)) {
          deletePermanent(group, db, now)
        } else {
          r.AddGroup(group, true, true)
          group.Touch(false)
        }
      } else {
        deletePermanent(group, db, now)
      }
    }
  }

  def delete(entries: PwEntry*): Unit = {
    val now = new java.util.Date
    for {
      db <- database
      r  <- ensureRecycleBin(db)
    } {
      val del = if (db.isRecycleBinEnabled) (e: PwEntry) => {
        Option(e.getParentGroup) foreach { p =>
          p.getEntries.Remove(e)
          if (p == r || p.IsContainedIn(r)) {
            deletePermanent(e, db, now)
          } else {
            r.AddEntry(e, true, true)
            e.Touch(false)
          }
        }
      }
      else (e: PwEntry) => {
        deletePermanent(e, db, now)
      }

      entries foreach del
    }
  }

  private[this] def deletePermanent(e: PwEntry, db: PwDatabase, now: java.util.Date) {
    Option(e.getParentGroup) foreach { p =>
      p.getEntries.Remove(e)
      val pdo = new PwDeletedObject(e.getUuid, now)
      db.getDeletedObjects.Add(pdo)
    }
  }
  private[this] def deletePermanent(g: PwGroup, db: PwDatabase, now: java.util.Date) {
    Option(g.getParentGroup) foreach { p =>
      p.getGroups.Remove(g)
      g.DeleteAllObjects(db)
      val pdo = new PwDeletedObject(g.getUuid, now)
      db.getDeletedObjects.Add(pdo)
    }
  }

  private[this] def ensureRecycleBin(db: PwDatabase): Option[PwGroup] = {
    for {
      root <- rootGroup
      id   <- recycleBinId
    } yield {
      Option(root.FindGroup(id, true)) getOrElse {
        val group = new PwGroup(true, true, "Recycle Bin", PwIcon.TrashBin)
        group.setEnableAutoType(false)
        group.setEnableSearching(false)
        group.setExpanded(false)
        db.getRootGroup.AddGroup(group, true)
        db.setRecycleBinUuid(group.getUuid)
        group
      }
    }
  }

  val Icons = Vector(
    R.mipmap.i00_password,
    R.mipmap.i01_package_network,
    R.mipmap.i02_messagebox_warning,
    R.mipmap.i03_server,
    R.mipmap.i04_klipper,
    R.mipmap.i05_edu_languages,
    R.mipmap.i06_kcmdf,
    R.mipmap.i07_kate,
    R.mipmap.i08_socket,
    R.mipmap.i09_identity,
    R.mipmap.i10_kontact,
    R.mipmap.i11_camera,
    R.mipmap.i12_irkickflash,
    R.mipmap.i13_kgpg_key3,
    R.mipmap.i14_laptop_power,
    R.mipmap.i15_scanner,
    R.mipmap.i16_mozilla_firebird,
    R.mipmap.i17_cdrom_unmount,
    R.mipmap.i18_display,
    R.mipmap.i19_mail_generic,
    R.mipmap.i20_misc,
    R.mipmap.i21_korganizer,
    R.mipmap.i22_ascii,
    R.mipmap.i23_icons,
    R.mipmap.i24_connect_established,
    R.mipmap.i25_folder_mail,
    R.mipmap.i26_filesave,
    R.mipmap.i27_nfs_unmount,
    R.mipmap.i28_quicktime,
    R.mipmap.i29_kgpg_term,
    R.mipmap.i30_konsole,
    R.mipmap.i31_fileprint,
    R.mipmap.i32_fsview,
    R.mipmap.i33_run,
    R.mipmap.i34_configure,
    R.mipmap.i35_krfb,
    R.mipmap.i36_ark,
    R.mipmap.i37_kpercentage,
    R.mipmap.i38_samba_unmount,
    R.mipmap.i39_history,
    R.mipmap.i40_mail_find,
    R.mipmap.i41_vectorgfx,
    R.mipmap.i42_kcmmemory,
    R.mipmap.i43_edittrash,
    R.mipmap.i44_knotes,
    R.mipmap.i45_cancel,
    R.mipmap.i46_help,
    R.mipmap.i47_kpackage,
    R.mipmap.i48_folder,
    R.mipmap.i49_folder_blue_open,
    R.mipmap.i50_folder_tar,
    R.mipmap.i51_decrypted,
    R.mipmap.i52_encrypted,
    R.mipmap.i53_apply,
    R.mipmap.i54_signature,
    R.mipmap.i55_thumbnail,
    R.mipmap.i56_kaddressbook,
    R.mipmap.i57_view_text,
    R.mipmap.i58_kgpg,
    R.mipmap.i59_package_development,
    R.mipmap.i60_kfm_home,
    R.mipmap.i61_services,
    R.mipmap.i62_tux,
    R.mipmap.i63_feather,
    R.mipmap.i64_apple,
    R.mipmap.i65_w,
    R.mipmap.i66_money,
    R.mipmap.i67_certificate,
    R.mipmap.i68_phone
  )
}

object NativeKeyTransformer extends KeyTransformer {
  def init(): Unit = {
    try {
      System.loadLibrary("aeskeytrans")
      AesEngines.setKeyTransformer(this)
    } catch {
      case t: Throwable => // ignore
    }
  }
  @native
  def transformKey(key: Array[Byte], seed: Array[Byte], rounds: Long): Boolean
}
