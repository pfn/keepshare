package com.hanhuy.android.keepshare

import java.io.FileInputStream
import java.nio.ByteBuffer

import com.hanhuy.keepassj._
import com.hanhuy.keepassj.spr.{SprEngine, SprContext, SprCompileFlags}

/**
 * @author pfnguyen
 */
object Database {
  private var database = Option.empty[PwDatabase]

  def isOpen = database.nonEmpty

  def open(db: String, pw: Option[String], keyfile: Option[String]) = {
    close()
    val pwdb = new PwDatabase
    val key = new CompositeKey

    pw find (_.nonEmpty) foreach { p => key.AddUserKey(new KcpPassword(p)) }
    keyfile find (_.nonEmpty) foreach { f => key.AddUserKey(new KcpKeyFile(f)) }

    pwdb.setMasterKey(key)
    val file = new KdbxFile(pwdb)
    val in = new FileInputStream(db)
    try {
      file.Load(new FileInputStream(db), KdbxFormat.Default, null)
    } finally {
      in.close()
    }

    database = Some(pwdb)
    true
  }

  def close(): Unit = {
    database foreach { _.Close() }
    database = None
  }

  def search(q: String) = {
    database flatMap { db =>
      val p = SearchParameters.getNone
      p.setSearchInTitles(true)
      p.setSearchInUrls(true)
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
}
