package com.hanhuy.android.keepshare

import android.content.Context
import android.preference.PreferenceManager

object Setting {
  def keys = settings.values
  private var settings = Map.empty[String,Setting[_]]
  def unapply(key: String): Option[Setting[_]] = settings get key
  def apply[A](key: String, default: A) = new Setting(key, default, None)
  def apply[A](key: String, res: Int) = new Setting(key,
    null.asInstanceOf[A], Some(res))
}
class Setting[A](val key: String, val default: A, val defaultRes: Option[Int]) {
  Setting.settings = Setting.settings + (key -> this)
}

object Settings {
  val GOOGLE_USER = Setting[String]("google_account", null)
  val CLOUD_KEY_HASH = Setting[String]("cloud_key_hash", null)
  val LOCAL_KEY = Setting[String]("local_key", null)
  val VERIFY_DATA = Setting[String]("verify_data", null)
  val DATABASE_FILE = Setting[String]("database_file", null)
  val KEYFILE_PATH = Setting[String]("key_file", null)
  val PASSWORD = Setting[String]("password", null)

  def apply(c: Context) = {
    new Settings(c.getApplicationContext)
  }
}

class Settings(val context: Context) {
    val p = PreferenceManager.getDefaultSharedPreferences(context)

  def get[A](setting: Setting[A])(implicit m: ClassManifest[A]): A = {
    val result = if (classOf[String] == m.erasure) {
      val default: String = setting.defaultRes map {
        context.getString
      } getOrElse setting.default.asInstanceOf[String]
      p.getString(setting.key, default)
    } else if (classOf[Boolean] == m.erasure) {
      p.getBoolean(setting.key, setting.default.asInstanceOf[Boolean])
    } else if (classOf[Float] == m.erasure) {
      p.getFloat(setting.key, setting.default.asInstanceOf[Float])
    } else if (classOf[Long] == m.erasure) {
      p.getLong(setting.key, setting.default.asInstanceOf[Long])
    } else if (classOf[Int] == m.erasure) {
      p.getInt(setting.key, setting.default.asInstanceOf[Int])
    } else {
      throw new IllegalArgumentException("Unknown type: " + m.erasure)
    }
    result.asInstanceOf[A]
  }
  def set[A](setting: Setting[A], value: A)(implicit m: ClassManifest[A]) {
    val editor = p.edit()
    if (classOf[Boolean] == m.erasure) {
      editor.putBoolean(setting.key, value.asInstanceOf[Boolean])
    } else if (classOf[String] == m.erasure) {
        editor.putString(setting.key, value.asInstanceOf[String])
    } else {
      throw new IllegalArgumentException("Unknown type: " + m.erasure)
    }
    editor.commit()
  }

  def clear() {
    Setting.keys foreach { k => p.edit.remove(k.key).commit() }
  }
}
