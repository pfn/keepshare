package com.hanhuy.android.keepshare

import android.content.Context
import android.preference.PreferenceManager

import scala.reflect.ClassTag

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
  val KEYBOARD_TIMEOUT = new Setting[Int]("timeout", 60, None)
  val PIN_TIMEOUT = new Setting[Int]("pin_timeout", 1, None)
  val FIRST_RUN = Setting[Boolean]("first_run", true)
  val CLOUD_KEY_HASH = Setting[String]("cloud_key_hash", null)
  val LOCAL_KEY = Setting[String]("local_key", null)
  val VERIFY_DATA = Setting[String]("verify_data", null)
  val DATABASE_FILE = Setting[String]("database_file", null)
  val KEYFILE_PATH = Setting[String]("key_file", null)
  val PASSWORD = Setting[String]("password", null)
  val IME = Setting[String]("ime", null)
  val PASSWORD_OVERRIDE = Setting[Boolean]("password_override", false)
  val NEEDS_PIN = Setting[Boolean]("needs_pin", false)
  val PIN_VERIFIER = Setting[String]("pin_verifier", "")

  def apply(c: Context) = {
    new Settings(c.getApplicationContext)
  }
}

class Settings(val context: Context) {
    val p = PreferenceManager.getDefaultSharedPreferences(context)

  def get[A](setting: Setting[A])(implicit m: ClassTag[A]): A = {
    // :Any is necessary, or else Int turns into Float?!
    val result: Any = if (classOf[String] == m.runtimeClass) {
      val default: String = setting.defaultRes map {
        context.getString
      } getOrElse setting.default.asInstanceOf[String]
      p.getString(setting.key, default)
    } else if (classOf[Boolean] == m.runtimeClass) {
      p.getBoolean(setting.key, setting.default.asInstanceOf[Boolean])
    } else if (classOf[Float] == m.runtimeClass) {
      p.getFloat(setting.key, setting.default.asInstanceOf[Float])
    } else if (classOf[Long] == m.runtimeClass) {
      p.getLong(setting.key, setting.default.asInstanceOf[Long])
    } else if (classOf[Int] == m.runtimeClass) {
      p.getInt(setting.key, setting.default.asInstanceOf[Int])
    } else {
      throw new IllegalArgumentException("Unknown type: " + m.runtimeClass)
    }
    result.asInstanceOf[A]
  }
  def set[A](setting: Setting[A], value: A)(implicit m: ClassTag[A]) {
    val editor = p.edit()
    if (classOf[Boolean] == m.runtimeClass) {
      editor.putBoolean(setting.key, value.asInstanceOf[Boolean])
    } else if (classOf[String] == m.runtimeClass) {
        editor.putString(setting.key, value.asInstanceOf[String])
    } else if (classOf[Int] == m.runtimeClass) {
      editor.putInt(setting.key, value.asInstanceOf[Int])
    } else {
      throw new IllegalArgumentException("Unknown type: " + m.runtimeClass)
    }
    editor.commit()
  }

  def clear() {
    Setting.keys foreach { k => p.edit.remove(k.key).commit() }
  }
}
