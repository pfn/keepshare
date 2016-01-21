package com.hanhuy.android.keepshare

import android.content.{SharedPreferences, Context}
import android.preference.PreferenceManager

object Setting {
  def keys = settings.values
  private var settings = Map.empty[String,Setting[_]]
  def unapply(key: String): Option[Setting[_]] = settings get key
}
sealed trait Setting[A] {
  type T = A
  def get(c: Context, p: SharedPreferences): T
  def set(p: SharedPreferences, value: T): Unit
  def key: String
  def default: T
  Setting.settings = Setting.settings + (key -> this)
}
case class StringSetting(key: String, default: String = null, defaultRes: Option[Int] = None) extends Setting[String] {
  def get(c: Context, p: SharedPreferences): String =
    p.getString(key, defaultRes.fold(default)(c.getString))
  def set(p: SharedPreferences, value: String): Unit = p.edit().putString(key, value).apply()
}
case class IntSetting(key: String, default: Int) extends Setting[Int] {
  def get(c: Context, p: SharedPreferences): Int = p.getInt(key, default)
  def set(p: SharedPreferences, value: Int): Unit = p.edit().putInt(key, value).apply()
}
case class LongSetting(key: String, default: Long) extends Setting[Long] {
  def get(c: Context, p: SharedPreferences): Long = p.getLong(key, default)
  def set(p: SharedPreferences, value: Long): Unit = p.edit().putLong(key, value).apply()
}
case class BooleanSetting(key: String, default: Boolean) extends Setting[Boolean] {
  def get(c: Context, p: SharedPreferences): Boolean = p.getBoolean(key, default)
  def set(p: SharedPreferences, value: Boolean): Unit = p.edit().putBoolean(key, value).apply()
}

object Settings {
  val KEYBOARD_TIMEOUT = IntSetting("timeout", 60)
  val PIN_TIMEOUT = IntSetting("pin_timeout", 1)
  val FIRST_RUN = BooleanSetting("first_run", true)
  val CLOUD_KEY_HASH = StringSetting("cloud_key_hash")
  val LOCAL_KEY = StringSetting("local_key")
  val VERIFY_DATA = StringSetting("verify_data")
  val DATABASE_FILE = StringSetting("database_file")
  val KEYFILE_PATH = StringSetting("key_file")
  val PASSWORD = StringSetting("password")
  val IME = StringSetting("ime")
  val PASSWORD_OVERRIDE = BooleanSetting("password_override", false)
  val NEEDS_PIN = BooleanSetting("needs_pin", false)
  val PIN_VERIFIER = StringSetting("pin_verifier", "")
  // PIN_TIMESTAMP must default to a value larger than FINGERPRINT_TIMESTAMP
  // or registration will fail
  val PIN_TIMESTAMP = LongSetting("pin_timestamp", 1l)
  val FINGERPRINT_TIMESTAMP = LongSetting("fingerprint_timestamp", 0l)
  val FINGERPRINT_PIN = StringSetting("fingerprint_pin")
  val FINGERPRINT_ENABLE = BooleanSetting("fingerprint_enable", true)

  val BROWSE_SORT_ALPHA = BooleanSetting("browse_sort_order", true)

  def apply(c: Context) = {
    new Settings(c.getApplicationContext)
  }
}

class Settings private(val context: Context) {
    val p = PreferenceManager.getDefaultSharedPreferences(context)
  def get[A](setting: Setting[A]): setting.T = setting.get(context, p)
  def set[A](setting: Setting[A], value: A) = setting.set(p, value)

  def clear() {
    Setting.keys foreach { k => p.edit.remove(k.key).commit() }
  }
}
