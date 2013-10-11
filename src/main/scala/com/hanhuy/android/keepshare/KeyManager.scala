package com.hanhuy.android.keepshare

import RichLogger._

import collection.JavaConversions._

import java.security.{Key, SecureRandom, MessageDigest}
import com.google.api.client.googleapis.extensions.android.gms.auth._
import com.google.api.client.json.gson.GsonFactory
import com.google.api.services.drive.{Drive, DriveScopes}
import com.google.api.client.extensions.android.http.AndroidHttp
import com.google.api.client.http.{ByteArrayContent, GenericUrl}
import java.nio.ByteBuffer
import com.google.api.services.drive.model.{File, ParentReference}
import android.app.Activity
import javax.crypto.{SecretKey, Cipher}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import android.content.Context
import java.io.IOException
import android.widget.Toast

object KeyManager {
  val VERIFIER = "KeepShare Verifier"

  val STATE_SAVE = "save"
  val STATE_LOAD = "load"

  val EXTRA_STATE = "com.hanhuy.android.keepshare.extra.STATE"

  val KEY_FILE = "keepshare.key"

  lazy val sha1 = MessageDigest.getInstance("SHA1")
  def sha1(b: Array[Byte]): String = hex(sha1.digest(b))

  private var _cloudKey: SecretKey = _

  def cloudKey = _cloudKey

  def clear() {
    _cloudKey = null
  }

  // prefix with 01 and strip it off, otherwise
  // prefixed 00's will be dropped from output
  // this inserts a sign byte at the beginning
  // if the first byte is > 0x7f strip it off
  def bytes(hex: String) =
    BigInt("01" + hex, 16).toByteArray.takeRight(hex.size / 2)
  //def bytes(hex: String): Array[Byte] =
  //  hex.grouped(2).map (Integer.parseInt(_, 16).asInstanceOf[Byte]).toArray

  def hex(b: Array[Byte]) =
    b.map { byte => "%02X" format (byte & 0xff) }.mkString

  val random = new SecureRandom
  val ALG = "AES"
  val CIPHER_ALG = ALG + "/CBC/PKCS5Padding"

  /** @return a hex string iv:encrypted
    */
  def encrypt(k: Key, data: Array[Byte]): String = {
    val cipher = Cipher.getInstance(CIPHER_ALG)
    val iv = Array.ofDim[Byte](16)
    random.nextBytes(iv)
    val ivspec = new IvParameterSpec(iv)
    cipher.init(Cipher.ENCRYPT_MODE, k, ivspec, random)
    hex(iv) + ":" + hex(cipher.doFinal(data))
  }

  def encrypt(k: Key, data: String): String =
    encrypt(k, data.getBytes("utf-8"))

  /** @param data iv:encrypted in hex
    * @return a byte array
    */
  def decrypt(k: Key, data: String): Array[Byte] = {
    val cipher = Cipher.getInstance(CIPHER_ALG)
    data.split(":") match {
      case Array(ivs, encs) =>
        val iv = bytes(ivs)
        val encrypted = bytes(encs)
        val ivspec = new IvParameterSpec(iv)
        cipher.init(Cipher.DECRYPT_MODE, k, ivspec)
        cipher.doFinal(encrypted)
    }
  }

  def decryptToString(k: Key, data: String): String =
    new String(decrypt(k, data), "utf-8")
}
class KeyManager(c: Context, settings: Settings) {
  import RequestCodes._

  import KeyManager._
  implicit val TAG = LogcatTag("KeyManager")

  private var nameSelected = false
  def accountName = credential.getSelectedAccountName
  def accountName_=(a: String) = {
    nameSelected = true
    credential.setSelectedAccountName(a)
  }

  def newChooseAccountIntent = credential.newChooseAccountIntent()

  def ready = (Seq(Settings.GOOGLE_USER,
    Settings.DATABASE_FILE,
    Settings.PASSWORD,
    Settings.KEYFILE_PATH,
    Settings.VERIFY_DATA) forall (settings.get(_) != null)) &&
    (!settings.get(Settings.NEEDS_PIN) || PINHolderService.instance.isDefined)

  def getConfig: Either[KeyError,(String,String,String)] = {
    localKey match {
      case Left(error) => Left(error)
      case Right(key) =>
        val db = KeyManager.decryptToString(
          key, settings.get(Settings.DATABASE_FILE))
        val pw = KeyManager.decryptToString(
          key, settings.get(Settings.PASSWORD))
        val keyf = KeyManager.decryptToString(
          key, settings.get(Settings.KEYFILE_PATH))
        val verifier = KeyManager.decryptToString(
          key, settings.get(Settings.VERIFY_DATA))
        if (verifier != KeyManager.VERIFIER)
          Left(KeyError.VerifyFailure(c.getString(R.string.failed_verify)))
        else
          Right((db, pw, keyf))
    }
  }

  private lazy val credential = GoogleAccountCredential.usingOAuth2(c,
    Seq(DriveScopes.DRIVE_APPDATA))

  lazy val drive = new Drive.Builder(AndroidHttp.newCompatibleTransport,
    new GsonFactory, credential).build

  def loadKey(): SecretKey = {

    Option(KeyManager.cloudKey) getOrElse {
      if (!nameSelected)
        throw new IllegalStateException("account name has not been set")
      val req = drive.files.list
      req.setQ("'appdata' in parents")
      try {
        val files = req.execute()
        files.getItems find (_.getTitle == KEY_FILE) map {
          file =>
            val resp = drive.getRequestFactory.buildGetRequest(
              new GenericUrl(file.getDownloadUrl)).execute
            val buf = Array.ofDim[Byte](32)
            val in = resp.getContent
            val b = ByteBuffer.allocate(32)
            Stream.continually(in.read(buf)).takeWhile(_ != -1) foreach { r =>
              b.put(buf, 0, r)
            }
            b.flip()
            if (b.remaining != 32) {
              e("wrong buffer size: " + b.remaining)
              return null
            }
            b.get(buf)
            val hash = settings.get(Settings.CLOUD_KEY_HASH)
            if (hash != null && sha1(buf) != hash) {
              e("cloud key has changed")
              return null
            }
            _cloudKey = new SecretKeySpec(buf, ALG)
            v("Loaded cloud key")
        } getOrElse createKey()
      } catch {
        case e: IOException =>
        case e: UserRecoverableAuthIOException => requestAuthz(e, STATE_LOAD)
      }
      KeyManager.cloudKey
    }
  }

  def createKey(): SecretKey = {
    val keybuf = Array.ofDim[Byte](32)
    random.nextBytes(keybuf)
    settings.set(Settings.CLOUD_KEY_HASH, sha1(keybuf))

    val content = new ByteArrayContent("application/octet-stream", keybuf)
    val f = new File
    f.setTitle(KEY_FILE)
    f.setParents(Seq(new ParentReference().setId("appdata")))
    try {
      val r = drive.files.insert(f, content).execute()
      loadKey()
    } catch {
      case e: UserRecoverableAuthIOException =>
        requestAuthz(e, STATE_SAVE)
        null
    }
  }

  def localKey: Either[KeyError,SecretKey] = {
    if (cloudKey == null)
      throw new IllegalStateException("Cloud Key must be loaded first")

    val k = settings.get(Settings.LOCAL_KEY)

    val needsPin = settings.get(Settings.NEEDS_PIN)
    if (needsPin && PINHolderService.instance.isEmpty) {
      Left(KeyError.NeedPin)
    } else if (k == null) {
      val keybuf = Array.ofDim[Byte](32)
      random.nextBytes(keybuf)

      val saved = PINHolderService.instance map { s =>
        bytes(encrypt(s.pinKey, keybuf)) } getOrElse keybuf

      val k = encrypt(cloudKey, saved)

      settings.set(Settings.LOCAL_KEY, k)
      Right(new SecretKeySpec(keybuf, ALG))
    } else {
      val key = decrypt(cloudKey, k)
      val actual = PINHolderService.instance map { s =>
        val st = new String(key, "utf-8")
        decrypt(s.pinKey, st)
      } getOrElse key
      Right(new SecretKeySpec(actual, ALG))
    }
  }

  private def requestAuthz(e: UserRecoverableAuthIOException, state: String) {
    val i = e.getIntent
    i.putExtra(EXTRA_STATE, state)
    c match {
      case a: Activity =>
        a.startActivityForResult(i, REQUEST_AUTHORIZATION)
      case _ =>
    }
  }
}

sealed trait KeyError
object KeyError {
  case object NeedPin extends KeyError
  case class VerifyFailure(error: String) extends KeyError
}
