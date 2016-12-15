package com.hanhuy.android.keepshare

import android.os.Bundle
import com.google.android.gms.auth.UserRecoverableAuthException
import com.google.android.gms.common.ConnectionResult
import com.google.android.gms.common.api.GoogleApiClient
import com.google.android.gms.common.api.GoogleApiClient.{ConnectionCallbacks, OnConnectionFailedListener}
import com.google.android.gms.drive.{Drive, DriveContents, DriveFile, MetadataChangeSet}
import com.google.android.gms.drive.query.{Filters, Query, SearchableField}
import com.hanhuy.android.common.{Futures, Logcat, ManagedResource, UiBus}

import collection.JavaConversions._
import java.security.{MessageDigest, SecureRandom}
import java.nio.ByteBuffer

import android.app.Activity
import android.content.Context
import java.io.{ByteArrayOutputStream, IOException}

import android.widget.Toast

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import Futures._
import ManagedResource._
import android.annotation.TargetApi
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.io.CipherOutputStream
import org.bouncycastle.crypto.modes.CBCBlockCipher
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}

import scala.util.Try

object KeyManager {

  type Key = Array[Byte]
  val VERIFIER = "KeepShare Verifier"

  val STATE_SAVE = "save"
  val STATE_LOAD = "load"

  val EXTRA_STATE = "com.hanhuy.android.keepshare.extra.STATE"

  val KEY_FILE = "keepshare.key"

  val HARDWARE_KEY_NAME = "keepshare-hardware-key"

  lazy val sha1 = MessageDigest.getInstance("SHA1")
  def sha1(b: Array[Byte]): String = hex(sha1.digest(b))

  private var _cloudFutureKey = Option.empty[Future[Either[KeyError,Key]]]

  def clear() {
    // Should clear be run on session timeout? Otherwise cloud key stays in RAM
    _cloudFutureKey = None
  }

  // prefix with 01 and strip it off, otherwise
  // prefixed 00's will be dropped from output
  // this inserts a sign byte at the beginning
  // if the first byte is > 0x7f strip it off
  def bytes(hex: String) =
    BigInt("01" + hex, 16).toByteArray.takeRight(hex.length / 2)
  //def bytes(hex: String): Array[Byte] =
  //  hex.grouped(2).map (Integer.parseInt(_, 16).asInstanceOf[Byte]).toArray

  def hex(b: Array[Byte]) =
    b.map { byte => "%02X" format (byte & 0xff) }.mkString

  val random = new SecureRandom
  val ALG = "AES"
  val CIPHER_ALG = ALG + "/CBC/PKCS7Padding"

  /** @return a hex string iv:encrypted
    */
  def encrypt(k: Key, data: Array[Byte]): String = {
    val cipher = blockCipher
    val iv = Array.ofDim[Byte](16)
    random.nextBytes(iv)
    cipher.init(true, ivParameter(keyParameter(k), iv))
    hex(iv) + ":" + hex(processCipher(cipher, data))
  }

  def encrypt(k: Key, data: String): String =
    encrypt(k, data.getBytes("utf-8"))

  def aes = new AESEngine
  def keyParameter(key: Array[Byte]) = new KeyParameter(key)
  def ivParameter(key: KeyParameter, iv: Array[Byte]) = new ParametersWithIV(key, iv)
  def blockCipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(aes))

  /** @param data iv:encrypted in hex
    * @return a byte array
    */
  def decrypt(key: Key, data: String): Either[String,Array[Byte]] = {
    data.split(":") match {
      case Array(ivs, encs) =>
        val iv = bytes(ivs)
        val encrypted = bytes(encs)
        val cipher = blockCipher
        cipher.init(false, ivParameter(keyParameter(key), iv))
        Right(processCipher(cipher, encrypted))
      case _ =>
        Application.logException(data, new Exception("bad crypto data"))
        Left(data)
    }
  }

  def processCipher(c: BufferedBlockCipher, data: Array[Byte]): Array[Byte] = {
    val bout = new ByteArrayOutputStream
    val cout = new CipherOutputStream(bout, c)
    cout.write(data, 0, data.length)
    cout.close()
    bout.toByteArray
  }

  def decryptToString(k: Key, data: String): Either[String,String] =
    decrypt(k, data).map(d => new String(d, "utf-8"))

  @TargetApi(23)
  def initHardwareKey(): Boolean = {
    if (iota.v(23)) {
      val ks = java.security.KeyStore.getInstance(FingerprintManager.AKS)
      ks.load(null)
      val created = Try(ks.getKey(HARDWARE_KEY_NAME, null)).toOption.exists(_ != null)
      if (!created) {
        import android.security.keystore.KeyProperties._
        val kg = javax.crypto.KeyGenerator.getInstance(KEY_ALGORITHM_AES, FingerprintManager.AKS)
        kg.init(
          new android.security.keystore.KeyGenParameterSpec.Builder(HARDWARE_KEY_NAME, PURPOSE_DECRYPT | PURPOSE_ENCRYPT)
            .setBlockModes(BLOCK_MODE_CBC)
            .setEncryptionPaddings(ENCRYPTION_PADDING_PKCS7)
            .setRandomizedEncryptionRequired(false) // supply our own IVs
            .build()
        )
        // side-effects yolo..., this generates in the keystore
        kg.generateKey()
      }
    }
    hasHardwareKey
  }

  @TargetApi(23)
  def resetHardwareKey(): Unit = {
    if (iota.v(23)) {
      val ks = java.security.KeyStore.getInstance(FingerprintManager.AKS)
      ks.load(null)
      Try(ks.deleteEntry(HARDWARE_KEY_NAME))
    }
  }

  @TargetApi(23)
  def hasHardwareKey: Boolean = if (iota.v(23)) {
    val ks = java.security.KeyStore.getInstance(FingerprintManager.AKS)
    ks.load(null)
    val key = ks.getKey(HARDWARE_KEY_NAME, null).asInstanceOf[javax.crypto.SecretKey]
    val kf = javax.crypto.SecretKeyFactory.getInstance(key.getAlgorithm, FingerprintManager.AKS)
    val info = kf.getKeySpec(key, classOf[android.security.keystore.KeyInfo]).asInstanceOf[android.security.keystore.KeyInfo]
    info.isInsideSecureHardware
  } else false
}
class KeyManager(c: Context, settings: Settings) {
  import RequestCodes._

  import KeyManager._
  val log = Logcat("KeyManager")

//  def accountName = credential.getSelectedAccountName
//  def accountName_=(a: String) = {
//    nameSelected = true
//    credential.setSelectedAccountName(a)
//  }

  def ready = (Seq(
    Settings.DATABASE_FILE,
    Settings.PASSWORD,
    Settings.KEYFILE_PATH,
    Settings.VERIFY_DATA) forall (settings.get(_) != null)) &&
    (!settings.get(Settings.NEEDS_PIN) || PINHolderService.instance.isDefined)

  def config: Future[Either[KeyError,(String,String,String)]] = {
    localKey map {
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

        (for {
          v <- verifier
          d <- db
          p <- pw
          k <- keyf
        } yield {
          v -> (d,p,k)
        }).left.map { e =>
          KeyError.DecryptionFailure(e)
        }.flatMap { case (v,r) =>
          if (v == KeyManager.VERIFIER)
            Right(r)
          else
            Left(KeyError.VerifyFailure(c.getString(R.string.failed_verify)))
        }
    }
  }

  lazy val googleClient = {
    val client = new GoogleApiClient.Builder(c)
      .addApi(Drive.API)
      .addScope(Drive.SCOPE_APPFOLDER)
      .build
    client.registerConnectionCallbacks(new ConnectionCallbacks {
      override def onConnectionSuspended(i: Int) = {}

      override def onConnected(bundle: Bundle) = {
        if (!clientPromise.isCompleted)
          clientPromise.success(client)
      }
    })
    client.registerConnectionFailedListener(new OnConnectionFailedListener {
      override def onConnectionFailed(r: ConnectionResult) = {
        if (r.getErrorCode == ConnectionResult.RESOLUTION_REQUIRED || r.getErrorCode == ConnectionResult.SIGN_IN_REQUIRED) {
          c match {
            case a: Activity => r.startResolutionForResult(a, RequestCodes.REQUEST_SIGN_IN)
            case _ => ()
          }
        }
      }
    })
    client
  }
  val clientPromise = Promise[GoogleApiClient]()
  def makeApiClient() = {
    googleClient.connect()
  }
  lazy val apiClient = {
    makeApiClient()
    Await.result(clientPromise.future, Duration.Inf)
  }

  private def _loadKey(): Either[KeyError,Key] = {
    val appFolder = Drive.DriveApi.getAppFolder(apiClient)
    try {
      val result = appFolder.queryChildren(apiClient,
        new Query.Builder()
          .addFilter(Filters.eq(SearchableField.TITLE, KEY_FILE))
          .build())
        .await().getMetadataBuffer
      implicit val dfileCloser = new ResourceManager[DriveContents] {
        override def dispose(resource: DriveContents) = resource.discard(apiClient)
      }
      val k = result find (_ != null) map { metadata =>
        val buf = Array.ofDim[Byte](32)
        val b = ByteBuffer.allocate(32)
        for {
          dfile <- using(Drive.DriveApi.getFile(apiClient, metadata.getDriveId)
            .open(apiClient, DriveFile.MODE_READ_ONLY, null)
            .await().getDriveContents)
          in <- using(dfile.getInputStream)
        } {
          Stream.continually(in.read(buf)).takeWhile(r => r != -1 && b.remaining >= r) foreach { r =>
            b.put(buf, 0, r)
          }
        }
        b.flip()
        if (b.remaining != 32) {
          log.e("wrong buffer size: " + b.remaining)
          Left(KeyError.LoadFailed("wrong buffer size: " + b.remaining))
        } else {
          b.get(buf)
          val hash = settings.get(Settings.CLOUD_KEY_HASH)
          if (hash != null && sha1(buf) != hash) {
            log.e("cloud key has changed")
            Left(KeyError.LoadFailed("cloud key has changed"))
          } else {
            log.v("Loaded cloud key")
            Right(buf)
          }
        }
      } getOrElse createCloudKey()
      result.release()
      k
    } catch {
      case e: UserRecoverableAuthException =>
        UiBus.post {
          Toast.makeText(Application.instance,
            Application.instance.getString(
              R.string.toast_unable_load_key, e.getMessage),
            Toast.LENGTH_SHORT).show()
        }
        requestAuthz(e, STATE_LOAD)
        Left(KeyError.LoadFailed("need authz"))
      case ex: IOException =>
        log.e("IO error loading cloud key", ex)
        UiBus.post {
          Toast.makeText(Application.instance,
            Application.instance.getString(
              R.string.toast_unable_load_key, ex.getMessage),
            Toast.LENGTH_SHORT).show()
        }
        _loadKey()
    }
  }

  def init(): Future[Either[KeyError,this.type]] = {
    if (settings.get(Settings.HARDWARE_KEY_ENABLE)) {
      Future.successful(Right(this))
    } else
      fetchCloudKey().map { e =>
        e.map(_ => this)
      }
  }

  def protectLocalKey(): Future[Either[KeyError,String]] = {
    val needsPin = settings.get(Settings.NEEDS_PIN)
    if (needsPin && PINHolderService.instance.isEmpty) Future.successful(Left(KeyError.NeedPin))
    else localKey.flatMap {
      case Left(l) => Future.successful(Left(l))
      case Right(r) =>
        val saved: Either[String,Array[Byte]] = PINHolderService.instance map { s =>
          if (needsPin) Left(encrypt(s.pinKey, r))
          else Right(r)
        } getOrElse Right(r)

        _localKey = None
        saved.fold(encryptWithExternalKey, encryptWithExternalKey)
    }
  }
  def decryptWithExternalKeyToString(data: String): Future[Either[String,String]] =
    decryptWithExternalKey(data).map(d => d.map(new String(_, "utf-8")))
  def decryptWithExternalKey(data: String): Future[Either[String,Array[Byte]]] = {
    data.split(":") match {
      case Array(ivs, encs) =>
        if (settings.get(Settings.HARDWARE_KEY_ENABLE)) {
          import javax.crypto.Cipher
          import javax.crypto.spec.IvParameterSpec
          val cipher = Cipher.getInstance(CIPHER_ALG)
          val ivspec = new IvParameterSpec(bytes(ivs))

          val ks = java.security.KeyStore.getInstance(FingerprintManager.AKS)
          ks.load(null)
          val key = ks.getKey(HARDWARE_KEY_NAME, null)
          cipher.init(Cipher.DECRYPT_MODE, key, ivspec)
          Future.successful(Right(cipher.doFinal(bytes(encs))))
        } else {
          fetchCloudKey().map(k => k.flatMap(key => KeyManager.decrypt(key, data)).left.map(e => "Key error: " + e))
        }
      case _ =>
        Application.logException(data, new Exception("bad crypto data"))
        Future.successful(Left(data))
    }
  }
  def encryptWithExternalKey(data: Array[Byte]): Future[Either[KeyError,String]] = {
    val iv = Array.ofDim[Byte](16)
    random.nextBytes(iv)
    if (settings.get(Settings.HARDWARE_KEY_ENABLE)) {
      import javax.crypto.Cipher
      import javax.crypto.spec.IvParameterSpec
      val cipher = Cipher.getInstance(CIPHER_ALG)
      val ivspec = new IvParameterSpec(iv)
      val ks = java.security.KeyStore.getInstance(FingerprintManager.AKS)
      ks.load(null)
      val key = ks.getKey(HARDWARE_KEY_NAME, null)
      cipher.init(Cipher.ENCRYPT_MODE, key, ivspec)
      Future.successful(Right(hex(iv) + ":" + hex(cipher.doFinal(data))))
    } else {
      fetchCloudKey().map(_.map { k =>
        val cipher = blockCipher
        cipher.init(true, ivParameter(keyParameter(k), iv))
        hex(iv) + ":" + hex(processCipher(cipher, data))
      })
    }
  }

  def encryptWithExternalKey(data: String): Future[Either[KeyError,String]] =
    encryptWithExternalKey(data.getBytes("utf-8"))
  def decryptWithLocalKeyToString(data: String): Future[Either[String,String]] =
    decryptWithLocalKey(data).map(d => d.map(new String(_, "utf-8")))
  def decryptWithLocalKey(data: String): Future[Either[String,Array[Byte]]] = {
    localKey.map(k => k.flatMap(key => KeyManager.decrypt(key, data)).left.map(e => "Key error: " + e))
  }
  def encryptWithLocalKey(data: Array[Byte]): Future[Either[KeyError,String]] = {
    localKey.map(_.map { k =>
      val cipher = blockCipher
      val iv = Array.ofDim[Byte](16)
      random.nextBytes(iv)
      cipher.init(true, ivParameter(keyParameter(k), iv))
      hex(iv) + ":" + hex(processCipher(cipher, data))
    })
  }

  def encryptWithLocalKey(data: String): Future[Either[KeyError,String]] = encryptWithLocalKey(data.getBytes("utf-8"))

  private def fetchCloudKey(): Future[Either[KeyError,Key]] = {
    val p = KeyManager._cloudFutureKey getOrElse {
      Future { _loadKey() }
    }
    KeyManager._cloudFutureKey = Some(p)
    p.onComplete {
      case util.Success(Right(_)) =>
      case util.Success(Left(_)) | util.Failure(_) => KeyManager._cloudFutureKey = None
    }

    p
  }

  private def createCloudKey(): Either[KeyError,Key] = {
    try {

      val keybuf = Array.ofDim[Byte](32)
      random.nextBytes(keybuf)
      settings.set(Settings.CLOUD_KEY_HASH, sha1(keybuf))
      val appFolder = Drive.DriveApi.getAppFolder(apiClient)
      val metadata = new MetadataChangeSet.Builder()
        .setMimeType("application/octet-stream")
        .setTitle(KEY_FILE).build()
      val contents = Drive.DriveApi.newDriveContents(apiClient).await().getDriveContents
      for {
        out <- using(contents.getOutputStream)
      } {
        out.write(keybuf)
      }
      appFolder.createFile(apiClient, metadata, contents)
      _loadKey()
    } catch {
      case e: UserRecoverableAuthException =>
        requestAuthz(e, STATE_SAVE)
        Left(KeyError.CreateFailed("need authz"))
    }
  }

  private var _localKey = Option.empty[Future[Either[KeyError,Key]]]

  private def localKey: Future[Either[KeyError,Key]] = _localKey.getOrElse {
    val k = settings.get(Settings.LOCAL_KEY)

    val needsPin = settings.get(Settings.NEEDS_PIN)
    val f = if (needsPin && PINHolderService.instance.isEmpty) {
      Future.successful(Left(KeyError.NeedPin))
    } else if (k == null) {
      // create key
      val keybuf = Array.ofDim[Byte](32)
      random.nextBytes(keybuf)

      val saved: Either[String,Array[Byte]] = PINHolderService.instance map { s =>
        if (needsPin) Left(encrypt(s.pinKey, keybuf))
        else Right(keybuf)
      } getOrElse Right(keybuf)

      val f = saved.fold(encryptWithExternalKey, encryptWithExternalKey)
      f.map { e =>
        e.map { k =>
          settings.set(Settings.LOCAL_KEY, k)
          keybuf
        }
      }
    } else {
      decryptWithExternalKey(k).map { ed =>
        val okey = ed.toOption
        val actual = (for {
          s <- PINHolderService.instance if settings.get(Settings.NEEDS_PIN)
          key <- okey
          st = new String(key, "utf-8")
          k <- decrypt(s.pinKey, st).toOption
        } yield k) orElse okey
        actual map { a =>
          Right(a)
        } getOrElse Left(KeyError.NeedClear)
      }
    }

    _localKey = Some(f)
    f
  }

  private def requestAuthz(e: UserRecoverableAuthException, state: String) {
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
  case object NeedPin extends Exception("Need PIN") with KeyError
  case class VerifyFailure(error: String) extends KeyError
  case class AuthFailure(error: String) extends Exception(error) with KeyError
  case object NeedLoad extends Exception("Need to load") with KeyError
  case object NeedClear extends KeyError
  case object NeedSetup extends Exception("Need setup") with KeyError
  case class LoadFailed(s: String) extends Exception(s) with KeyError
  case class CreateFailed(s: String) extends Exception(s) with KeyError
  case object NotReady extends Exception("Not ready") with KeyError
  case class DecryptionFailure(s: String) extends KeyError
}
