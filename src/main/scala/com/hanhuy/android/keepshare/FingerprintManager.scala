package com.hanhuy.android.keepshare

import java.security.spec.{MGF1ParameterSpec, X509EncodedKeySpec}
import java.security.{KeyFactory, KeyPairGenerator, KeyStore}
import javax.crypto.Cipher
import javax.crypto.spec.{OAEPParameterSpec, PSource}

import android.annotation.TargetApi
import android.content.Context
import android.hardware.fingerprint.FingerprintManager.{CryptoObject, AuthenticationResult, AuthenticationCallback}
import android.hardware.fingerprint.{FingerprintManager => FPM}
import android.os.CancellationSignal
import android.security.keystore.{KeyGenParameterSpec, KeyProperties}
import com.hanhuy.android.common.{Futures, Logcat}

import iota.v
import scala.concurrent.Future

/**
  * @author pfnguyen
  */
object FingerprintManager {
  val KEY_NAME = "keepshare-fingerprint-key"
  val CIPHER_ALG = "RSA/ECB/OAEPWithSHA-256AndMGF1Padding"
  val AKS = "AndroidKeyStore"

  sealed trait FingerprintResult
  case class FingerprintSuccess(pin: String) extends FingerprintResult
  case class FingerprintFailure(failure: CharSequence) extends FingerprintResult
  sealed trait FingerprintError extends FingerprintResult
  case object FingerprintUnavailable extends FingerprintError
  case class FingerprintException(e: Exception) extends FingerprintError
  case class FingerprintAuthenticationError(code: Int, error: CharSequence) extends FingerprintError
}
case class FingerprintManager(context: Context, settings: Settings) {
  import FingerprintManager._
  val log = Logcat("FingerprintManager")
  val fpm = _fpm

  @TargetApi(23)
  def _fpm = if (v(23)) {
    Option(context.getSystemService(classOf[FPM])) filter (m =>
      m.isHardwareDetected && m.hasEnrolledFingerprints)
  } else None

  @TargetApi(23)
  def registerPin(pin: String): Unit = {
    fpm.foreach { m =>
      import Futures._
      if (settings.get(Settings.FINGERPRINT_TIMESTAMP) < settings.get(Settings.PIN_TIMESTAMP)) Future {

        val ks = KeyStore.getInstance(AKS)
        ks.load(null)
        import KeyProperties._
        val kg = KeyPairGenerator.getInstance(KeyProperties.KEY_ALGORITHM_RSA, AKS)
        kg.initialize(
          new KeyGenParameterSpec.Builder(KEY_NAME, PURPOSE_DECRYPT)
            .setUserAuthenticationRequired(true)
            .setDigests(DIGEST_SHA256, DIGEST_SHA512)
            .setEncryptionPaddings(ENCRYPTION_PADDING_RSA_OAEP)
            .build()
        )
        // side-effects yolo..., this generates in the keystore
        val kp = kg.generateKeyPair()
        // rewrap public key, otherwise api23 bug
        val pub = kp.getPublic
        val pk = KeyFactory.getInstance(pub.getAlgorithm).generatePublic(
          new X509EncodedKeySpec(pub.getEncoded))
        val cipher = Cipher.getInstance(CIPHER_ALG)
        // see https://code.google.com/p/android/issues/detail?id=197719
        cipher.init(Cipher.ENCRYPT_MODE, pk, new OAEPParameterSpec(
          "SHA-256", "MGF1", MGF1ParameterSpec.SHA1, PSource.PSpecified.DEFAULT))
        val fkey = KeyManager.hex(cipher.doFinal(pin.getBytes("utf-8")))
        settings.set(Settings.FINGERPRINT_PIN, fkey)
        settings.set(Settings.FINGERPRINT_TIMESTAMP, System.currentTimeMillis)
      }
    }
  }

  def hasFingerprints = fpm.isDefined &&
    settings.get(Settings.FINGERPRINT_PIN) != null &&
    settings.get(Settings.FINGERPRINT_TIMESTAMP) > settings.get(Settings.PIN_TIMESTAMP) &&
    settings.get(Settings.FINGERPRINT_ENABLE)

  @TargetApi(23)
  def authenticate(): Obs[FingerprintResult] = {
    val cancelToken = new CancellationSignal
    Obs.create { sig =>
      fpm match {
        case Some(m) if hasFingerprints =>
          val ks = KeyStore.getInstance(AKS)
          ks.load(null)
          val pk = ks.getKey(KEY_NAME, null)
          val cipher = Cipher.getInstance(CIPHER_ALG)
          try {
            cipher.init(Cipher.DECRYPT_MODE, pk)
            val co = new CryptoObject(cipher)
            m.authenticate(co, cancelToken, 0, new AuthenticationCallback {
              override def onAuthenticationSucceeded(result: AuthenticationResult) = {
                val cipher = co.getCipher
                val fpin = settings.get(Settings.FINGERPRINT_PIN)
                val bytes = KeyManager.bytes(fpin)
                val pin = new String(cipher.doFinal(bytes), "utf-8")
                sig(FingerprintSuccess(pin))
                cancelToken.cancel()
              }

              override def onAuthenticationError(errorCode: Int, errString: CharSequence) = {
                sig(FingerprintAuthenticationError(errorCode, errString))
                cancelToken.cancel()
              }

              override def onAuthenticationFailed() = {
                sig(FingerprintFailure(context.getString(R.string.fingerprint_unrecognized)))
              }

              override def onAuthenticationHelp(helpCode: Int, helpString: CharSequence) = sig(FingerprintFailure(helpString))
            }, null)
          } catch {
            case e: Exception =>
              settings.set(Settings.FINGERPRINT_TIMESTAMP, 0l)
              sig(FingerprintException(e))
              cancelToken.cancel()
          }
        case _ =>
          settings.set(Settings.FINGERPRINT_TIMESTAMP, 0l)
          sig(FingerprintUnavailable)
          cancelToken.cancel()
      }
      Sub(() => cancelToken.cancel())
    }
  }

}
