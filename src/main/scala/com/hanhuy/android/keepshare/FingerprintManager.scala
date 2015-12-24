package com.hanhuy.android.keepshare

import java.security.spec.X509EncodedKeySpec
import java.security.{KeyFactory, KeyPairGenerator, KeyStore}
import javax.crypto.Cipher

import android.annotation.TargetApi
import android.content.Context
import android.hardware.fingerprint.FingerprintManager.{CryptoObject, AuthenticationResult, AuthenticationCallback}
import android.hardware.fingerprint.{FingerprintManager => FPM}
import android.os.CancellationSignal
import android.security.keystore.{KeyGenParameterSpec, KeyProperties}
import com.hanhuy.android.common.Logcat

import iota.v
import rx.lang.scala.{Subscription, Observable}

import scala.concurrent.Future

/**
  * @author pfnguyen
  */
object FingerprintManager {
  val KEY_NAME = "keepshare-fingerprint-key"
  val CIPHER_ALG = "RSA/ECB/OAEPWithSHA-256AndMGF1Padding"
  val AKS = "AndroidKeyStore"
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
      if (settings.get(Settings.FINGERPRINT_TIMESTAMP) > settings.get(Settings.PIN_TIMESTAMP)) {
        // for backward compat, always reset pin's timestamp if fingerprint older
        settings.set(Settings.PIN_TIMESTAMP, System.currentTimeMillis)

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
        val kp = kg.generateKeyPair() // side-effects ftw, this places it in the keystore automatically
        // rewrap public key, otherwise api23 bug
        val pk = KeyFactory.getInstance(kp.getPublic.getAlgorithm).generatePublic(
          new X509EncodedKeySpec(kp.getPublic.getEncoded))
        val cipher = Cipher.getInstance(CIPHER_ALG)

        cipher.init(Cipher.ENCRYPT_MODE, pk)
        val fkey = KeyManager.hex(cipher.doFinal(pin.getBytes("utf-8")))
        settings.set(Settings.FINGERPRINT_PIN, fkey)
        settings.set(Settings.FINGERPRINT_TIMESTAMP, System.currentTimeMillis)
      }
    }
  }

  def hasFingerprints = fpm.isDefined &&
    settings.get(Settings.FINGERPRINT_PIN) != null &&
    settings.get(Settings.FINGERPRINT_TIMESTAMP) > settings.get(Settings.PIN_TIMESTAMP)

  @TargetApi(23)
  def authenticate(): Observable[Either[CharSequence,String]] = {
    val cancelToken = new CancellationSignal
    Observable.create {obs =>
      fpm match {
        case Some(m) if hasFingerprints =>
          val ks = KeyStore.getInstance(AKS)
          ks.load(null)
          val pk = ks.getKey(KEY_NAME, null)
          val cipher = Cipher.getInstance(CIPHER_ALG)
          cipher.init(Cipher.DECRYPT_MODE, pk)
          val co = new CryptoObject(cipher)
          m.authenticate(co, cancelToken, 0, new AuthenticationCallback {
            override def onAuthenticationSucceeded(result: AuthenticationResult) = {
              val cipher = co.getCipher
              val fpin = settings.get(Settings.FINGERPRINT_PIN)
              val bytes = KeyManager.bytes(fpin)
              val pin = new String(cipher.doFinal(bytes), "utf-8")
              obs.onNext(Right(pin))
              log.v("Successful authentication: " + pin)
              obs.onCompleted()
              cancelToken.cancel()
            }

            override def onAuthenticationError(errorCode: Int, errString: CharSequence) = {
              log.v("Authentication error: " + errString)
              obs.onError(FingerprintAuthenticationError(errorCode, errString))
              cancelToken.cancel()
            }

            override def onAuthenticationFailed() = {
              log.v("Authentication failed")
              obs.onNext(Left(context.getString(R.string.fingerprint_unrecognized)))
            }

            override def onAuthenticationHelp(helpCode: Int, helpString: CharSequence) = obs.onNext(Left(helpString))
          }, null)
        case _ =>
          obs.onError(FingerprintUnavailable)
          cancelToken.cancel()
      }
      Subscription(cancelToken.cancel())
    }
  }

  sealed trait FingerprintError extends Exception
  case object FingerprintUnavailable extends FingerprintError
  case object FingerprintAuthenticationFailed extends FingerprintError
  case class FingerprintAuthenticationError(code: Int, error: CharSequence) extends FingerprintError
}
