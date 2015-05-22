package com.hanhuy.android.keepshare

import java.io.{InputStream, OutputStream}

import com.hanhuy.keepassj.{StandardAesEngine, ICipherEngine}
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.io.{CipherInputStream, CipherOutputStream}
import org.bouncycastle.crypto.modes.CBCBlockCipher
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher
import org.bouncycastle.crypto.params.{ParametersWithIV, KeyParameter}

/**
 * @author pfnguyen
 */
class AesEngine extends ICipherEngine {
  override def getDisplayName = "AES"
  override def getCipherUuid = StandardAesEngine.getAesUuid

  def makeCipher(mode: Boolean, key: Array[Byte], iv: Array[Byte]): BufferedBlockCipher = {
    val aes = new NdkAESEngine
    val k = new KeyParameter(key.clone())
    val i = new ParametersWithIV(k, iv.clone())
    val cipher = new PaddedBufferedBlockCipher(new CBCBlockCipher(aes))
    cipher.init(mode, i)
    cipher
  }

  override def EncryptStream(sPlainText: OutputStream, pbKey: Array[Byte], pbIV: Array[Byte]) =
    new CipherOutputStream(sPlainText, makeCipher(true, pbKey, pbIV))
  override def DecryptStream(sEncrypted: InputStream, pbKey: Array[Byte], pbIV: Array[Byte]) =
    new CipherInputStream(sEncrypted, makeCipher(false, pbKey, pbIV))
}
