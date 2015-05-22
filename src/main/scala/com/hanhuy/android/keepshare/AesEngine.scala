package com.hanhuy.android.keepshare

import java.io.{InputStream, OutputStream}

import com.hanhuy.keepassj.{StandardAesEngine, ICipherEngine}
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.io.{CipherInputStream, CipherOutputStream}
import org.bouncycastle.crypto.modes.CBCBlockCipher
import org.bouncycastle.crypto.paddings.{PKCS7Padding, PaddedBufferedBlockCipher}
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
    new AesInputStream(sEncrypted,pbKey, pbIV)
}

class AesInputStream(in: InputStream, key: Array[Byte], iv: Array[Byte]) extends InputStream {
  val bufsize = 32768
  val readbuffer = Array.ofDim[Byte](bufsize)
  var pos = 0
  var count = 0
  val currentIV = iv.clone()
  val keyschedule = Array.ofDim[Int](60)
  var eof = false
  NdkAESEngine.scheduleKey(key, keyschedule)
  override def read() = {
    val b = Array.ofDim[Byte](1)
    val r = read(b)
    if (r == -1) -1 else b(0)
  }
  override def read(buffer: Array[Byte]) = read(buffer, 0, buffer.length)
  override def close() = in.close()

  override def read(buffer: Array[Byte], byteOffset: Int, byteCount: Int) = {
    if (eof && count == 0) -1 else if (byteCount == 0) byteCount else {
      if (byteCount > bufsize) throw new IllegalArgumentException("Unsupported count: " + byteCount)
      System.arraycopy(readbuffer, pos, readbuffer, 0, count)
      pos = 0

      val read = in.read(readbuffer, count, bufsize - count)
      System.arraycopy(readbuffer, count + read - 16, currentIV, 0, 16)
      if (!NdkAESEngine.decrypt_cbc(readbuffer, count, readbuffer, count, read, keyschedule, key.length * 8, currentIV)) {
        throw new IllegalArgumentException("Bad input buffer size: " + count)
      }
      count = count + read
      if (read < bufsize - count) {
        // assume eof
        eof = true
      }
      val c = math.min(count, byteCount)
      System.arraycopy(readbuffer, pos, buffer, byteOffset, c)

      count = count - c
      pos = c
      c
    }
  }
}

class AesOutputStream(out: OutputStream, key: Array[Byte], iv: Array[Byte]) extends OutputStream {
  val bufsize = 32768
  val blocksize = 16

  var remaining = 0
  val padding = new PKCS7Padding
  val buf = Array.ofDim[Byte](bufsize)
  val lastblock = Array.ofDim[Byte](blocksize)
  val keyschedule = Array.ofDim[Int](60)
  val currentIV = iv.clone()
  padding.init(null)

  NdkAESEngine.scheduleKey(key, keyschedule)

  override def write(i: Int) = ???
  override def write(buffer: Array[Byte]) = write(buffer, 0, buffer.length)
  override def flush() = out.flush()

  override def write(buffer: Array[Byte], offset: Int, count: Int) = {
    if (count + remaining < blocksize) {
      // schedule to be handled later if too small to process
      System.arraycopy(buffer, offset, lastblock, remaining, count)
      remaining = count + remaining
    } else {
      val maxToCopy = math.min(count, bufsize - remaining)
      // after copying these bytes, this + remaining should be aligned
      val nFromBuffer = (maxToCopy + remaining) - ((maxToCopy + remaining) % blocksize) - remaining
      val buflen = remaining + nFromBuffer // should be aligned to blocksize
      if (buflen % 16 != 0) {
        throw new IllegalStateException("Bad math, should be 16: " + buflen)
      }

      // copy left over from previous write
      System.arraycopy(lastblock, 0, buf, 0, remaining)
      // copy the rest in, blocksize aligned
      System.arraycopy(buffer, offset, buf, remaining, nFromBuffer)

      if (!NdkAESEngine.encrypt_cbc(buf, 0, buf, 0, buflen, keyschedule, key.length * 8, currentIV)) {
        throw new IllegalArgumentException("Bad input buffer size: " + buflen)
      }
      out.write(buf, 0, buflen)
      // last block is next process cycle's IV
      System.arraycopy(buf, buflen - blocksize, currentIV, 0, blocksize)
      if (count + remaining > bufsize) {
        // write request is bigger than our buffer size, process the rest of it
        remaining = 0
        write(buffer, offset + nFromBuffer, count - nFromBuffer)
      } else {
        // do we have leftovers to process later?
        if (nFromBuffer != count) {
          remaining = count - nFromBuffer
          System.arraycopy(buffer, offset + nFromBuffer, lastblock, 0, remaining)
        } else remaining = 0
      }
    }
  }

  override def close() = {
    padding.addPadding(lastblock, remaining)
    out.write(lastblock, 0, blocksize)
    out.close()
  }
}


