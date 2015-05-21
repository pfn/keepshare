package com.hanhuy.android.keepshare

import java.io.{InputStream, OutputStream}
import java.nio.{ByteOrder, ByteBuffer}

import android.support.v8.renderscript.{Element, Allocation, RenderScript}
import com.hanhuy.keepassj.{StandardAesEngine, ICipherEngine}
import org.bouncycastle.crypto.io.{CipherInputStream, CipherOutputStream}
import org.bouncycastle.crypto.modes.CBCBlockCipher
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher
import org.bouncycastle.crypto.params.{ParametersWithIV, KeyParameter}
import org.bouncycastle.crypto.{BufferedBlockCipher, CipherParameters, BlockCipher}

/**
 * @author pfnguyen
 */
class RenderscriptAESEngine extends BlockCipher {

  val in = Array.ofDim[Int](4)
  var rs: RenderScript = _
  var scriptc: ScriptC_aes = _
  var ain: Allocation = _
  var aout: Allocation = _
  val outbuf = Array.ofDim[Int](4)

  override def getAlgorithmName = "AES"
  override def getBlockSize = 16
  override def reset() = {
    if (rs != null)
      rs.destroy()
    rs = null
    ain = null
    aout = null
  }

  override def init(b: Boolean, cipherParameters: CipherParameters) = {
    rs = RenderScript.create(Application.instance)
    scriptc = new ScriptC_aes(rs)
    ain = Allocation.createSized(rs, Element.U32_4(rs), 1)
    aout = Allocation.createSized(rs, Element.U32_4(rs), 1)

    cipherParameters match {
      case kp: KeyParameter =>
        val key = kp.getKey
        scriptc.set_key(key)
        scriptc.set_keysize(key.length * 8)
        scriptc.set_forEncryption(if (b) 1 else 0)
        scriptc.invoke_aes_init_key()
    }
  }

  override def processBlock(bytes: Array[Byte], i: Int, bytes1: Array[Byte], i1: Int) = {
    val buf = ByteBuffer.wrap(bytes)
    buf.position(i)
    buf.order(ByteOrder.LITTLE_ENDIAN)
    in(0) = buf.getInt()
    in(1) = buf.getInt()
    in(2) = buf.getInt()
    in(3) = buf.getInt()

    ain.copyFrom(in)
    scriptc.forEach_process_block(ain, aout)
    aout.copyTo(outbuf)

    val out = ByteBuffer.wrap(bytes1)
    out.position(i1)
    out.order(ByteOrder.LITTLE_ENDIAN)
    out.putInt(outbuf(0))
    out.putInt(outbuf(1))
    out.putInt(outbuf(2))
    out.putInt(outbuf(3))

    16
  }
}

class AesEngine extends ICipherEngine {
  override def getDisplayName = "AES"
  override def getCipherUuid = StandardAesEngine.getAesUuid

  def makeCipher(mode: Boolean, key: Array[Byte], iv: Array[Byte]): BufferedBlockCipher = {
    val aes = new RenderscriptAESEngine
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
