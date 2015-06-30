package com.hanhuy.android.keepshare.tests

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.security.MessageDigest

import android.support.test.InstrumentationRegistry
import android.support.test.runner.AndroidJUnit4
import com.hanhuy.android.keepshare.{KeyManager, NdkShaDigest}
import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith

@RunWith(classOf[AndroidJUnit4])
class NdkShaTest {

  @Test
  def simpleFileDigest(): Unit = {
    val ctx = InstrumentationRegistry.getContext
    val ciphertext = ctx.getAssets.open("sbt-0.13.6.tgz.aes")
    val bout = new ByteArrayOutputStream
    val buf = Array.ofDim[Byte](32768)
    Stream.continually(ciphertext.read(buf, 0, 32768)) takeWhile (_ != -1) foreach (bout.write(buf, 0, _))
    ciphertext.close()
    val data = bout.toByteArray
    val md = MessageDigest.getInstance("SHA-256")
    val bin = new ByteArrayInputStream(data)
    Stream.continually(bin.read(buf, 0, 32768)) takeWhile (_ != -1) foreach (md.update(buf, 0, _))
    val digest1 = md.digest
    val bin2 = new ByteArrayInputStream(data)
    val shactx = NdkShaDigest.sha256_new
    Stream.continually(bin2.read(buf, 0, 32768)) takeWhile (_ != -1) foreach (NdkShaDigest.sha256_update(shactx, buf, 0, _))
    val digest2 = Array.ofDim[Byte](32)
    NdkShaDigest.sha256_final(shactx, digest2)

    assertArrayEquals(KeyManager.hex(digest1) + " == " + KeyManager.hex(digest2), digest1, digest2)
  }

  @Test
  def cannedTests(): Unit = {
    val hash1 = Array(0xba,0x78,0x16,0xbf,0x8f,0x01,0xcf,0xea,0x41,0x41,0x40,0xde,0x5d,0xae,0x22,0x23,0xb0,0x03,0x61,0xa3,0x96,0x17,0x7a,0x9c,0xb4,0x10,0xff,0x61,0xf2,0x00,0x15,0xad).map(_.toByte)
    val hash2 = Array(0x24,0x8d,0x6a,0x61,0xd2,0x06,0x38,0xb8,0xe5,0xc0,0x26,0x93,0x0c,0x3e,0x60,0x39,0xa3,0x3c,0xe4,0x59,0x64,0xff,0x21,0x67,0xf6,0xec,0xed,0xd4,0x19,0xdb,0x06,0xc1).map(_.toByte)
    val hash3 = Array(0xcd,0xc7,0x6e,0x5c,0x99,0x14,0xfb,0x92,0x81,0xa1,0xc7,0xe2,0x84,0xd7,0x3e,0x67,0xf1,0x80,0x9a,0x48,0xa4,0x97,0x20,0x0e,0x04,0x6d,0x39,0xcc,0xc7,0x11,0x2c,0xd0).map(_.toByte)
    val text1 = "abc"
    val text2 = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
    val text3 = "aaaaaaaaaa"

    val out = Array.ofDim[Byte](32)
    val ctx = NdkShaDigest.sha256_new
    NdkShaDigest.sha256_update(ctx, text1.getBytes, 0, text1.length)
    NdkShaDigest.sha256_final(ctx, out)
    assertArrayEquals(hash1, out)
  }
}
