package com.hanhuy.android.keepshare

import java.security.MessageDigest

/**
 * @author pfnguyen
 */
class NdkSha256 extends MessageDigest("SHA-256") {
  private var shactx = NdkShaDigest.sha256_new
  override def engineDigest() = {
    val hash = Array.ofDim[Byte](32)
    NdkShaDigest.sha256_final(shactx, hash)
    hash
  }

  override def engineUpdate(b: Byte) = {
    val buf = Array.ofDim[Byte](1)
    engineUpdate(buf, 0, 1)
  }

  override def engineUpdate(bytes: Array[Byte], i: Int, i1: Int) =
    NdkShaDigest.sha256_update(shactx, bytes, i, i1)

  override def engineReset() = shactx = NdkShaDigest.sha256_new
}
