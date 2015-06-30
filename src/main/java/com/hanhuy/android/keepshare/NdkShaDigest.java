package com.hanhuy.android.keepshare;

import java.nio.ByteBuffer;

/**
 * @author pfnguyen
 */
public class NdkShaDigest {
    static {
        System.loadLibrary("aes");
    }

    public static byte[] allocateStateBuffer() {
        return new byte[sha256_size()];
    }

    public static byte[] sha256_new() {
        byte[] state = allocateStateBuffer();
        sha256_init(state);
        return state;
    }
    private static native void sha256_init(byte[] state);
    public static native void sha256_update(byte[] state, byte[] data, int off, int len);
    public static native void sha256_final(byte[] state, byte[] digest);
    public static native int sha256_size();
}
