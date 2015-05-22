package com.hanhuy.android.keepshare;

import org.bouncycastle.crypto.BlockCipher;
import org.bouncycastle.crypto.CipherParameters;
import org.bouncycastle.crypto.DataLengthException;
import org.bouncycastle.crypto.params.KeyParameter;

/**
 * @author pfnguyen
 */
public class NdkAESEngine implements BlockCipher {
    private int[] keyschedule;
    private boolean encrypting = false;
    private int keysize;

    static {
        System.loadLibrary("aes");
    }
    @Override
    public void init(boolean forEncryption, CipherParameters params) throws IllegalArgumentException {
        if (params instanceof KeyParameter) {
            KeyParameter kp = (KeyParameter) params;
            encrypting = forEncryption;
            byte[] key = kp.getKey();
            keyschedule = new int[60];
            scheduleKey(key, keyschedule);
            keysize = key.length * 8;
        } else {
            throw new IllegalArgumentException("Wrong parameters: " + params.getClass());
        }
    }

    private static native void scheduleKey(byte[] key, int[] scheduleOut);
    private static native void encrypt(byte[] in, int inoff, byte[] out, int outoff, int[] keyschedule, int keysize);
    private static native void decrypt(byte[] in, int inoff, byte[] out, int outoff, int[] keyschedule, int keysize);

    @Override
    public String getAlgorithmName() {
        return "AES";
    }

    @Override
    public int getBlockSize() {
        return 16;
    }

    @Override
    public int processBlock(byte[] in, int inOff, byte[] out, int outOff) throws DataLengthException, IllegalStateException {
        if (encrypting) {
            encrypt(in, inOff, out, outOff, keyschedule, keysize);
        } else {
            decrypt(in, inOff, out, outOff, keyschedule, keysize);
        }
        return 16;
    }

    @Override
    public void reset() {
        keyschedule = null;
    }
}
