package com.hanhuy.android.keepshare.tests;

import android.content.ComponentName;
import android.content.Intent;
import android.test.ActivityUnitTestCase;
import com.hanhuy.android.keepshare.KeyManager;
import com.hanhuy.android.keepshare.SetupActivity;
import com.hanhuy.android.keepshare.Settings;

import javax.crypto.spec.SecretKeySpec;

/** in order for any of these tests to succeed, the app must have been run
  *  at least once with an account selected/granted access to Google Drive
  */
public class KeyManagerTest extends ActivityUnitTestCase {
    final static Intent ACTIVITY = Intent.makeMainActivity(
            new ComponentName("com.hanhuy.android.keepshare", ".MainActivity"));

    static String localKey = null;
    public KeyManagerTest() {
        super(SetupActivity.class);
    }

    public void testLoadKey() {
        startActivity(ACTIVITY, null, null);
        assertNotNull(getActivity());
        Settings settings = new Settings(getActivity());
        KeyManager km = new KeyManager(getActivity(), settings);
        km.accountName_$eq("pfnguyen@gmail.com");
        byte[] key = km.loadKey();
        assertNotNull("Key cannot be null", key);
        assertEquals("improper key length", 32, key.length);
    }

    public void testLocalKey() {
        startActivity(ACTIVITY, null, null);
        assertNotNull(getActivity());
        Settings settings = new Settings(getActivity());
        KeyManager km = new KeyManager(getActivity(), settings);
        km.accountName_$eq("pfnguyen@gmail.com");
        km.loadKey();
        SecretKeySpec k = km.localKey();
        assertNotNull("key cannot be null", k);
        String hexed = KeyManager.hex(k.getEncoded());
        assertNotNull("cannot be null", hexed);
        // repeated test will verify localKey != null clause
        if (localKey != null)
            assertEquals("Keys must match", localKey, hexed);
        else
            localKey = hexed;
    }

    public void testLocalKey2() { // second time must be same
        testLocalKey();
    }
}
