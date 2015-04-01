package com.hanhuy.android.keepshare.tests;

import android.test.AndroidTestCase;
import android.util.Log;

import com.hanhuy.android.keepshare.ShareActivity;

public class ShareActivityTest extends AndroidTestCase {
    public void testSubHosts() {
        scala.collection.Seq<String> items =
                ShareActivity.subhosts("foo.bar.baz.mainhost.maindomain.com");
        assertEquals("must be 5 items", 5, items.size());
        Log.v("ShareActivityTest", "items: " + items);
    }
}
