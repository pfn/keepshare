#include <com_hanhuy_android_keepshare_NativeKeyTransformer__.h>
#include "../aesgladman/aes.h"
JNIEXPORT jboolean JNICALL Java_com_hanhuy_android_keepshare_NativeKeyTransformer_00024_transformKey
  (JNIEnv *env, jobject self, jbyteArray key, jbyteArray seed, jlong rounds)
{
  int i;
  int r = EXIT_SUCCESS;
  jbyte *seeddata = (*env)->GetByteArrayElements(env, seed, 0);
  jbyte *keydata  = (*env)->GetByteArrayElements(env, key, 0);

  aes_encrypt_ctx ctx[1];
  // shouldn't necessarily assume 256bit keys...
  r = aes_encrypt_key(seeddata, 32, ctx);
  for (i = 0; i < rounds && r == EXIT_SUCCESS; i++) {
    aes_encrypt(keydata, keydata, ctx);
    r = aes_encrypt(keydata + 16, keydata + 16, ctx);
  }

  (*env)->ReleaseByteArrayElements(env, seed, seeddata, 0);
  (*env)->ReleaseByteArrayElements(env, key, keydata, 0);
  return EXIT_SUCCESS == r ? JNI_TRUE : JNI_FALSE;
}
