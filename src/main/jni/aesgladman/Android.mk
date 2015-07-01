LOCAL_PATH	:= $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE		:= aesgladman
LOCAL_SRC_FILES		:= aescrypt.c aestab.c aeskey.c

include $(BUILD_STATIC_LIBRARY)
