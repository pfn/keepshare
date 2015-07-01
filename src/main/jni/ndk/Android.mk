LOCAL_PATH		:= $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE		:= aeskeytrans
LOCAL_SRC_FILES		:= jni.c
LOCAL_C_INCLUDES	:= $(SBT_SOURCE_MANAGED)
LOCAL_LDLIBS		:= -llog
LOCAL_STATIC_LIBRARIES	:= aesgladman

include $(BUILD_SHARED_LIBRARY)
