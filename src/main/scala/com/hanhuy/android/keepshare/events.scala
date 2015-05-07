package com.hanhuy.android.keepshare

import com.hanhuy.android.common.BusEvent

/**
 * @author pfnguyen
 */
case object DatabaseClosed extends BusEvent
case object PINServiceExit extends BusEvent
case object PINServiceStart extends BusEvent
case object ServiceExit extends BusEvent
case object KeyboardExit extends BusEvent
case object IMESearchOk extends BusEvent
case object IMESearchCancel extends BusEvent
case object ShareActivityCancel extends BusEvent
case class AccessibilityFillEvent(pkg: String, windowId: Int, uri: String,
  username: String, password: String) extends BusEvent
