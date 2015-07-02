package com.hanhuy.android.keepshare

import android.app.Dialog

/**
 * @author pfnguyen
 */
trait DialogManager {
  private[this] var dialogSet = Set.empty[Dialog]

  def dismissAllDialogs() = {
    dialogSet foreach (_.dismiss())
    dialogSet = Set.empty
  }

  def showingDialog(d: Dialog) = {
    dialogSet += d
    d
  }

  def dismissDialog(d: Dialog): Unit = {
    if (dialogSet(d)) {
      dialogSet -= d
      d.dismiss()
    }
  }
}
