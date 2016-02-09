package com.hanhuy.android

import android.text.TextWatcher
import android.widget.EditText

/**
  * @author pfnguyen
  */
package object keepshare {

  implicit class EditTextWatcher(val e: EditText) extends AnyVal {
    def onTextChanged[A](fn: CharSequence => A) = e.addTextChangedListener(
      iota.single[TextWatcher].onTextChanged(
        (s: CharSequence, _: Int, _: Int, _: Int) => fn(s)))
  }
}
