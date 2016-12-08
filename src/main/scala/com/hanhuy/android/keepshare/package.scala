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

  implicit class RightBiasEither[R,L](val either: Either[L,R]) extends AnyVal {
    def foreach[U](f: R => U): Unit = either.right.foreach(f)
    def flatMap[A,B >: L](f: R => Either[B,A]): Either[B,A] = either.right.flatMap(f)
    def map[A](f: R => A): Either[L,A] = either.right.map(f)
    def toOption: Option[R] = either.right.toOption
    def flip = LeftBiasEither(either)
  }
  case class LeftBiasEither[L,R](either: Either[L,R]) extends AnyVal {
    def foreach[U](f: L => U): Unit = either.left.foreach(f)
    def flatMap[A >: R, B](f: L => Either[B,A]): Either[B,A] = either.left.flatMap(f)
    def map[A](f: L => A): Either[A,R] = either.left.map(f)
    def toOption: Option[L] = either.left.toOption
    def flip = new RightBiasEither(either)
  }
}
