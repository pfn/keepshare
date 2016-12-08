package com.hanhuy.android

import android.text.TextWatcher
import android.widget.EditText

import scala.concurrent.Future

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
  }

  implicit val futureMonad = new Monad[concurrent.Future] {
    import com.hanhuy.android.common.Futures.AsyncThread
    override def bind[A, B](fa: Future[A])(f: (A) => Future[B]) = fa.flatMap(f)
    override def point[A](a: A) = Future(a)
    override def map[A, T](a: Future[T])(f: (T) => A) = a.map(f)
  }

  type FutureEitherT[L,R] = EitherT[concurrent.Future, L, R]
  type FutureEither[L,R] = Future[Either[L,R]]
  object FutureEitherT {
    def apply[L,R](etf: FutureEither[L, R]) = EitherT(etf)
  }
}
