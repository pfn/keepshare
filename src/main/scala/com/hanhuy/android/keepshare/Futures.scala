package com.hanhuy.android.keepshare

import android.util.Log
import com.hanhuy.android.common.{AndroidConversions, UiBus}

import scala.concurrent.{Future, ExecutionContext}
import scala.util.Try

/**
 * @author pfnguyen
 */
object Futures {
  object CurrentThread extends ExecutionContext {
    override def execute(runnable: Runnable) = runnable.run()
    override def reportFailure(cause: Throwable) = Log.w(
      "Futures", cause.getMessage, cause)
  }
  object MainThread extends ExecutionContext {
    override def execute(runnable: Runnable) = UiBus.run(runnable.run())
    override def reportFailure(cause: Throwable) = throw cause
  }

  implicit object AsyncThread extends ExecutionContext {
    override def execute(runnable: Runnable) =
      AndroidConversions._threadpool.execute(runnable)
    override def reportFailure(cause: Throwable) = Log.w(
      "Futures", cause.getMessage, cause)
  }

  implicit class RichFuturesType(val f: Future.type) extends AnyVal {
    def main[A](b: => A) = f.apply(b)(MainThread)
  }

  implicit class RichFutures[T](val f: Future[T]) extends AnyVal {
    type S[U] = PartialFunction[T,U]
    type F[U] = PartialFunction[Throwable,U]
    type C[U] = Try[T] => U
    def onSuccessHere[U]  = f.onSuccess( _: S[U])(CurrentThread)
    def onFailureHere[U]  = f.onFailure( _: F[U])(CurrentThread)
    def onCompleteHere[U] = f.onComplete(_: C[U])(CurrentThread)
    def onSuccessMain[U]  = f.onSuccess( _: S[U])(MainThread)
    def onFailureMain[U]  = f.onFailure( _: F[U])(MainThread)
    def onCompleteMain[U] = f.onComplete(_: C[U])(MainThread)

    def ~[A >: T](f: Future[A]): Future[A] = f.flatMap(_ => f)
  }
  def traverseO[A, B](o: Option[A])(f: A => Future[B])(implicit ev: ExecutionContext): Future[Option[B]] =
    (o map f).fold(Future.successful(Option.empty[B]))(_.flatMap(x => Future.successful(Some(x)))(ev))
  def sequenceO[A](o: Option[Future[A]])(implicit ev: ExecutionContext): Future[Option[A]] = traverseO(o)(identity)(ev)
}
