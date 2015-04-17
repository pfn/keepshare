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
  }
}
