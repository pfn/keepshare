package com.hanhuy.android.keepshare

/**
 * @author pfnguyen
 */
object Rx {
  implicit class JavaObservableExtension[T](val obs: rx.Observable[_ <: T]) extends AnyVal {
    def asScala = new rx.lang.scala.Observable[T] {
      val asJavaObservable = obs
    }
  }

  implicit class ScalaObservableExtension[T](val obs: rx.lang.scala.Observable[_ <: T]) extends AnyVal {
    import rx.lang.scala.JavaConversions
    def observeOn(s: rx.Scheduler): rx.lang.scala.Observable[T] =
      obs.observeOn(JavaConversions.javaSchedulerToScalaScheduler(s))
    def subscribeOn(s: rx.Scheduler): rx.lang.scala.Observable[T] =
      obs.subscribeOn(JavaConversions.javaSchedulerToScalaScheduler(s))
  }
}
