package com.hanhuy.android.keepshare

/**
  * replace rx, only want observer pattern
  * @author pfnguyen
  */
object Var {
  def apply[T](value: T): Var[T] = new Var(value)
}

final class Var[T] private(initial: T) extends Obs[T] {
  private[this] var _value = initial
  private[this] var subscribers: List[(Sub,(T,Sub) => _)] = Nil
  def value = _value
  def update(value: T): Unit = {
    _value = value
    subscribers.foreach { case (sub,f) => f.apply(value,sub) }
  }
  def subscribe2[A](f: (T, Sub) => A, skipInitial: Boolean = true): Sub = this.synchronized {
    lazy val sub: Sub = Sub(() => this.synchronized { subscribers = subscribers.filterNot(_ == (sub,f)) })
    subscribers = (sub,f) :: subscribers
    if (!skipInitial) f(value, sub)
    sub
  }
  def subscribe[A](f: T => A, skipInitial: Boolean = true): Sub = subscribe2((t,_) => f(t), skipInitial)
}

final case class Sig[T](f: T => Any) {
  @inline def apply(value: T) = f(value)
}

case class Sub(unsubscribe: () => _)
object Obs {
  def create[T](createObs: Sig[T] => Sub): Obs[T] = new Obs[T] {
    def subscribe2[A](f: (T, Sub) => A, skipInitial: Boolean = true): Sub = {
      def sub2(sig: Sig[T]) = createObs(sig)
      def sub(sub2: Sub): Sub = Sub(() => sub2.unsubscribe())
      lazy val v: Sig[T] = Sig[T](x => f(x, s))
      lazy val s = sub(sub2(v))
      s
    }
    def subscribe[A](f: T => A, skipInitial: Boolean = true): Sub = subscribe2((t,_) => f(t), skipInitial)
  }
}

sealed trait Obs[T] {
  def subscribe2[A](f: (T, Sub) => A, skipInitial: Boolean = true): Sub
  def subscribe[A](f: T => A, skipInitial: Boolean = true): Sub
}
