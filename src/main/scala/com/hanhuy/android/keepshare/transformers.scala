package com.hanhuy.android.keepshare

/**
  * @author pfnguyen
  */
import language.higherKinds
import scala.annotation.implicitNotFound
@implicitNotFound("Missing instance for Monad[${F}]")
trait Monad[F[_]] extends Functor[F] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  def point[A](a: A): F[A]
}
@implicitNotFound("Missing instance for Functor[${F}]")
trait Functor[F[_]] {
  def map[A,T](a: F[T])(f: T => A): F[A]
}
case class EitherT[F[_], L, R](run: F[Either[L,R]]) extends AnyVal {
  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isLeft)
  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isRight)
  def map[C](f: R => C)(implicit F: Functor[F]): EitherT[F, L, C] =
    EitherT(F.map(run)(_.right.map(f)))
  def flatMap[C](f: R => EitherT[F, L, C])(implicit F: Monad[F]): EitherT[F, L, C] =
    EitherT(F.bind(run)(_.fold(a => F.point(Left(a)), b => f(b).run)))
  def swap(implicit F: Functor[F]): EitherT[F, R, L] =
    EitherT(F.map(run)(_.swap))
  def foreach[C](f: R => C)(implicit F: Functor[F]): Unit =
    F.map(run)(_.right.foreach(f))
}

