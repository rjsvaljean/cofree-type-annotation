package rjs.recursion.data

import cats.{Applicative, Monad}


/**
  * Traversable
  *===========
  **
 Traversable gives you the ability to traverse a structure from left-to-right,
  *performing an effectful action on each element and preserving the shape.
  **
 Intuitively: fmap with effects
  */
trait Traversable[T[_]] {
  def traverse[F[_]: Applicative, A, B](f: => (A => F[B]))(ta: T[A]): F[T[B]]

  def mapM[M[_]: Monad, A, B](f: => (A => M[B]))(ta: T[A]): M[T[B]] =
    traverse[M, A, B](f)(ta)
}

object Traversable {
  def apply[T[_]](implicit traversable: Traversable[T]) = traversable
}