package rjs.recursion

import cats.Functor

object utils {
  def &&&[B, C1, C2](f: B => C1, g: B => C2): B => (C1, C2) = {
    (b: B) => (f(b), g(b))
  }

  def |||[B1, B2, C](f: B1 => C, g: B2 => C): Either[B1, B2] => C = {
    _.fold(f, g)
  }

  def ***[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = Function.tupled {
    (a: A, b: C) => (f(a), g(b))
  }

  def funzip[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = (
    Functor[F].map(fab)(_._1),
    Functor[F].map(fab)(_._2)
  )

  final def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  def fmap[F[_]: Functor, A, B](f: => A => B): F[A] => F[B] = {
    Functor[F].map(_)(f)
  }

}
