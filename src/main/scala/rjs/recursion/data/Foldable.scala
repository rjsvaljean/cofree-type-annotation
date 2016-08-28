package rjs.recursion.data

import cats.Monoid

/**
Foldable
========

The Foldable class gives you the ability to process the elements of a structure
one-at-a-time, discarding the shape.

Intuitively: list-like fold methods
  */
trait Foldable[T[_]] {
  def foldMap[M : Monoid, A](f: A => M)(ta: T[A]): M
  def fold[M : Monoid](tm: T[M]): M = foldMap[M, M](identity)(tm)
}

object Foldable {
  def apply[T[_]: Foldable] = implicitly[Foldable[T]]
  import cats.std.int.intGroup
  def count[T[_] : Foldable, A] = Foldable[T].foldMap((_: A) => 1) _
}
