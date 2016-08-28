package rjs.recursion
package data

import cats.{Monoid, Functor, Applicative}

trait RecTree[+A] extends Product with Serializable
case object Empty extends RecTree[Nothing]
case class Leaf[A](a: A) extends RecTree[A]
case class Node[A](left: RecTree[A], right: RecTree[A]) extends RecTree[A]

object RecTree {
  implicit object foldable extends Foldable[RecTree] {
    def foldMap[M: Monoid, A](f: (A) => M)(ta: RecTree[A]): M = ta match {
      case Empty => Monoid[M].empty
      case Leaf(a) => f(a)
      case Node(l, r) => Monoid[M].combine(foldMap(f)(l), foldMap(f)(r))
    }
  }

  implicit object functor extends Functor[RecTree] {
    def map[A, B](fa: RecTree[A])(f: (A) => B): RecTree[B] = fa match {
      case Empty => Empty
      case Leaf(a) => Leaf(f(a))
      case Node(l, r) => Node(map(l)(f), map(r)(f))
    }
  }

  implicit object traversable extends Traversable[RecTree] {
    implicit def functor: Functor[RecTree] = Functor[RecTree]

    implicit def foldable: Foldable[RecTree] = Foldable[RecTree]

    def traverse[F[_] : Applicative, A, B](f: (A) => F[B])(ta: RecTree[A]): F[RecTree[B]] = ta match {
      case Empty => Applicative[F].pure(Empty)
      case Leaf(a) => Applicative[F].map(f(a))(Leaf(_))
      case Node(l, r) =>
        Applicative[F].ap2[RecTree[B], RecTree[B], RecTree[B]](Applicative[F].pure(Node[B]))(traverse(f)(l), traverse(f)(r))
    }
  }
}
