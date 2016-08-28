package rjs.recursion.data

import cats.Functor

sealed trait TreeF[A, R]
trait TreeFA[A] {
  type l[r] = TreeF[A, r]
}
case class LeafF[A, R](a: A) extends TreeF[A, R]
case class NodeF[A, R](r: R, l: R) extends TreeF[A, R]

object TreeF {
  implicit def functor[A]: Functor[TreeFA[A]#l] = new Functor[TreeFA[A]#l] {
    def map[R, B](fa: TreeF[A, R])(f: (R) => B): TreeF[A, B] = fa match {
      case LeafF(a) => LeafF(a)
      case NodeF(l, r) => NodeF(f(l), f(r))
    }
  }
}
