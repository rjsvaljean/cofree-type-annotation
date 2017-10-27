package rjs.recursion.data

import cats.Functor

sealed trait NatF[+R] extends Product with Serializable {
  def fold[X](ifZero: => X)(ifNonZero: R => X): X
}

case object Zero extends NatF[Nothing] {
  def fold[X](ifZero: => X)(ifNonZero: (Nothing) => X): X = ifZero
}

case class Succ[R](r: R) extends NatF[R] {
  def fold[X](ifZero: => X)(ifNonZero: (R) => X): X = ifNonZero(r)
}

object NatF {
  type T = Fix[NatF]
  def zero: NatF[Nothing] = Zero
  def succ[R](r: R): NatF[R] = Succ(r)

  implicit object functor extends Functor[NatF] {
    def map[A, B](fa: NatF[A])(f: (A) => B): NatF[B] = fa match {
      case Zero => zero
      case Succ(r) => succ(f(r))
    }
  }

  def expand(i: Int): Fix[NatF] = i match {
    case 0 ⇒ Fix[NatF](zero)
    case n ⇒ Fix[NatF](succ(expand(n - 1)))
  }

  def toInt(n: Fix[NatF]): Int = n match {
    case Fix(Zero) ⇒ 0
    case Fix(Succ(_n)) ⇒ 1 + toInt(_n)
  }

  def compress[A](n: NatF[Ann.T[NatF, A]]): Int = n match {
    case Zero ⇒ 0
    case Succ(f) ⇒ 1 + compress(f.unFix.fr)
  }
}
