package rjs.recursion.data

import cats.{Functor, Monoid}
import rjs.recursion.schemes.cata

sealed trait ListF[+A, +R] extends Product with Serializable {
  def fold[X](ifNil: => X)(ifCons: (A, R) => X): X
}

trait ListFA[A] {
  type l[r] = ListF[A, r]
}

case object NilF extends ListF[Nothing, Nothing] {
  def fold[X](ifNil: => X)(ifCons: (Nothing, Nothing) => X): X = ifNil
}

case class ConsF[A, R](a: A, r: R) extends ListF[A, R] {
  def fold[X](ifNil: => X)(ifCons: (A, R) => X) = ifCons(a, r)
}

object MyList {

  type T[A] = Fix[({type l[r] = ListF[A, r]})#l]

  def nil[A] = Fix[ListFA[A]#l](ListF.nilF)
  def cons[A](h: A, t: T[A]) = Fix[ListFA[A]#l](ListF.consF(h, t))

  def apply[A](as: A*): MyList.T[A] = as.foldRight(
    Fix[ListFA[A]#l](NilF): MyList.T[A]
  )(
    (i: A, l: MyList.T[A]) => Fix[ListFA[A]#l](ListF.consF[A, MyList.T[A]](i, l))
  )

  def toList[A]: T[A] => List[A] = cata[ListFA[A]#l, List[A]] {
    case NilF => Nil
    case ConsF(h, t) => h :: t
  }

}

object ListF {
  def consF[A, R](a: A, r: R): ListF[A, R] = ConsF(a, r)
  def nilF: ListF[Nothing, Nothing] = NilF

  implicit def functor[AA]: Functor[ListFA[AA]#l] = new Functor[ListFA[AA]#l] {
    def map[A, B](fa: ListF[AA, A])(f: (A) => B): ListF[AA, B] = fa match {
      case NilF => nilF
      case ConsF(a, r) => consF(a, f(r))
    }
  }

  implicit def foldable[AA]: Foldable[ListFA[AA]#l] = new Foldable[ListFA[AA]#l] {
    def foldMap[M: Monoid, R](f: (R) => M)(ta: ListF[AA, R]): M = ta match {
      case ConsF(_, r) => f(r)
      case NilF => Monoid[M].empty
    }
  }
}
