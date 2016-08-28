package rjs.recursion.data

import cats.{Functor, Monoid}

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

  def apply[A](as: A*): MyList.T[A] = as.foldLeft(
    Fix[ListFA[A]#l](NilF): MyList.T[A]
  )(
    (l: MyList.T[A], i: A) => Fix[ListFA[A]#l](ListF.consF[A, MyList.T[A]](i, l))
  )

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
