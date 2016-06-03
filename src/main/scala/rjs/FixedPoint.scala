package rjs.fixedPoint


// Quick Segway to http://debasishg.blogspot.com/2011/07/datatype-generic-programming-in-scala.html

case class Mu[F[_]](value: F[Mu[F]])

sealed trait WildIntList
case object WildNil extends WildIntList
case class WildCons(head: Int, tail: WildIntList)


sealed trait WildNum
case object WildZero extends WildNum
case class WildSucccessor(n: WildNum) extends WildNum


sealed trait TameIntList[+T]
object TameIntList {
  type FIX = Mu[TameIntList]

  implicit object functor extends Functor[TameIntList] {
    def map[A, B](f: (A) => B): (TameIntList[A]) => TameIntList[B] = (ta: TameIntList[A]) => ta match {
      case TameNil => TameNil
      case TameCons(h, t) => TameCons(h, f(t))
    }
  }

}
case object TameNil extends TameIntList[Nothing]
case class TameCons[+T](head: Int, tail: T) extends TameIntList[T]

sealed trait TameNum[+T]
object TameNum {
  type FIX = Mu[TameNum]
  def zero: FIX = Mu[TameNum](TameZero)
  def succcessor: FIX => FIX = n => Mu(TameSuccessor(n))

  def fromInt(i: Int): FIX = if (i == 0) zero else succcessor(fromInt(i - 1))

  implicit object functor extends Functor[TameNum] {
    def map[A, B](f: (A) => B): (TameNum[A]) => TameNum[B] = (ta: TameNum[A]) => ta match {
      case TameZero => TameZero
      case TameSuccessor(n) => TameSuccessor(f(n))
    }
  }
}
case object TameZero extends TameNum[Nothing]
case class TameSuccessor[+T](n: T) extends TameNum[T]

trait Functor[F[_]] { def map[A, B](f: A => B): F[A] => F[B] }

object Recursion {
  def cata[A, F[_]](f: F[A] => A)(t: Mu[F])(implicit fc: Functor[F]): A = f(fc.map(cata[A, F](f))(t.value))

  def fib: TameNum.FIX => Int = cata[Int, TameNum] {i =>
    println(i)
    i match {
    case TameZero => 1
    case TameSuccessor(0) => 1
    case TameSuccessor(nMinus1) => (nMinus1 + 1) * nMinus1
  }  }

  def fibN(i: Int) = fib(TameNum.fromInt(i))
}


