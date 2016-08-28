package rjs.recursion

import cats.Functor
import rjs.recursion.data._
import rjs.recursion.data.Fixpoint.{ana, cata, apo, outF}
import rjs.recursion.utils.{fmap, |||}

object Part8Apomorphism {

  // Can be expressed in terms of an anamorphism
  def _apo[F[_], A, T](coa: A => F[Either[A, T]])(implicit fixpoint: Fixpoint[F, T]): A => T = {
    implicit val func: Functor[F] = fixpoint.functor
    val left: (A) => Either[A, T] = Left(_)
    val right: (T) => Either[A, T] = Right(_)
    left andThen ana[F, T, Either[A, T]](|||(coa, outF[F, T] andThen fmap[F, T, Either[A, T]](right)))
  }

  def insertElem: ListF[Int, List[Int]] => List[Int] = {
    val c: ListF[Int, List[Int]] => ListF[Int, (Either[ListF[Int, List[Int]], List[Int]])] = {
      case NilF => NilF
      case ConsF(x, Nil) => ConsF(x, Left(NilF))
      case ConsF(x, y :: xs) if x <= y => ConsF(x, Right(y :: xs))
      case ConsF(x, y :: xs) if x > y => ConsF(y, Left(ConsF(x, xs)))
    }
    apo[ListFA[Int]#l, ListF[Int, List[Int]], List[Int]](c)
  }

  def insertionSort: List[Int] => List[Int] = cata[ListFA[Int]#l, List[Int], List[Int]](insertElem)
}
