package rjs.recursion.data

import cats.Functor
import rjs.recursion.utils.{fmap, &&&, |||}

abstract class Fixpoint[F[_]: Functor, T] {
  def inF: F[T] => T
  def outF: T => F[T]
  def functor: Functor[F] = Functor[F]
}

object Fixpoint {
  implicit def fixFixPoint[F[_]: Functor]: Fixpoint[F, Fix[F]] = new Fixpoint[F, Fix[F]] {
    val inF: (F[Fix[F]]) => Fix[F] = Fix[F]
    val outF: (Fix[F]) => F[Fix[F]] = (_: Fix[F]).unFix
  }

  implicit def listFixPoint[A]: Fixpoint[ListFA[A]#l, List[A]] =
    new Fixpoint[ListFA[A]#l, List[A]] {
      def inF: (ListF[A, List[A]]) => List[A] = _.fold(List[A]())(_ :: _)
      def outF: (List[A]) => ListF[A, List[A]] = {
        case Nil => ListF.nilF
        case x :: xs => ListF.consF(x, xs)
      }
    }


  implicit object natFixPoint extends Fixpoint[NatF, Int] {
    def inF: (NatF[Int]) => Int = _.fold(0)(_ + 1)

    def outF: (Int) => NatF[Int] = {
      case 0 => NatF.zero
      case i => NatF.succ(i - 1)
    }
  }

  def cata[F[_], T, A](
    alg: F[A] => A
  )(
    implicit
    fixpoint: Fixpoint[F, T]
  ): T => A =
    fixpoint.outF andThen
      fmap[F, T, A](cata(alg))(fixpoint.functor) andThen
      alg

  def ana[F[_], T, A](
    coalg: A => F[A]
  )(
    implicit
    fixpoint: Fixpoint[F, T]
  ): A => T =
    coalg andThen
      fmap[F, A, T](ana(coalg))(fixpoint.functor) andThen
      fixpoint.inF

  def para[F[_], A, T](
    alg: F[(A, T)] => A
  )(
    implicit
    fixpoint: Fixpoint[F, T]
  ): T => A = {
    implicit val functorF: Functor[F] = fixpoint.functor
    alg compose fmap[F, T, (A, T)](&&&(para[F, A, T](alg), identity)) compose fixpoint.outF
  }

  def apo[F[_], A, T](
    coalg: A => F[Either[A, T]]
  )(
    implicit fixpoint: Fixpoint[F, T]
  ): A => T =
    inF[F, T] compose fmap(|||(apo(coalg), identity[T]))(fixpoint.functor) compose coalg

  def histo[F[_], A, T](
    alg: F[Ann.T[F, A]] => A
  )(
    implicit fixPoint: Fixpoint[F, T]
  ): T => A = {
    implicit val functor: Functor[F] = fixPoint.functor
    Ann.attr[F, A] compose Fixpoint.cata(Ann.ann[F, A] compose &&&(identity[F[Ann.T[F, A]]], alg))
  }



  def inF[F[_], T](implicit fixPoint: Fixpoint[F, T]): (F[T]) => T = fixPoint.inF
  def outF[F[_], T](implicit fixPoint: Fixpoint[F, T]): (T) => F[T] = fixPoint.outF
}

