package rjs.recursion.data

import cats.Functor

sealed trait CtxF[F[_], A, R] // The free monad pattern functor
case class Term[F[_], A, R](fr: F[R]) extends CtxF[F, A, R]
case class Hole[F[_], A, R](a: A) extends CtxF[F, A, R]
trait CtxFA[F[_], A] {
  type l[r] = CtxF[F, A, r]
}

object CtxF {
  implicit def functor[F[_]: Functor, A]: Functor[CtxFA[F, A]#l] = new Functor[CtxFA[F, A]#l] {
    def map[R, B](fa: CtxF[F, A, R])(f: (R) => B): CtxF[F, A, B] = fa match {
      case Term(fr) => Term[F, A, B](Functor[F].map(fr)(f))
      case Hole(a) => Hole[F, A, B](a)
    }
  }
}

object Ctx {
  type T[F[_], A] = Fix[CtxFA[F, A]#l]

  def unCtx[F[_], A](c: Ctx.T[F, A]): Either[A, F[Ctx.T[F, A]]] = {
    unFix[CtxFA[F, A]#l](c) match {
      case Hole(x) => Left(x)
      case Term(t) => Right(t)
    }
  }

  def term[F[_]: Functor, A](x: F[Fix[CtxFA[F, A]#l]]): Ctx.T[F, A] =
    Fix[CtxFA[F, A]#l](Term[F, A, Fix[CtxFA[F, A]#l]](x))

  def hole[F[_]: Functor, A](a: A): Ctx.T[F, A] =
    Fix[CtxFA[F, A]#l](Hole[F, A, Fix[CtxFA[F, A]#l]](a))

}