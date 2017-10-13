package rjs.recursion.data

import cats.Functor

import rjs.recursion.schemes.cata

case class AnnF[F[_], A, R](fr: F[R], a: A) // The Cofree pattern functor
trait AnnFA[F[_], A] {
  type l[r] = AnnF[F, A, r]
}
object AnnF {
  implicit def functor[F[_]: Functor, A]: Functor[AnnFA[F, A]#l] = new Functor[AnnFA[F, A]#l] {
    def map[R, B](fa: AnnF[F, A, R])(f: (R) => B): AnnF[F, A, B] = AnnF(implicitly[Functor[F]].map(fa.fr)(f), fa.a)
  }
}

object Ann{
  type T[F[_], A] = Fix[AnnFA[F, A]#l]

  def attr[F[_], A]: Ann.T[F, A] => A = {
    unFix[AnnFA[F, A]#l] andThen(_.a)
  }

  def strip[F[_], A]: Ann.T[F, A] => F[Ann.T[F, A]] = {
    unFix[AnnFA[F, A]#l] andThen(_.fr)
  }

  def stripAll[F[_]: Functor, A]: Ann.T[F, A] => Fix[F] = {
    def alg(ann: AnnF[F, A, Fix[F]]) = Fix[F](ann.fr)
    cata[AnnFA[F, A]#l, Fix[F]](alg)
  }

  def ann[F[_]: Functor, A]: ((F[Ann.T[F, A]], A)) => Ann.T[F, A] = {
    Function.tupled(AnnF[F, A, Ann.T[F, A]] _) andThen Fix[AnnFA[F, A]#l]
  }

  def unAnn[F[_]: Functor, A]: Ann.T[F, A] => (F[Ann.T[F, A]], A) = { (annFA: Ann.T[F, A]) =>
    val AnnF(fr, a) = annFA.unFix
    (fr, a)
  }
}