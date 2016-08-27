package rjs

// Attributed to https://github.com/willtim/recursion-schemes/blob/master/slides.lhs

import cats.Functor
import FixPart.{Fix, unFix, cata}
import cats._
import cats.syntax._

object Annotating {

  // - usefult for storing intermediate values
  // - inspired by ideas from *attribute grammars*

  case class AnnF[F[_], A, R](fr: F[R], a: A)
  trait AnnFA[F[_], A] {
    type l[r] = AnnF[F, A, r]
  }

  implicit def functor[F[_]: Functor, A]: Functor[AnnFA[F, A]#l] = new Functor[AnnFA[F, A]#l] {
    def map[R, B](fa: AnnF[F, A, R])(f: (R) => B): AnnF[F, A, B] = AnnF(implicitly[Functor[F]].map(fa.fr)(f), fa.a)
  }

  type Ann[F[_], A] = Fix[AnnFA[F, A]#l]


  def attr[F[_], A]: Ann[F, A] => A = {
    unFix[AnnFA[F, A]#l] andThen(_.a)
  }

  def strip[F[_], A]: Ann[F, A] => F[Ann[F, A]] = {
    unFix[AnnFA[F, A]#l] andThen(_.fr)
  }

  def stripAll[F[_]: Functor, A, R]: Ann[F, A] => Fix[F] = {
    def alg(ann: AnnF[F, A, Fix[F]]) = Fix[F](ann.fr)
    cata[AnnFA[F, A]#l, Fix[F]](alg)
  }

  def ann[F[_]: Functor, A, R]: ((F[Ann[F, A]], A)) => Ann[F, A] = {
    Function.tupled(AnnF[F, A, Ann[F, A]] _) andThen Fix[AnnFA[F, A]#l]
  }

  def unAnn[F[_]: Functor, A, R]: Ann[F, A] => (F[Ann[F, A]], A) = { (annFA: Ann[F, A]) =>
    val AnnF(fr, a) = annFA.unFix
    (fr, a)
  }

  import RecursionSchemesTalk.&&&
  def synthesize[F[_]: Functor, A, R](f: F[A] => A): Fix[F] => Ann[F, A] = {
    val alg: F[Ann[F, A]] => Ann[F, A] =
      ann[F, A, R] compose &&&(identity[F[Ann[F, A]]], fmap[F, Ann[F, A], A](attr[F, A]) andThen f)
    cata(alg)
  }

  import cats.std.int.intGroup
  def sizes[F[_]: Functor : Foldable]: Fix[F] => Ann[F, Int] = synthesize(Foldable[F].foldMap(_ => 1))


}
