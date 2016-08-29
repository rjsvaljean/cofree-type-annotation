package rjs.recursion

import cats.Functor
import rjs.recursion.data._
import rjs.recursion.data.Ctx.hole
import rjs.recursion.utils.{&&&, fmap, |||}

object schemes {
  def cata[F[_]: Functor, A](alg: F[A] => A): Fix[F] => A =
    alg compose fmap[F, Fix[F], A](cata(alg)) compose unFix[F]

  def ana[F[_]: Functor, A](coalg: A => F[A]): A => Fix[F] =
    coalg andThen fmap[F, A, Fix[F]](ana(coalg)) andThen Fix[F]

  def anaCofix[F[_]: Functor, A](coalg: A => F[A]): A => Cofix[F] =
    coalg andThen fmap[F, A, Cofix[F]](anaCofix(coalg)) andThen Cofix[F]

  def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    ana(g) andThen cata(f)

  def para[F[_]: Functor, A](alg: F[(A, Fix[F])] => A): Fix[F] => A = {
    cata[F, (A, Fix[F])](&&&(alg, fmap[F, (A, Fix[F]), Fix[F]](_._2) andThen Fix[F])) andThen(_._1)
  }

  def apo[F[_]: Functor, A](coalg: A => F[Either[A, Fix[F]]]): A => Fix[F] = {
    Fix[F] _ compose fmap(|||(apo[F, A](coalg), identity[Fix[F]])) compose coalg
  }
  def histo[F[_]: Functor, A](alg: F[Ann.T[F, A]] => A): Fix[F] => A = {
    Ann.attr[F, A] compose cata(Ann.ann[F, A] compose &&&(identity[F[Ann.T[F, A]]], alg))
  }

  def zygo[F[_]: Functor, A, B](f: F[B] => B)(g: F[(A, B)] => A): Fix[F] => A = {
    def algZygo(f: F[B] => B)(g: F[(A, B)] => A): F[(A, B)] => (A, B) =
      &&&(g, fmap[F, (A, B), B](_._2) andThen f)

    cata[F, (A, B)](algZygo(f)(g)) andThen(_._1)
  }

  def futu[F[_]: Functor, A](coa: A => F[Ctx.T[F, A]]): A => Cofix[F] = {
    anaCofix[F, Ctx.T[F, A]](|||(coa, identity[F[Ctx.T[F, A]]]) compose Ctx.unCtx[F, A]) compose hole[F, A]
  }




}
