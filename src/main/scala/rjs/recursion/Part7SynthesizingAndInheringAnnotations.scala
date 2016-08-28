package rjs.recursion

// Attributed to https://github.com/willtim/recursion-schemes/blob/master/slides.lhs

import cats.Functor
import cats.std.int.intGroup
import rjs.recursion.Part2GeneralizedCata.pprAlg
import rjs.recursion.data.Expr.example
import rjs.recursion.data.Fixpoint.para
import rjs.recursion.data._
import rjs.recursion.schemes.cata
import rjs.recursion.utils.{&&&, fmap, funzip}

object Part7SynthesizingAndInheringAnnotations {

  // - useful for storing intermediate values
  // - inspired by ideas from *attribute grammars*
  def synthesize[F[_]: Functor, A, R](f: F[A] => A): Fix[F] => Ann.T[F, A] = {
    val alg: F[Ann.T[F, A]] => Ann.T[F, A] =
      Ann.ann[F, A] compose &&&(identity[F[Ann.T[F, A]]], fmap[F, Ann.T[F, A], A](Ann.attr[F, A]) andThen f)
    cata(alg)
  }

  def sizes[F[_]: Functor : Foldable]: Fix[F] => Ann.T[F, Int] = synthesize(Foldable[F].foldMap(_ => 1))

  def pprAnn[A]: Ann.T[ExprF, A] => String = {
    def alg(annf: AnnF[ExprF, A, String]): String = {
      "[ " + pprAlg(annf.fr) + " @ " + annf.a.toString + " ]"
    }
    cata[AnnFA[ExprF, A]#l, String](alg)
  }

  def inherit[F[_]: Functor, A](f: Fix[F] => A => A)(root: A)(n: Fix[F]): Ann.T[F, A] = {
    def alg(x: F[(A => Ann.T[F, A], Fix[F])])(p: A): Ann.T[F, A] = {
      val (ff, n) = funzip(x)
      val a: A = f(Fix[F](n))(p)
      val n_ : F[Ann.T[F, A]] = Functor[F].map(ff)(_(a))
      val _ann = Ann.ann[F, A]
      _ann((n_, a))
    }
    val _para = para[F, A => Ann.T[F, A], Fix[F]](alg)
    _para(n)(root)
  }

  def depths[F[_]: Functor]: Fix[F] => Ann.T[F, Int] = inherit[F, Int](_ => (i: Int) => i + 1)(0)

  def run() = {
    val sizesF: (Fix[ExprF]) => Ann.T[ExprF, Int] = sizes[ExprF]
    val depthsF: (Fix[ExprF]) => Ann.T[ExprF, Int] = depths[ExprF]
    println(pprAnn(sizesF(example)))
    println(pprAnn(depthsF(example)))
  }
}










