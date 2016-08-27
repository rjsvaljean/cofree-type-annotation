package rjs

// Attributed to https://github.com/willtim/recursion-schemes/blob/master/slides.lhs

import cats.Functor
import FixPart.{Fix, cata, unFix}
import cats.data.ReaderT
import cats.{Functor, Monad}

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

  def ann[F[_]: Functor, A]: ((F[Ann[F, A]], A)) => Ann[F, A] = {
    Function.tupled(AnnF[F, A, Ann[F, A]] _) andThen Fix[AnnFA[F, A]#l]
  }

  def unAnn[F[_]: Functor, A, R]: Ann[F, A] => (F[Ann[F, A]], A) = { (annFA: Ann[F, A]) =>
    val AnnF(fr, a) = annFA.unFix
    (fr, a)
  }

  import RecursionSchemesTalk.{&&&, funzip}
  def synthesize[F[_]: Functor, A, R](f: F[A] => A): Fix[F] => Ann[F, A] = {
    val alg: F[Ann[F, A]] => Ann[F, A] =
      ann[F, A] compose &&&(identity[F[Ann[F, A]]], fmap[F, Ann[F, A], A](attr[F, A]) andThen f)
    cata(alg)
  }

  import cats.std.int.intGroup
  def sizes[F[_]: Functor : Foldable]: Fix[F] => Ann[F, Int] = synthesize(Foldable[F].foldMap(_ => 1))

  import ExprExample.{ExprF, pprAlg, example}

  def pprAnn[A]: Ann[ExprF, A] => String = {
    def alg(annf: AnnF[ExprF, A, String]): String = {
      "[ " + pprAlg(annf.fr) + " @ " + annf.a.toString + " ]"
    }
    cata[AnnFA[ExprF, A]#l, String](alg)
  }

  def inherit[F[_]: Functor, A](f: Fix[F] => A => A)(root: A)(n: Fix[F]): Ann[F, A] = {
    def alg(x: F[(A => Ann[F, A], Fix[F])])(p: A): Ann[F, A] = {
      val (ff, n) = funzip(x)
      val a: A = f(Fix[F](n))(p)
      val n_ : F[Ann[F, A]] = Functor[F].map(ff)(_(a))
      val _ann = ann[F, A]
      _ann((n_, a))
    }
    val _para = Paramorphism.para[F, A => Ann[F, A], Fix[F]](alg)
    _para(n)(root)
  }

  def depths[F[_]: Functor]: Fix[F] => Ann[F, Int] = inherit[F, Int](_ => (i: Int) => i + 1)(0)

  def run() = {
    val sizesF: (Fix[ExprF]) => Ann[ExprF, Int] = sizes[ExprF]
    val depthsF: (Fix[ExprF]) => Ann[ExprF, Int] = depths[ExprF]
    println(pprAnn(sizesF(example)))
    println(pprAnn(depthsF(example)))
  }
}

object MonadicVariants {
  import cats.data.Kleisli
  def cataM[M[_]: Monad, F[_]: Traversable, A](algM: F[A] => M[A]): Fix[F] => M[A] = {
    (Kleisli(Traversable[F].mapM(cataM(algM)) _ compose unFix[F]) andThen Kleisli(algM)).run
  }

  /*
    def eval(env: Env): Expr => Option[Int] = {
    def evalAlg[R](env: Env)(expr: ExprF[Option[Int]]): Option[Int] = {
      import ExprF._
      expr match {
        case Const(i) => Some(i)
        case Var(id) => env.get(id)
        case Add(r, l) => tuple2(r, l).map { case (_r, _l) => _r + _l }
        case Mul(r, l) => tuple2(r, l).map { case (_r, _l) => _r * _l }
        case IfNeg(condition, ifTrue, ifFalse) => condition.flatMap(c => if (c < 0) ifTrue else ifFalse)
      }
    }

    cata(evalAlg(env))
  }
  * */

  import rjs.ExprExample.{Env, Expr, ExprF}
  import cats.data.Kleisli._
  import cats.std.option._

  type EnvReader[A] = ReaderT[Option, Env, A]
  def eval(env: Env): Expr => Option[Int] = {
    val algM: ExprF[Int] => ReaderT[Option, Env, Int] = {
      case ExprF.Const(c) => Monad[EnvReader].pure(c)
      case ExprF.Var(i) => ReaderT.ask[Option, Env].flatMap(_env => Kleisli { _: Env => _env.get(i) })
      case ExprF.Add(x, y) => Monad[EnvReader].pure(x + y)
      case ExprF.Mul(x, y) => Monad[EnvReader].pure(x * y)
      case ExprF.IfNeg(t, x, y) => Monad[EnvReader].pure(if (t < 0) x else y)
    }
    val f: (Expr) => EnvReader[Int] = cataM[EnvReader, ExprF, Int](algM)
    f andThen (_.run(env))
  }


//  def memoize[K, V, M[_]](f: K => M[V])(x: K): M[V] = {

}
