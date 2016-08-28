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

object Apomorphism {
  import ExprExample.{Fixpoint, inF, outF}
  import ExprExample.Fixpoint.{ana, cata}
  import FixPart.{ListF, ListFA, NilF, Cons}
  import RecursionSchemesTalk.|||
  def apo[F[_], A, T](coa: A => F[Either[A, T]])(implicit fixpoint: Fixpoint[F, T]): A => T =
    inF[F, T] compose fmap(|||(apo(coa), identity[T]))(fixpoint.functor) compose coa

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
      case Cons(x, Nil) => Cons(x, Left(NilF))
      case Cons(x, y :: xs) if x <= y => Cons(x, Right(y :: xs))
      case Cons(x, y :: xs) if x > y => Cons(y, Left(Cons(x, xs)))
    }
    apo[ListFA[Int]#l, ListF[Int, List[Int]], List[Int]](c)
  }

  def insertionSort: List[Int] => List[Int] = cata[ListFA[Int]#l, List[Int], List[Int]](insertElem)
}

object Zygomorphism {

  import FixPart.cata
  import RecursionSchemesTalk.&&&
  import ExprExample.{ExprF, Env, Expr, evalAlg, example, freeVars, ppr, optimizeFast}
  import ExprExample.ExprF.IfNeg
  import cats.std.int.intGroup

  def algZygo[F[_]: Functor, A, B](f: F[B] => B)(g: F[(A, B)] => A): F[(A, B)] => (A, B) =
    &&&(g, fmap[F, (A, B), B](_._2) andThen f)

  def zygo[F[_]: Functor, A, B](f: F[B] => B)(g: F[(A, B)] => A): Fix[F] => A =
    cata[F, (A, B)](algZygo(f)(g)) andThen(_._1)

  val discontAlg: ExprF[(Int, Option[Int])] => Int = {
    case IfNeg((t, tv), (x, Some(xv)), (y, Some(yv))) if xv == yv =>
      t + x + y
    case IfNeg((t, Some(tv)), (x, xv), (y, yv)) =>
      if (tv < 0) t + x else t + y
    case IfNeg((t, None), (x, xv), (y, yv)) =>
      1 + t + x  + y
    case e => (Foldable[ExprF].fold[Int] _ compose fmap[ExprF, (Int, Option[Int]), Int](_._1))(e)
  }
  def disconts(env: Env):  Expr => Int = zygo(evalAlg(env))(discontAlg)

  val e2 = Expr.IfNeg(Expr.Var("b"), example, Expr.Const(4))
  def run = (
    freeVars(e2),
    ppr(optimizeFast(e2)),
    disconts(Map("b" -> -1))(e2)
  )

}

object Histomorphism {
  import Annotating.{Ann, attr, ann, unAnn, strip}
  import RecursionSchemesTalk.&&&
  import ExprExample.Fixpoint.cata
  import FixPart.{NatF, ListF, ListFA, NilF, Cons}
  import ExprExample.Fixpoint
  def histo[F[_], A, T](alg: F[Ann[F, A]] => A)(implicit fixPoint: Fixpoint[F, T]): T => A = {
    implicit val functor: Functor[F] = fixPoint.functor
    attr[F, A] compose cata(ann[F, A] compose &&&(identity[F[Ann[F, A]]], alg))
  }

  def fib: Int => Int = {
    val unAnnNat = unAnn[NatF, Int, Int]
    def f: NatF[Ann[NatF, Int]] => Int = {
      case FixPart.Zero => 0
      case FixPart.Succ(i) => unAnnNat(i) match {
        case (FixPart.Zero, _) => 1
        case (FixPart.Succ(_i), m) => unAnnNat(_i) match {
          case (_, n) => n + m
        }
      }
    }
    histo(f)
  }

  def evens[A]: List[A] => List[A] = {
    def alg: ListF[A, Ann[ListFA[A]#l, List[A]]] => List[A] = {
      case NilF => List[A]()
      case Cons(_, i) => strip[ListFA[A]#l, List[A]](i) match {
        case NilF => List[A]()
        case Cons(x, y) => x :: attr[ListFA[A]#l, List[A]](y)
      }
    }
    histo[ListFA[A]#l, List[A], List[A]](alg)
  }
}
