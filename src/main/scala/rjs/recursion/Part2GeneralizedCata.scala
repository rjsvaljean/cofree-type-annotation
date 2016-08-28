package rjs.recursion

import cats.Functor
import cats.std.set.setMonoid
import rjs.recursion.data._
import rjs.recursion.schemes.cata
import utils.&&&


object Part2GeneralizedCata {
  /*
 F[Fix[F]] - fmap(cata(alg)) -> F[A]
     |                            |
     |                            |
     |                            |
    Fix                          alg
     |                            |
     |                            |
     v                            v
  Fix[F]-------- cata alg ------> A
  */

//  def cata[F[_]: Functor, A](alg: F[A] => A): Fix[F] => A =
//    alg compose fmap[F, Fix[F], A](cata(alg)) compose unFix[F]


  type Env = Map[String, Int]
  import cats.std.option.optionInstance.tuple2

  def evalAlg[R](env: Env)(expr: ExprF[Option[Int]]): Option[Int] = {
    import ExprF.{Const, Var, Add, Mul, IfNeg}
    expr match {
      case Const(i) => Some(i)
      case Var(id) => env.get(id)
      case Add(r, l) => tuple2(r, l).map { case (_r, _l) => _r + _l }
      case Mul(r, l) => tuple2(r, l).map { case (_r, _l) => _r * _l }
      case IfNeg(condition, ifTrue, ifFalse) => condition.flatMap(c => if (c < 0) ifTrue else ifFalse)
    }
  }
  def eval(env: Env): Expr.T => Option[Int] = {
    cata(evalAlg(env))
  }


  // Pretty printer

  def pprAlg(exprF: ExprF[String]): String = {
    import ExprF._
    exprF match {
      case Const(i) => i.toString
      case Var(id) => id
      case Add(r, l) => s"( $r + $l )"
      case Mul(r, l) => s"$r * $l"
      case IfNeg(condition, ifTrue, ifFalse) => s"( if ($condition < 0) $ifTrue else $ifFalse )"
    }
  }
  val ppr = cata(pprAlg)

  def freeVarsAlg(exprF: ExprF[Set[String]]): Set[String] = {
    import ExprF._
    exprF match {
      case Var(id) => Set(id)
      case e => Foldable[ExprF].fold(e)
    }
  }
  val freeVars = cata(freeVarsAlg)

  def substituteAlg(context: Env)(exprF: ExprF[Expr.T]): Expr.T = {
    import ExprF._
    exprF match {
      case e @ Var(id) => context.get(id).fold(Fix(e: ExprF[Fix[ExprF]]))(Expr.Const)
      case e => Fix(e)
    }
  }
  def substitute(env: Env) = cata(substituteAlg(env))

  def run() = {
    import Expr.example
    println(s"Trying to evaluate: ${ppr(example)}")
    val context: Env = freeVars(example).map { freeVar =>
      println(s"Enter value for $freeVar:")
      val value = scala.io.StdIn.readInt()
      println(value)
      freeVar -> value
    }.toMap
    println(s"Substitued: ${ppr(substitute(context)(example))}")
    println(s"Evaluated: ${eval(context)(example)}")

    println("Trace:")
    val trace = Part5Paramorphisms.cataTrace(evalAlg(context))
    trace(optimizeFast(example)).foreach { case (k: Expr.T, v) =>
      println(s"${ppr(k)} -> $v")
    }


  }

  def optAdd(exprF: ExprF[Expr.T]): Expr.T = {
    import ExprF._
    exprF match {
      case Add(Fix(Const(0)), e) => e
      case Add(e, Fix(Const(0))) => e
      case e => Fix(e)
    }
  }

  def optMul(exprF: ExprF[Expr.T]): Expr.T = {
    import ExprF._
    exprF match {
      case Mul(Fix(Const(1)), e) => e
      case Mul(e, Fix(Const(1))) => e
      case e => Fix(e)
    }
  }

  // In general cata-morphisms don't compose
  val optimizeSlow: Expr.T => Expr.T = cata(optMul) andThen cata(optAdd) // two traversals

  // We need an algebra composition operator that gives us short-cut fusion
  // s.t. : cata f . cata g = cata ( f `algebraComposition` g )
  // For the special case:
  // f[F: Functor]: F[A] => A  and  g[F: Functor, G: Functor]: G[Fix[F]] => Fix[F]
  // comp f g = g andThen unFix andThen f

  val optimizeFast: Expr.T => Expr.T = cata(optMul _ andThen((_: Fix[ExprF]).unFix) andThen optAdd)

  // We have just applied the catamorphism compose law usually state in the form
  object CatamorphismComposeLaw {
    def f[F[_]: Functor, A]: F[A] => A = ???
    def g[F[_]: Functor, G[_]: Functor, A]: G[A] => F[A] = ???

    def lhs[F[_]: Functor, G[_]: Functor, A]: (Fix[G]) => A =
      cata(g[F, G, Fix[F]] andThen (Fix[F](_: F[Fix[F]]))) andThen cata(f[F, A])
    def rhs[F[_]: Functor, G[_]: Functor, A]: (Fix[G]) => A =
      cata(g[F, G, A] andThen f[F, A])
  }

  object BananaSplitTheorem {
    def f[F[_]: Functor, A]: F[A] => A = ???
    def g[F[_]: Functor, B]: F[B] => B = ???

    def lhs[F[_]: Functor, A, B]: (Fix[F]) => (A, B) = &&&(cata(f[F, A]), cata(g[F, B]))
    def rhs[F[_]: Functor, A, B]: (Fix[F]) => (A, B) = cata(
      &&&[F[(A, B)], A, B](
        (Functor[F].map(_: F[(A, B)])(_._1)) andThen f[F, A],
        (Functor[F].map(_: F[(A, B)])(_._2)) andThen g[F, B]
      )
    )
  }

  def countDownFrom(n: Int) = {
    val f = Fixpoint.cata[NatF, Int, List[Int]] {
      _.fold(List[Int]()) {
        case Nil => 1 :: Nil
        case all @ (i :: _) => (i + 1) :: all
      }
    }
    f(n)
  }

}
