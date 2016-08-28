package rjs.recursion

import cats.Monad
import cats.data._
import rjs.recursion.data.{Expr, ExprF, Fix, Traversable, unFix}
import rjs.recursion.Part2GeneralizedCata.Env

object Part8MonadicVariants {
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

  import cats.data.Kleisli._
  import cats.std.option._

  type EnvReader[A] = ReaderT[Option, Env, A]
  def eval(env: Env): Expr.T => Option[Int] = {
    val algM: ExprF[Int] => ReaderT[Option, Env, Int] = {
      case ExprF.Const(c) => Monad[EnvReader].pure(c)
      case ExprF.Var(i) => ReaderT.ask[Option, Env].flatMap(_env => Kleisli { _: Env => _env.get(i) })
      case ExprF.Add(x, y) => Monad[EnvReader].pure(x + y)
      case ExprF.Mul(x, y) => Monad[EnvReader].pure(x * y)
      case ExprF.IfNeg(t, x, y) => Monad[EnvReader].pure(if (t < 0) x else y)
    }
    val f: (Expr.T) => EnvReader[Int] = cataM[EnvReader, ExprF, Int](algM)
    f andThen (_.run(env))
  }


//  def memoize[K, V, M[_]](f: K => M[V])(x: K): M[V] = { ooh boy ... maybe later

}
