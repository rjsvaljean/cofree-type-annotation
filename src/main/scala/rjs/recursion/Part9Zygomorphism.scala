package rjs.recursion

import cats.std.int.intGroup
import rjs.recursion.Part2GeneralizedCata._
import rjs.recursion.data.Expr.example
import rjs.recursion.data._
import rjs.recursion.schemes.zygo
import rjs.recursion.utils.fmap

object Part9Zygomorphism {

  val discontAlg: ExprF[(Int, Option[Int])] => Int = {
    case ExprF.IfNeg((t, tv), (x, Some(xv)), (y, Some(yv))) if xv == yv =>
      t + x + y
    case ExprF.IfNeg((t, Some(tv)), (x, xv), (y, yv)) =>
      if (tv < 0) t + x else t + y
    case ExprF.IfNeg((t, None), (x, xv), (y, yv)) =>
      1 + t + x  + y
    case e => (Foldable[ExprF].fold[Int] _ compose fmap[ExprF, (Int, Option[Int]), Int](_._1))(e)
  }
  def disconts(env: Env): Expr.T => Int = zygo(evalAlg(env))(discontAlg)

  val e2 = Expr.IfNeg(Expr.Var("b"), example, Expr.Const(4))
  def run = (
    freeVars(e2),
    ppr(optimizeFast(e2)),
    disconts(Map("b" -> -1))(e2)
  )

}
