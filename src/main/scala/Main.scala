package rjs

import cats.syntax.semigroup._
import cats.data._
import cats.{Traverse, Monoid}

object Main {
  def main(args: Array[String]): Unit = {
    println(CoFree.cofreeMu(AST.example))
    println(attribute(CoFree.cofreeMu(AST.example)))
    println(typeTree(CoFree.cofreeMu(AST.example)))
  }

  def freshVarId[ T, M ]: State[ TypeState[ T, M ], Type ] =
    for {
      v <- State.inspect( ( _: TypeState[ T, M ] ).varId )
      _ <- State.modify( ( ts: TypeState[ T, M ] ) => ts.copy( varId = ts.varId + 1 ) )
    } yield TVar( v )


  def memoizedTC[ C ]( f: C => TypeCheck.TYPE[ C ], c: C ): TypeCheck.TYPE[ C ] = {
    def memoize: State[ TypeState[ C, (Type, TypeResult) ], (Type, TypeResult) ] = for {
      r <- f( c )
      _ <- State.modify( ( ts: TypeState[ C, (Type, TypeResult) ] ) => ts.copy( memo = ts.memo + ( c -> r ) ) )
    } yield r
    State.inspect( ( _: TypeState[ C, (Type, TypeResult) ] ).memo ).
      flatMap( _.get( c ).fold( memoize )( State.pure ) )
  }

  def generateConstraints( c: CoFree[ AST, Unit ] ): TypeCheck.TYPE[ CoFree[ AST, Unit ] ] = {
    c.tail match {
      case ANumber( _ ) => State.pure( (TNumber, Monoid[ TypeResult ].empty) )
      case AString( _ ) => State.pure( (TString, Monoid[ TypeResult ].empty) )
      case AIdent( s ) => for {
        vaar <- freshVarId
      } yield (vaar, TypeResult( Nil, Map( s -> List(vaar) ) ))
      case ALambda( s, b ) => for {
        vaar <- freshVarId
        br <- memoizedTC(generateConstraints, b)
        cs = br._2.assumptions.get(s).fold(List[Constraint]())(_.map(EqualityConstraint(vaar, _)))
        as = br._2.assumptions - s
      } yield (
        TLambda(vaar, br._1) : Type,
        TypeResult( br._2.constraints ::: cs, Map( s -> List(vaar) ) )
        )
      case AApply( a, b ) => for {
        vaar <- freshVarId
        ar <- memoizedTC(generateConstraints, a)
        br <- memoizedTC(generateConstraints, b)
      } yield (
        vaar,
        ar._2 |+| br._2 |+| TypeResult(List(EqualityConstraint(ar._1, TLambda(br._1, vaar))), Map())
        )
    }
  }

  def attribute( c: CoFree[ AST, Unit ] ): CoFree[ AST, (Type, TypeResult) ] = {
    val initial = TypeState[CoFree[AST, Unit], (Type, TypeResult)]( memo = Map( ), varId = 0 )

    Traverse[({type l[ a ] = CoFree[ AST, a ]} )#l].
      sequence[
      ({type l[ a ] = State[TypeState[ CoFree[ AST, Unit ], (Type, TypeResult) ], a]} )#l,
      (Type, TypeResult)
      ](c.extend( memoizedTC[ CoFree[ AST, Unit ] ]( generateConstraints, _ ) )).
      run(initial).
      value.
      _2
  }

  val solveConstraints: List[Constraint] => Option[Map[Int, Type]] = _.foldLeft(
    Option(Map[Int, Type]())
  ) { ( b: Option[ Map[ Int, Type ] ], a: Constraint ) =>
    def solve( maybeSubs: Option[ Map[ Int, Type ] ], const: Constraint ) = {
      const match {
        case EqualityConstraint(_a, _b) => for {
          subs <- maybeSubs
          t <- mostGeneralUnifier(substitute(subs, _a), substitute(subs, _b))
        } yield t
      }
    }
    cats.std.option.optionInstance.map2(solve(b, a), b)(_ ++ _)
  }



  private def mostGeneralUnifier(t1: Type, t2: Type): Option[Map[Int, Type]] = {
    (t1, t2) match {
      case (TVar(i), b) => Some(Map(i -> b))
      case (a, TVar(i)) => Some(Map(i -> a))
      case (TNumber, TNumber) => Some(Map())
      case (TString, TString) => Some(Map())
      case (TLambda(a, b), TLambda(c, d)) => for {
        s1 <- mostGeneralUnifier(a, c)
        o <- mostGeneralUnifier(substitute(s1, b), substitute(s1, d))
      } yield s1 ++ o
      case _ => None
    }
  }

  private def substitute(m: Map[Int, Type], t: Type): Type = (m, t) match {
    case (subs, v@TVar(i)) => subs.get(i).fold(v : Type)(substitute(subs, _))
    case (subs, TLambda(a, b)) => TLambda(substitute(subs, a), substitute(subs, b))
    case (_, t) => t
  }

  def typeTree(c: CoFree[AST, Unit]): Option[CoFree[AST, Type]] = {
    val result = attribute(c)
    val r = result.head
    val constraints = r._2.constraints
    val maybeSubs = solveConstraints(constraints)
    maybeSubs.map(subs => result.map(r => substitute(subs, r._1)))
  }
}
