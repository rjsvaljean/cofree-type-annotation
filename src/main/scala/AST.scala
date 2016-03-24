package rjs

import cats._
import cats.syntax.semigroup._
import cats.data.State

sealed trait AST[ +A ]

case class ALambda[ A ]( arg: String, lam: A ) extends AST[ A ]

case class AApply[ A ]( abstractExpr: A, appliedTo: A ) extends AST[ A ]

case class ANumber( i: Int ) extends AST[ Nothing ]

case class AString( s: String ) extends AST[ Nothing ]

case class AIdent( id: String ) extends AST[ Nothing ]

case class Mu[ F[ _ ] ]( value: F[ Mu[ F ] ] )

object AST {
  type Unfixed = Mu[ AST ]

  def example: Unfixed = Mu[ AST ]( AApply(
    Mu[ AST ]( ALambda( "x", Mu[ AST ]( AIdent( "x" ) ) ) ),
    Mu[ AST ]( ANumber( 2 ) )
  ) )

  implicit val traversable: Traverse[AST] = new Traverse[AST] {
    def traverse[ G[ _ ], A, B ](
      fa: AST[ A ]
    )(
      f: ( A ) => G[ B ]
    )(
      implicit ap: Applicative[ G ]
    ): G[ AST[ B ] ] = {
      fa match {
        case n @ ANumber(_) => ap.pure(n)
        case s @ AString(_) => ap.pure(s)
        case id @ AIdent(_) => ap.pure(id)
        case AApply(abs, appliedTo) =>
          ap.map2(f(abs), f(appliedTo))(AApply(_, _))
        case ALambda(arg, lam) =>
          ap.map2(ap.pure(arg), f(lam))(ALambda(_, _))
      }
    }

    def foldLeft[ A, B ]( fa: AST[ A ], b: B )( f: (B, A) => B ): B = ???

    def foldRight[ A, B ]( fa: AST[ A ], lb: Eval[ B ] )( f: (A, Eval[ B ]) => Eval[ B ] ): Eval[ B ] = ???
  }
}

sealed trait Type

case class TLambda( t1: Type, t2: Type ) extends Type

case class TVar( i: Int ) extends Type

case object TNumber extends Type

case object TString extends Type

sealed trait Constraint

// A Type Constraint
case class EqualityConstraint( t1: Type, t2: Type ) extends Constraint

case class TypeResult( constraints: List[ Constraint ], assumptions: Map[ String, List[Type] ] )

object TypeResult {
  implicit val monoid: Monoid[ TypeResult ] = new Monoid[ TypeResult ] {
    def empty: TypeResult = TypeResult( Nil, Map( ) )

    def combine( x: TypeResult, y: TypeResult ): TypeResult =
      TypeResult(
        constraints = x.constraints ::: y.constraints,
        assumptions = {
          x.assumptions.foldLeft(y.assumptions) { case (acc, (k, vs)) =>
            acc + (k -> acc.get(k).fold(vs)(_ ::: vs))
          }
        }
      )
  }
}

case class TypeState[ T, M ]( varId: Int, memo: Map[ T, M ] )

object TypeCheck {
  type TYPE[ T ] = State[ TypeState[ T, (Type, TypeResult) ], (Type, TypeResult) ]

}

case class CoFree[ S[ _ ] : Functor, A ]( head: A, tail: S[ CoFree[ S, A ] ] ) {
  def functor = Functor[ S ]

  final def extract: A = head

  final def map[ B ]( f: A => B ): CoFree[ S, B ] =
    applyCoFree( f, _ map f )

  /** Redecorates this structure with a computation whose context is the entire structure under that value. */
  final def extend[ B ]( f: CoFree[ S, A ] => B ): CoFree[ S, B ] =
    applyTail( f( this ), _ extend f )

  /** Replaces the head with `b` and applies `g` through the tail. */
  final def applyTail[ B ]( b: B, g: CoFree[ S, A ] => CoFree[ S, B ] ): CoFree[ S, B ] =
    applyCoFree( x => b, g )

  /** Applies `f` to the head and `g` through the tail. */
  final def applyCoFree[ B ]( f: A => B, g: CoFree[ S, A ] => CoFree[ S, B ] ): CoFree[ S, B ] =
    CoFree( f( head ), functor.map( tail )( g ) )

}

object CoFree {
  def cofreeMu[ F[ _ ] : Functor ]( muf: Mu[ F ] ): CoFree[ F, Unit ] = {
    val Mu( f ) = muf
    CoFree[ F, Unit ]( (), Functor[ F ].map( f )( cofreeMu( _ )( Functor[ F ] ) ) )
  }

  implicit def traversable[ S[ _ ] : Traverse ]: Traverse[ ({type l[ a ] = CoFree[ S, a ]} )#l ] =
    new Traverse[ ({type l[ a ] = CoFree[ S, a ]} )#l ] {
      def traverse[ G[ _ ], A, B ](
        fa: CoFree[ S, A ]
      )(
        f: ( A ) => G[ B ]
      )(
        implicit ap: Applicative[ G ]
      ): G[ CoFree[ S, B ] ] = {
        val ga: G[ S[ CoFree[ S, B ] ] ] = Traverse[ S ].traverse( fa.tail )( traverse( _ )( f ) )
        val gb: G[ B ] = f(fa.head)
        ap.map2(gb, ga)(CoFree[ S, B ])
      }

      def foldLeft[ A, B ]( fa: CoFree[ S, A ], b: B )( f: (B, A) => B ): B = ???

      def foldRight[ A, B ]( fa: CoFree[ S, A ], lb: Eval[ B ] )( f: (A, Eval[ B ]) => Eval[ B ] ): Eval[ B ] = ???
    }
}