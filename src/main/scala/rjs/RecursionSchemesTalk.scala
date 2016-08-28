package rjs

// Original code in Haskell attributed to willtim here: https://raw.githubusercontent.com/willtim/recursion-schemes/master/slides.lhs

import cats.std.int.intGroup
import cats.std.option.optionInstance.tuple2
import cats.std.set.setMonoid
import cats.{Applicative, Functor, Monad, Monoid, Order}
import rjs.FixAnamorphims.Cofix
import rjs.FixPart.{ListF, NatF}
import rjs.UnfixedJson.JsValueF
import rjs.json.pJSValueF

object RecursionSchemesTalk {
  def &&&[B, C1, C2](f: B => C1, g: B => C2): B => (C1, C2) = {
    (b: B) => (f(b), g(b))
  }

  def |||[B1, B2, C](f: B1 => C, g: B2 => C): Either[B1, B2] => C = {
    _.fold(f, g)
  }

  def ***[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = Function.tupled {
    (a: A, b: C) => (f(a), g(b))
  }

  def funzip[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = (
    Functor[F].map(fab)(_._1),
    Functor[F].map(fab)(_._2)
  )
}

/**
Foldable
========

The Foldable class gives you the ability to process the elements of a structure
one-at-a-time, discarding the shape.

Intuitively: list-like fold methods
  */
trait Foldable[T[_]] {
  def foldMap[M : Monoid, A](f: A => M)(ta: T[A]): M
  def fold[M : Monoid](tm: T[M]): M = foldMap[M, M](identity)(tm)
  def foldr[A, B](f: A => B => B)(b: B)(ta: T[A]): B = ???
  def foldl[A, B](f: A => B => A)(a: A)(tb: T[B]): A = ???
  def foldr1[A](f: A => A => A)(ta: T[A]): A = ???
  def foldl1[A](f: A => A => A)(ta: T[A]): A = ???
}

object Foldable {
  def apply[T[_]: Foldable] = implicitly[Foldable[T]]
  def count[T[_] : Foldable, A] = Foldable[T].foldMap((_: A) => 1) _
}

/**
Traversable
===========

Traversable gives you the ability to traverse a structure from left-to-right,
performing an effectful action on each element and preserving the shape.

Intuitively: fmap with effects
  */
trait Traversable[T[_]] {
  def traverse[F[_]: Applicative, A, B](f: A => F[B])(ta: T[A]): F[T[B]]

  // sequence can also be thought of as a generalised matrix transpose![!!]
  def sequenceA[F[_]: Applicative, A](tfa: T[F[A]]): F[T[A]] =
    traverse[F, F[A], A](identity[F[A]])(tfa)

  def mapM[M[_]: Monad, A, B](f: A => M[B])(ta: T[A]): M[T[B]] =
    traverse[M, A, B](f)(ta)

  def sequence[M[_]: Monad, A](tma: T[M[A]]): M[T[A]] =
    mapM[M, M[A], A](identity[M[A]])(tma)
}

object Traversable {
  def apply[T[_]](implicit traversable: Traversable[T]) = traversable
}

trait Tree[+A] extends Product with Serializable
case object Empty extends Tree[Nothing]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  implicit object foldable extends Foldable[Tree] {
    def foldMap[M: Monoid, A](f: (A) => M)(ta: Tree[A]): M = ta match {
      case Empty => Monoid[M].empty
      case Leaf(a) => f(a)
      case Node(l, r) => Monoid[M].combine(foldMap(f)(l), foldMap(f)(r))
    }
  }

  implicit object functor extends Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Empty => Empty
      case Leaf(a) => Leaf(f(a))
      case Node(l, r) => Node(map(l)(f), map(r)(f))
    }
  }

  implicit object traversable extends Traversable[Tree] {
    implicit def functor: Functor[Tree] = Functor[Tree]

    implicit def foldable: Foldable[Tree] = Foldable[Tree]

    def traverse[F[_] : Applicative, A, B](f: (A) => F[B])(ta: Tree[A]): F[Tree[B]] = ta match {
      case Empty => Applicative[F].pure(Empty)
      case Leaf(a) => Applicative[F].map(f(a))(Leaf(_))
      case Node(l, r) =>
        Applicative[F].ap2[Tree[B], Tree[B], Tree[B]](Applicative[F].pure(Node[B]))(traverse(f)(l), traverse(f)(r))
    }
  }
}

/**
What if we need to access the structure?
----------------------------------------

We need to work with a domain of (`f a`) instead of `a`


Catamorphisms
=============

A *catamorphism* (cata meaning “downwards”) is a generalisation of the concept of a fold.

- models the fundamental pattern of (internal) *iteration*
- for a list, it describes bracketing from the right
- for a tree, it describes a bottom-up traversal, i.e. children first

*/


object foldr {
  import RecursionSchemesTalk.***
  //`foldr` on list is a specialised catamorphism

  def foldr0[A, B](f: A => B => B)(b: B)(as: List[A]): B = as match {
    case Nil => b
    case a :: _as => foldr0(f)(f(a)(b))(_as)
  }

  // We can express the parameters used above in terms of a single
  // F-algebra `F[B] => B` over a functor `F` and carrier `B`

  def foldr1[A, B](alg: Option[(A, B)] => B)(as: List[A]): B =
    as match {
      case Nil => alg(None)
      case a :: _as => alg(Some((a, foldr1(alg)(_as))))
    }

  // here F = Option[(A, _)]
  // Can also be written as

  def foldr2[A, B](alg: Option[(A, B)] => B): List[A] => B = {
    val mapIdAndFoldAlg: Option[(A, List[A])] => Option[(A, B)] =
      _.map(***(identity[A], foldr2(alg)))

    val unlist: List[A] => Option[(A, List[A])] = {
      case Nil => None
      case a :: _as => Some((a, _as))
    }

    alg compose mapIdAndFoldAlg compose unlist
  }

  // This definition of `foldr` can literally be read from the commutative diagram below
  // The nodes represent types (objects) and the edges functions (morphisms)
/*
  Option[(A, List[A])] ---- mapIdAndFoldAlg ---> Option[(A, B)]  |   F[A] ---- α -----> A
           ^                                           |         |    |                 |
           |                                           |         |    |                 |
           |                                           |         |    |                 |
        unList                                        alg        |   F(f)               f
           |                                           |         |    |                 |
           |                                           |         |    |                 |
           |                                           v         |    v                 v
         List[A] ----------- foldr2(alg) ------------> B         |   F[B]----- β -----> B
*/

  def length2[A] = {
    val alg: Option[(A, Int)] => Int = {
      case None => 0
      case Some((_, len)) => len + 1
    }

    foldr2(alg)
  }

  def foldl[A, B](f: B => A => B): (List[A]) => (B) => B = {
    val alg: Option[(A, B => B)] => (B => B) = {
      case None => identity[B]
      case Some((a, bToB)) => (b: B) => bToB(f(b)(a))
    }
    foldr2(alg)
  }

  // Fixed point of Functors
  // Gives us
  // - data-type generic functions
  // - compositional data


}

object FixPart {
  case class Fix[F[_] : Functor](unFix: F[Fix[F]])
  def unFix[F[_]]: Fix[F] => F[Fix[F]] = _.unFix
  final def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  // Data type generic programming
  // - allows parametrise functions on the structure or shape of a data-type
  // - useful for large complex data-type where boilerplate traversal code often dominates, especially when updating a small subset of constructors
  // - for recursion schemes, we can capture the pattern as a standalone combinator
  // Limitations:
  // - the set of data-types that can be represented by means of Fix is limited to _regular_ data-types (da faq?)


  sealed trait ListF[+A, +R] extends Product with Serializable {
    def fold[X](ifNil: => X)(ifCons: (A, R) => X): X
  }
  trait ListFA[A] {
    type l[r] = ListF[A, r]
  }
  case object NilF extends ListF[Nothing, Nothing] {
    def fold[X](ifNil: => X)(ifCons: (Nothing, Nothing) => X): X = ifNil
  }
  case class Cons[A, R](a: A, r: R) extends ListF[A, R] {
    def fold[X](ifNil: => X)(ifCons: (A, R) => X) = ifCons(a, r)
  }
  def cons[A, R](a: A, r: R): ListF[A, R] = Cons(a, r)

  type MyList[A] = Fix[({type l[r] = ListF[A, r]})#l]
  def myList[A](as: A*): MyList[A] = as.foldLeft(
    Fix[ListFA[A]#l](NilF): MyList[A]
  )(
    (l: MyList[A], i: A) => Fix[ListFA[A]#l](Cons[A, MyList[A]](i, l))
  )

  object ListF {
    implicit def functor[AA]: Functor[ListFA[AA]#l] = new Functor[ListFA[AA]#l] {
      def map[A, B](fa: ListF[AA, A])(f: (A) => B): ListF[AA, B] = fa match {
        case NilF => NilF
        case Cons(a, r) => Cons(a, f(r))
      }
    }
    implicit def foldable[AA]: Foldable[ListFA[AA]#l] = new Foldable[ListFA[AA]#l] {
      def foldMap[M: Monoid, R](f: (R) => M)(ta: ListF[AA, R]): M = ta match {
        case Cons(_, r) => f(r)
        case NilF => Monoid[M].empty
      }
    }
  }

  sealed trait NatF[+R] extends Product with Serializable {
    def fold[X](ifZero: => X)(ifNonZero: R => X): X
  }
  case object Zero extends NatF[Nothing] {
    def fold[X](ifZero: => X)(ifNonZero: (Nothing) => X): X = ifZero
  }
  case class Succ[R](r: R) extends NatF[R] {
    def fold[X](ifZero: => X)(ifNonZero: (R) => X): X = ifNonZero(r)
  }

  object NatF {
    implicit object functor extends Functor[NatF] {
      def map[A, B](fa: NatF[A])(f: (A) => B): NatF[B] = fa match {
        case Zero => Zero
        case Succ(r) => Succ(f(r))
      }
    }
  }

  def cata[F[_]: Functor, A](alg: F[A] => A): Fix[F] => A = {
    val unFix: (Fix[F]) => F[Fix[F]] = (_: Fix[F]).unFix
    val fmap_cata_alg: (F[Fix[F]]) => F[A] = Functor[F].map(_: F[Fix[F]])(cata(alg))
    alg compose fmap_cata_alg compose unFix
  }

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

  // Fusion law: lhs = rhs => lhs1 = rhs1
  def f[F[_], A, B]: F[A] => A = ???
  def g[F[_], A, B]: F[B] => B = ???
  def h[A, B]: A => B = ???

  def lhs[F[_] : Functor, A, B]: (F[A]) => B = // h . f
    h[A, B] compose f[F, A, B]
  def rhs[F[_] : Functor, A, B]: (F[A]) => B = // g . fmap(h)
    g[F, A, B] compose (Functor[F].map[A, B](_: F[A])(h))

  def lhs1[F[_]: Functor, A, B]: (Fix[F]) => B = // h . cata(f)
    h[A, B] compose cata(f[F, A, B])
  def rhs1[F[_]: Functor, A, B]: (Fix[F]) => B = // cata(g)
    cata(g[F, A, B])
}

object ExprExample {
  import FixPart.{Fix, cata}
  sealed trait ExprF[+R]
  type Expr = Fix[ExprF]

  // The `pattern functor` ExprF represents the structure of type Expr
  // The isomorphism between a data-type and its pattern functor type
  //   is witnessed by the functions Fix(_) and (_:Fix[F]).unfix

  object ExprF {
    case class Const(i: Int) extends ExprF[Nothing]
    case class Var(id: String) extends ExprF[Nothing]
    case class Add[+R](r: R, l: R) extends ExprF[R]
    case class Mul[+R](r: R, l: R) extends ExprF[R]
    case class IfNeg[+R](condition: R, ifTrue: R, ifFalse: R) extends ExprF[R]

    implicit object functor extends Functor[ExprF] {
      def map[A, B](fa: ExprF[A])(f: (A) => B): ExprF[B] = fa match {
        case Const(i) => Const(i)
        case Var(id) => Var(id)
        case Add(r, l) => Add(f(r), f(l))
        case Mul(r, l) => Mul(f(r), f(l))
        case IfNeg(c, ifT, ifF) => IfNeg(f(c), f(ifT), f(ifF))
      }
    }

    implicit object traversable extends Traversable[ExprF] {
      def traverse[F[_] : Applicative, A, B](f: (A) => F[B])(ta: ExprF[A]): F[ExprF[B]] = {
        ta match {
          case const @ Const(_) => Applicative[F].pure(const)
          case vaar @ Var(id) => Applicative[F].pure(vaar)
          case Add(r, l) =>
            Applicative[F].map2(f(r), f(l))(Add(_, _))
          case Mul(r, l) =>
            Applicative[F].map2(f(r), f(l))(Mul(_, _))
          case IfNeg(c, ifT, ifF) =>
            Applicative[F].map3(f(c), f(ifT), f(ifF))(IfNeg(_, _, _))
        }
      }
    }

    implicit object foldable extends Foldable[ExprF] {
      def foldMap[M: Monoid, A](f: (A) => M)(ta: ExprF[A]): M = {
        ta match {
          case Const(i) => Monoid[M].empty
          case Var(id) => Monoid[M].empty
          case Add(r, l) => Monoid[M].combine(f(r), f(l))
          case Mul(r, l) => Monoid[M].combine(f(r), f(l))
          case IfNeg(c, ifT, ifF) => Monoid[M].combineAll(Seq(f(c), f(ifT), f(ifF)))
        }

      }
    }

  }

  object Expr {
    def IfNeg[R](condition: Expr, ifTrue: Expr, ifFalse: Expr): Expr = Fix(ExprF.IfNeg(condition, ifTrue, ifFalse))
    def Mul[R](r: Expr, l: Expr): Expr = Fix(ExprF.Mul(r, l))
    def Add[R](r: Expr, l: Expr): Expr = Fix(ExprF.Add(r, l))
    def Var(id: String): Expr = Fix(ExprF.Var(id): ExprF[Fix[ExprF]])
    def Const(i: Int): Expr = Fix(ExprF.Const(i): ExprF[Fix[ExprF]])
  }

  val example = {
    import Expr._
    Mul(
      IfNeg(
        Mul(Const(1), Var("a")),
        Add(Var("b"), Const(0)),
        Add(Var("b"), Const(2))
      ),
      Const(3)
    )
  }



  // Evaluator with a global environment

  type Env = Map[String, Int]
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
  def eval(env: Env): Expr => Option[Int] = {
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

  def substituteAlg(context: Env)(exprF: ExprF[Expr]): Expr = {
    import ExprF._
    exprF match {
      case e @ Var(id) => context.get(id).fold(Fix(e: ExprF[Fix[ExprF]]))(Expr.Const)
      case e => Fix(e)
    }
  }
  def substitute(env: Env) = cata(substituteAlg(env))

  def run() = {
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
    val trace = rjs.Paramorphism.Examples.cataTrace(evalAlg(context))
    trace(optimizeFast(example)).foreach { case (k: Fix[ExprF], v) =>
      println(s"${ppr(k)} -> $v")
    }


  }

  def optAdd(exprF: ExprF[Expr]): Expr = {
    import ExprF._
    exprF match {
      case Add(Fix(Const(0)), e) => e
      case Add(e, Fix(Const(0))) => e
      case e => Fix(e)
    }
  }

  def optMul(exprF: ExprF[Expr]): Expr = {
    import ExprF._
    exprF match {
      case Mul(Fix(Const(1)), e) => e
      case Mul(e, Fix(Const(1))) => e
      case e => Fix(e)
    }
  }

  // In general cata-morphisms don't compose
  val optimizeSlow: Expr => Expr = cata(optMul) andThen cata(optAdd) // two traversals

  // We need an algebra composition operator that gives us short-cut fusion
  // s.t. : cata f . cata g = cata ( f `algebraComposition` g )
  // For the special case:
  // f[F: Functor]: F[A] => A  and  g[F: Functor, G: Functor]: G[Fix[F]] => Fix[F]
  // comp f g = g andThen unFix andThen f

  val optimizeFast: Expr => Expr = cata(optMul _ andThen((_: Fix[ExprF]).unFix) andThen optAdd)

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
    import RecursionSchemesTalk.&&&
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

  // Hence

  implicit class AlgebraComposition[F[_]: Functor, A](f: F[A] => A) {
    def andThenCata[G[_]: Functor](g: G[A] => F[A]): (Fix[G]) => A = cata(g andThen f)

    import RecursionSchemesTalk.{***, funzip, |||}

    def algProd[B](g: F[B] => B): F[(A, B)] => (A, B) = funzip[F, A, B] _ andThen ***(f, g)
    def algCoprod[G[_]: Functor](g: G[A] => A): Either[F[A], G[A]] => A = |||(f, g)
  }

  abstract class Fixpoint[F[_]: Functor, T] {
    def inF: F[T] => T
    def outF: T => F[T]
    def functor: Functor[F] = Functor[F]
  }
  def inF[F[_], T](implicit fixPoint: Fixpoint[F, T]): (F[T]) => T = fixPoint.inF
  def outF[F[_], T](implicit fixPoint: Fixpoint[F, T]): (T) => F[T] = fixPoint.outF


  object Fixpoint {
    implicit def fixFixPoint[F[_]: Functor]: Fixpoint[F, Fix[F]] = new Fixpoint[F, Fix[F]] {
      val inF: (F[Fix[F]]) => Fix[F] = Fix[F]
      val outF: (Fix[F]) => F[Fix[F]] = (_: Fix[F]).unFix
    }

    implicit def listFixPoint[A]: Fixpoint[({type l[a] = FixPart.ListF[A, a]})#l, List[A]] =
      new Fixpoint[({type l[a] = FixPart.ListF[A, a]})#l, List[A]] {
        def inF: (ListF[A, List[A]]) => List[A] = _.fold(List[A]())(_ :: _)
        def outF: (List[A]) => ListF[A, List[A]] = {
          case Nil => FixPart.NilF
          case x :: xs => FixPart.Cons(x, xs)
        }
      }


    implicit object natFixPoint extends Fixpoint[FixPart.NatF, Int] {
      def inF: (NatF[Int]) => Int = _.fold(0)(_ + 1)

      def outF: (Int) => NatF[Int] = {
        case 0 => rjs.FixPart.Zero
        case i => rjs.FixPart.Succ(i - 1)
      }
    }

    def cata[F[_], T, A](
      alg: F[A] => A
    )(
      implicit
      fixpoint: Fixpoint[F, T]
    ): T => A =
      fixpoint.outF andThen
        (fixpoint.functor.map(_: F[T])(cata(alg))) andThen
        alg

    def ana[F[_], T, A](
      coalg: A => F[A]
    )(
      implicit
      fixpoint: Fixpoint[F, T]
    ): A => T =
      coalg andThen
        fmap[F, A, T](ana(coalg))(fixpoint.functor) andThen
        fixpoint.inF
  }


  def countDownFrom(n: Int) = {
    val f = Fixpoint.cata[FixPart.NatF, Int, List[Int]] {
      _.fold(List[Int]()) {
        case Nil => 1 :: Nil
        case all @ (i :: _) => (i + 1) :: all
      }
    }
    f(n)
  }



}

object Anamorphisms {
  def rememberFoldr[A, B](f: Option[(A, B)] => B): List[A] => B = {
    case Nil => f(None)
    case x :: xs => f(Some(x, rememberFoldr(f)(xs)))
  }

  def unfoldR[A, B](f: B => Option[(A, B)]): B => List[A] = { (b: B) =>
    f(b) match {
      case None => Nil
      case Some((a, _b)) => a :: unfoldR(f)(_b)
    }
  }

  def replicate[A](x: A): Int => List[A] = {
    def c: Int => Option[(A, Int)] = { (n: Int) =>
      if (n == 0) None
      else Some((x, n - 1))
    }
    unfoldR[A, Int](c)
  }
  // scala> rjs.Anamorphisms.replicate(10)("a")
  // res2: List[String] = List(a, a, a, a, a, a, a, a, a, a)


  def splitBy[T](pred: T => Boolean): List[T] => List[List[T]] = {
    def c: List[T] => Option[(List[T], List[T])] = {
      case Nil => None
      case xs =>
        val (chunk, rest) = xs.span(pred andThen(!_))
        Some((chunk, rest.drop(1)))
    }
    unfoldR(c)
  }

  def mergeLists[A: Order]: (List[A], List[A]) => List[A] = {
    val c: ((List[A], List[A])) => Option[(A, (List[A], List[A]))] = {
      case (Nil, Nil) => None
      case (Nil, y :: ys) => Some((y, (Nil, ys)))
      case (x :: xs, Nil) => Some((x, (xs, Nil)))
      case (x :: xs, y :: ys) if Order[A].lteqv(x, y) => Some((x, (xs, y :: ys)))
      case (x :: xs, y :: ys) if Order[A].gt(x, y) => Some((y, (x :: xs, ys)))
    }
    Function.untupled(unfoldR[A, (List[A], List[A])](c))
  }

  // example of co-recursion
  // - produces (potentially infinite) codata
  // - as opposed to recursion which consumes necessarily finite data
  // Using cata or ana only, our program is guaranteed to terminate
  // However not every program can be written in terms of just cata or ana
}

object FixAnamorphims {

  object FixAna {
    import FixPart.Fix
    def ana[F[_]: Functor, A](coalg: A => F[A]): A => Fix[F] =
      coalg andThen (Functor[F].map(_: F[A])(ana(coalg))) andThen Fix[F]
  }
  // There's no enforced distinction between data and codata, so we can use Fix
  // (apparently in Agda and Coq we would be required to make this distinction)
  // However it is often useful to try and enforce this distinction, especially when working with streams
  case class Cofix[F[_]](unCofix: F[Cofix[F]])
  object Cofix {
    def ana[F[_]: Functor, A](coalg: A => F[A]): A => Cofix[F] =
      coalg andThen (Functor[F].map(_: F[A])(ana(coalg))) andThen Cofix[F]

  }


  /*
 F[Cofix[F]] <-- fmap(ana(alg)) --- F[A]
     ^                               ^
     |                               |
     |                               |
  unCofix                          coalg
     |                               |
     |                               |
     |                               |
  Cofix[F] <------ ana coalg ------- A
  */


}

object CoinductiveStreams {
  // Stream with no termination case
  class StreamF[A, R](val a: A, _r: => R) {
    lazy val r = _r
  }
  trait StreamFA[A] { type l[r] = StreamF[A, r] }
  type Stream[A] = Cofix[StreamFA[A]#l]

  object StreamF {
    def apply[A, R](a: A, r: => R): StreamF[A, R] = new StreamF[A, R](a, r)
    def unapply[A, R](streamF: StreamF[A, R]): Option[(A, R)] = Some((streamF.a, streamF.r))
    implicit def functor[A]: Functor[StreamFA[A]#l] =
      new Functor[StreamFA[A]#l] {
        def map[R, B](fa: StreamF[A, R])(f: (R) => B): StreamF[A, B] = StreamF(fa.a, f(fa.r))
      }
  }

  object Stream {
    import FixAnamorphims.Cofix.ana
    def consS[A, R](x: A, xs: Cofix[StreamFA[A]#l]): Stream[A] = Cofix[StreamFA[A]#l](StreamF(x, xs))
    def headS[A](s: Stream[A]): A = s.unCofix.a
    def tailS[A](s: Stream[A]): Cofix[StreamFA[A]#l] = s.unCofix.r
    def iterateS[A](f: A => A): A => Stream[A] = {
      def c(x: A): StreamF[A, A] = StreamF(x, f(x))
      ana[StreamFA[A]#l, A](c)
    }

    def takeS[A](n: Int): Stream[A] => List[A] = n match {
      case 0 => (_: Stream[A]) => Nil
      case _n => { (stream: Stream[A]) =>
        val StreamF(x, xs) = stream.unCofix
        x :: takeS(_n - 1)(xs)
      }
    }
  }
}

object Hylomorphism {
  // Composition of cata and ana
  // Models general recursion
  // Allows us to substitute any recursive control structure with a data structure
  // A representation that allows us to easily exploit parallelism

  import FixAnamorphims.FixAna.ana
  import FixPart.cata
  import FixPart.Fix
  import FixPart.unFix
  def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
    ana(g) andThen cata(f)

  // Hylomorphisms are turing complete. So we've lost any termination guarantees

  // To see the explicit recursion, cata and ana can be fused together via substitution
  // and fmap-fusion Functor law:

  // fmap p . fmap q = fmap ( p . q ) OR (_: F[A]).map(p).map(q) = (_: F[A]).map(p andThen q)

  // Giving:

  abstract class Result1[F[_]: Functor, A, B] {
    // hylo f g = f . fmap (hylow f g) . g
    def f: F[B] => B
    def g: A => F[A]
    def lhs = hylo(f)(g)
    def rhs = f compose fmap[F, A, B](hylo(f)(g)) compose g
    def rhs1 = f compose fmap[F, A, B](cata(f) compose ana(g)) compose g
  }
  // This transformation is the basis for deforestation,
  // eliminating the intermediate data structures

  object InTermsOfHylo {
    def cata[F[_]: Functor, A](alg: F[A] => A): Fix[F] => A = hylo(alg)(unFix[F])
    def ana[F[_]: Functor, A](coalg: A => F[A]): (A) => Fix[F] = hylo(Fix[F])(coalg)
  }

  object MergeSort {
    sealed trait LTreeF[A, R]
    trait LTreeFA[A] {
      type l[r] = LTreeF[A, r]
    }
    case class Leaf[A, R](a: A) extends LTreeF[A, R]
    case class Bin[A, R](r: R, l: R) extends LTreeF[A, R]

    object LTreeF {
      implicit def functor[A]: Functor[LTreeFA[A]#l] = new Functor[LTreeFA[A]#l] {
        def map[R, B](fa: LTreeF[A, R])(f: (R) => B): LTreeF[A, B] = fa match {
          case Leaf(a) => Leaf(a)
          case Bin(l, r) => Bin(f(l), f(r))
        }
      }
    }

    import rjs.Anamorphisms.mergeLists
    def merge[A: Order]: LTreeF[A, List[A]] => List[A] = {
      case Leaf(x) => List(x)
      case Bin(xs, ys) =>
        val merge = mergeLists[A]
        merge(xs, ys)
    }

    object half {
      def unapply[A](as: List[A]): Option[(List[A], List[A])] = Some(as.splitAt(as.length / 2))
    }

    def unflatten[A]: List[A] => LTreeF[A, List[A]] = {
      case x :: Nil => Leaf[A, List[A]](x)
      case half(xs, ys) => Bin[A, List[A]](xs, ys)
    }

    def mSort[A: Order]: List[A] => List[A] =
      hylo[LTreeFA[A]#l, List[A], List[A]](merge[A])(unflatten[A])

    def run() = {
      implicit val intOrder = algebra.std.int.intAlgebra
      val sort = mSort[Int]

      val explode = ana[LTreeFA[Int]#l, List[Int]](unflatten[Int])

      println(explode(List(7, 6, 3, 1, 5, 4)))
      println(sort(List(7, 6, 3, 1, 5, 4)))
    }
  }


}

object fmap {
  def apply[F[_]: Functor, A, B](f: => A => B): F[A] => F[B] = {
    Functor[F].map(_)(f)
  }
}

object Paramorphism {
  // extension of hte concept of catamorphism
  // - models primitive recursion over an inductive type
  // - a convenient way of getting access to the original input structures
  // - very useful in practice !

  import ExprExample.Fixpoint
  import RecursionSchemesTalk.&&&

  def para[F[_], A, T](alg: F[(A, T)] => A)(implicit ev: Fixpoint[F, T]): T => A = {
    implicit val functorF = ev.functor
    alg compose fmap[F, T, (A, T)](&&&(para(alg), identity)) compose ev.outF
  }

  object Examples {
    import rjs.FixPart.{ NatF, Succ, Zero }
    import rjs.FixPart.{ ListF, Cons, NilF, ListFA }
    import rjs.FixPart.Fix
    import rjs.RecursionSchemesTalk.funzip
    def fact: Int => Int = {
      val alg: NatF[(Int, Int)] => Int = {
        case Zero => 1                   // 0! = 1
        case Succ((f, n)) => f * (n + 1) // (n + 1)! = n! * (n + 1)
      }
     para(alg)
    }

    implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
      def empty: Vector[A] = Vector()

      def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
    }

    def factWithFold(n: Int) = rjs.foldr.foldr0[Int, Int](i => j => i * j)(1)((1 to n).toList)
    // Not as desirable because of the unfold followed by the fold

    def sliding[A](n: Int): List[A] => List[List[A]] = {
      val alg: ListF[A, (List[List[A]], List[A])] => List[List[A]] = {
        case NilF => List[List[A]]()
        case Cons(x, (r, xs)) => (x :: xs).take(n) :: r
      }
      para[ListFA[A]#l, List[List[A]], List[A]](alg)
    }

    def cataTrace[F[_], A](
      alg: F[A] => A
    )(
      implicit
      functor: Functor[F],
      foldable: Foldable[F]
    ): Fix[F] => Vector[(Fix[F], A)] = {
      def phi: F[(Vector[(Fix[F], A)], Fix[F])] => Vector[(Fix[F], A)] = { in =>
        val (fm, ft) = funzip(in)
        val k: Fix[F] = Fix[F](ft)
        val m_ : Vector[(Fix[F], A)] = Foldable[F].fold(fm)
        val v = alg(fmap[F, Fix[F], A]({_k => m_.find(_._1 == _k).get._2})(functor)(ft))
        m_ :+ (k -> v)
      }
      para(phi)
    }
  }
}

object CompositionalDataTypes {
  sealed trait :+:[F[_], G[_], R] // Coproduct of pattern functors F and G
  case class Inl[F[_], G[_], R](fr: F[R]) extends :+:[F, G, R]
  case class Inr[F[_], G[_], R](gr: G[R]) extends :+:[F, G, R]

  case class :*:[F[_], G[_], R](fr: F[R], gr: G[R]) // Product of pattern functors F and G

  sealed trait FreeF[F[_], A, R] // The free monad pattern functor
  case class Suspend[F[_], A, R](fr: F[R]) extends FreeF[F, A, R]
  case class Pure[F[_], A, R](a: A) extends FreeF[F, A, R]

  case class CofreeF[F[_], A, R](fr: F[R], a: A) // The cofree monad pattern functor
}



object UnfixedJson {
  import FixPart.Fix
  sealed trait JsValueF[R]
  trait JsNull[R] extends JsValueF[R]
  def JSNull[R]: JsValueF[R] = new JsNull[R] {}
  case class JSBool[R](asBoolean: Boolean) extends JsValueF[R]
  case class JSNumber[R](asDouble: Double) extends JsValueF[R]
  case class JSString[R](asSting: String) extends JsValueF[R]
  case class JSArray[R](asVector: Vector[R]) extends JsValueF[R]
  case class JSObject[R](asAssocList: Vector[(String, R)]) extends JsValueF[R]

  type JSValue = Fix[JsValueF]

  implicit object functor extends Functor[JsValueF] {
    def map[A, B](fa: JsValueF[A])(f: (A) => B): JsValueF[B] = fa match {
      case JSBool(asBoolean) => JSBool(asBoolean)
      case JSNumber(asDouble) => JSNumber(asDouble)
      case JSString(asSting) => JSString(asSting)
      case JSArray(asVector) => JSArray(asVector.map(f))
      case JSObject(asAssocList) => JSObject(asAssocList.map { case (k, v) => (k, f(v)) } )
      case _: JsNull[A] => JSNull
    }
  }


  import fastparse.all._
  def pars(p: => Parser[JSValue]) = rjs.json.pJSValueF(p).map(Fix[JsValueF])
  val pJSValue: Parser[JSValue] = FixPart.fix[Parser[JSValue]](pars)


}

object TemplatingExample {
  import FixPart.Fix
  import FixPart.cata
  sealed trait CtxF[F[_], A, R] // The free monad pattern functor
  case class Term[F[_], A, R](fr: F[R]) extends CtxF[F, A, R]
  case class Hole[F[_], A, R](a: A) extends CtxF[F, A, R]
  trait CtxFA[F[_], A] {
    type l[r] = CtxF[F, A, r]
  }

  object CtxF {
    implicit def functor[F[_]: Functor, A]: Functor[CtxFA[F, A]#l] = new Functor[CtxFA[F, A]#l] {
      def map[R, B](fa: CtxF[F, A, R])(f: (R) => B): CtxF[F, A, B] = fa match {
        case Term(fr) => Term[F, A, B](Functor[F].map(fr)(f))
        case Hole(a) => Hole[F, A, B](a)
      }
    }
  }

  type Ctx[F[_], A] = Fix[CtxFA[F, A]#l]

  def fillHoles[F[_]: Functor, A](g: A => Fix[F]): Ctx[F, A] => Fix[F] = {
    val alg: CtxF[F, A, Fix[F]] => Fix[F] = {
      case Term(t) => Fix[F](t)
      case Hole(a) => g(a)
    }
    cata[CtxFA[F, A]#l, Fix[F]](alg)
  }

  type Name = String
  type JSTemplate = Ctx[JsValueF, Name]
  import fastparse.all._
  def pVar: Parser[Name] = {
    import fastparse.all._
    "${" ~ CharIn('a' to 'z').rep.! ~ "}"
  }

  def pJSTemplate: Parser[Ctx[JsValueF, Name]] = {
    def f(p: => Parser[Ctx[JsValueF, Name]]) = {
      val hole = pVar.map(Hole[JsValueF, Name, Ctx[JsValueF, Name]])
      val term = pJSValueF(p).map(Term[JsValueF, Name, Ctx[JsValueF, Name]])
      val holeOrTerm = hole | term
      holeOrTerm.map(Fix[CtxFA[JsValueF, Name]#l](_))
    }
    FixPart.fix(f)
  }

  def parseUnsafe[R](parser: Parser[R]) = parser.parse(_: String).get.value
  val temp1 = parseUnsafe(pJSTemplate)("[{\"foo\":${a}}]")

  import UnfixedJson._
  def vlookup[A](env: Map[A, JSValue]): A => JSValue = {
    (env.get(_: A)) andThen ((_: Option[JSValue]).getOrElse(Fix(UnfixedJson.JSNull): JSValue))
  }

  val temp2 = {
    val f = fillHoles(vlookup(Map("a" -> Fix(JSNumber(42)))))
    f(temp1)
  }
}
