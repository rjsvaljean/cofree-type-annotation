package rjs

object BLEBW {
  // (| b, plus |)
  trait Catamorphism[A, B] {
    def b: B
    def plus: (A, B) => B
    // then a list catamorphism
    def h : List[A] => B = {
      case Nil => b
      case a :: as => plus(a, h(as))
    }
  }

  object Catamorphism {
    def Length[A] = new Catamorphism[A, Int] {
      def b: Int = 0

      def plus: (A, Int) => Int = { case (_, i) => i + 1 }
    }

    def Map[A, B](f: A => B) = new Catamorphism[A, List[B]] {
      def b: List[B] = Nil

      def plus: (A, List[B]) => List[B] = {
        case (a, bs) => f(a) :: bs
      }
    }

    object CatamorphismPartOfFactorial extends Catamorphism[Int, Int] {
      def b: Int = 1

      def plus: (Int, Int) => Int = _ * _
    }
  }

  // if f ( b ) = c
  // && f ( a plus as ) == a multiply ( f as )
  // then
  // f compose (| b, plus |) = (| c, multiply |)
  trait FusionLaw[C, B, Acc] {

    def h = new Catamorphism[Acc, B] {
      def b: B = ???

      def plus: (Acc, B) => B = ???
    }

    def g = new Catamorphism[Acc, C] {
      def b: C = ???

      def plus: (Acc, C) => C = ???
    }

    def f: B => C

    def law = if (f(h.b) == g.b && ({(a: Acc, as: B) => f(h.plus(a, as)) == g.plus(a, f(as))})(null.asInstanceOf[Acc], null.asInstanceOf[B])) {
      (f compose h.h) == g.h
    }
  }

  // [( g , p )]
  trait Anamorphism[A, B] {
    def p: A => Boolean
    def g: A => (B, A)
    def h: A => List[B] = { b =>
      if (p(b)) Nil
      else {
        val (a, _b) = g(b)
        a :: h(_b)
      }
    }
  }

  object Anamorphism {
    def Zip[A, B] = new Anamorphism[(List[A], List[B]), (A, B)] {
      def p: ((List[A], List[B])) => Boolean = {
        case (Nil, _) | (_, Nil) => true
        case _ => false
      }

      def g: ((List[A], List[B])) => ((A, B), (List[A], List[B])) = {
        case (a :: as, b :: bs) => ((a, b), (as, bs))
        case _ => ???
      }
    }

    def Iterate[A](f: A => A, _p: A => Boolean) = new Anamorphism[A, A] {
      def p: (A) => Boolean = _p

      def g: (A) => (A, A) = {a => (a, f(a))}
    }

    def Map[A, B](f: A => B) = new Anamorphism[List[A], B] {
      def p: (List[A]) => Boolean = _.isEmpty

      def g: (List[A]) => (B, List[A]) = {
        case a :: as => (f(a), as)
        case _ => ???
      }
    }

    object AnamorphismPartOfFactorial extends Anamorphism[Int, Int] {
      def p: (Int) => Boolean = _ == 0

      def g: (Int) => (Int, Int) = {i => (i, i -1)}
    }
  }

  // [[ ( c , plus ) , (g , p) ]]
  trait Hylomorphism[A, B, C] {
    def c: C              // From RequirementForConvertingAnaToHylomorphism#zero (C == F[A])
    def plus: (B, C) => C // From RequirementForConvertingAnaToHylomorphism#cons
    def g: A => (B, A)    // From Anamorphism
    def p: A => Boolean   // From Anamorphism

    def h(a: A): C =
      if (p(a)) c
      else {
        val (b, _a) = g(a)
        plus(b, h(_a))
      }
  }

  object Hylomorphism {
    object Factorial extends Hylomorphism[Int, Int, Int] {
      def c: Int = 1

      def plus: (Int, Int) => Int = _ * _

      def g: (Int) => (Int, Int) = {i => (i, i - 1)}

      def p: (Int) => Boolean = _ == 0
    }
  }

  // This is exactly the same structure as an anamorphism except that Nil has been replaced by c and Cons by plus

  trait RequirementForConvertingAnaToHylomorphism[F[_]] {
    type A
    def zero: F[A]
    def cons: (A, F[A]) => F[A]
  }

  object RequirementForConvertingAnaToHylomorphism {
    type Aux[F[_], _A] = RequirementForConvertingAnaToHylomorphism[F] { type A = _A }

    implicit def ListFreeMonoid[Fix]: RequirementForConvertingAnaToHylomorphism.Aux[List, Fix] =
      new RequirementForConvertingAnaToHylomorphism[List] {
        type A = Fix

        def zero: List[Fix] = Nil
        def cons: (Fix, List[Fix]) => List[Fix] = _ :: _
      }
  }

  abstract class AbstractedOverListAnamorphism[F[_], A, B](implicit req: RequirementForConvertingAnaToHylomorphism.Aux[F, B]) {
    def p: A => Boolean
    def g: A => (B, A)
    def h: A => F[B] = { b =>
      if (p(b)) req.zero
      else {
        val (a, _b) = g(b)
        req.cons(a, h(_b))
      }
    }
  }

  object AbstractedOverListAnamorphism {
    def Map[A, B](f: A => B) = new AbstractedOverListAnamorphism[List, List[A], B] {
      def p: (List[A]) => Boolean = _.isEmpty

      def g: (List[A]) => (B, List[A]) = {
        case a :: as => (f(a), as)
        case _ => ???
      }
    }
  }

  trait Paramorphism[A, B] {
    def b: B
    def plus(x: A, y: B): B
    def fold(a: A)(b: B)(f: (A, A) => B): B
    def h(i: A): B = fold(i)(b)((context, _i) => plus(context, h(_i)))
  }

  trait NumParamorphism[B] extends Paramorphism[Num, B] {
    def fold(a: Num)(b: B)(f: (Num, Num) => B): B = a match { case Zero => b ; case Succ(_a) => f(_a, _a)}
  }

  trait ListParamorphism[A, B] extends Paramorphism[List[A], B] {
    def fold(a: List[A])(b: B)(f: (List[A], List[A]) => B): B = a match { case Nil => b ; case as @ _ :: tail => f(as, tail)}
  }

  object Paramorphism {
    object Factorial extends NumParamorphism[Num] {
      def b: Num = Num(1)

      def plus(x: Num, y: Num): Num = Succ(x) times y
    }

    def Tails[A] = new ListParamorphism[A, List[List[A]]] {
      def b: List[List[A]] = List[A]() :: Nil

      def plus(cons: List[A], tails: List[List[A]]): List[List[A]] =
        cons :: tails // Fine because cons is never empty
    }
  }

  trait BiFunctor[F[_, _]] {
    def biMap[A, B, C, D](f: A => B, g: C => D): F[A, C] => F[B, D]
  }

  class BiFunctorLaws[F[_, _]] {
    def id[A, B](
      example: F[A, B]
    )(
      implicit bif: BiFunctor[F]
    ) = {
      val lhs = bif.biMap(identity[A], identity[B])
      val rhs = identity[F[A, B]]
      lhs(example) == rhs(example)
    }

    def composition[A, B, C, D, X, Y](
      f: A => B,
      g: C => D,
      h: X => A,
      j: Y => C
    )(
      example: F[X, Y]
    )(
      implicit functor: BiFunctor[F]
    ) = {
      val lhs = functor.biMap(f, g).compose(functor.biMap(h, j))
      val rhs = functor.biMap(f compose h, g compose j)
      lhs(example) == rhs(example)
    }
  }
}

object Zero extends Num
case class Succ(n: Num) extends Num
object Num {
  def apply(i: Int) = fromInt(i)

  def fromInt(i: Int): Num =
    if (i == 0) Zero
    else Succ(fromInt(i - 1))

  def toInt(n: Num): Int = n match {
    case Zero => 0
    case Succ(nMinus1) => 1 + toInt(nMinus1)
  }
}

sealed trait Num {
  override def toString = s"Num: ${Num.toInt(this)}"

  def plus(n2: Num): Num = this match {
    case Zero => n2
    case Succ(nMinus1) => nMinus1.plus(Succ(n2))
  }

  def times(n: Num): Num = n match {
    case Zero => Zero
    case Succ(Zero) => this
    case Succ(nMinus1) => this plus times(nMinus1)
  }

}
