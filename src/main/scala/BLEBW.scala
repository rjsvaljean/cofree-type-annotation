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

  trait Anamorphism[A, B] {
    def p: B => Boolean
    def g: B => (A, B)
    def h: B => List[A] = { b =>
      if (p(b)) Nil
      else {
        val (a, _b) = g(b)
        a :: h(_b)
      }
    }
  }

  object Anamorphism {
    def Zip[A, B] = new Anamorphism[(A, B), (List[A], List[B])] {
      def p: ((List[A], List[B])) => Boolean = {
        case (Nil, _) | (_, Nil) => true
        case _ => false
      }

      def g: ((List[A], List[B])) => ((A, B), (List[A], List[B])) = {
        case (a :: as, b :: bs) => ((a, b), (as, bs))
        case _ => sys.error("Invalid")
      }
    }
  }
}
