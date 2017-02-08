package rjs.recursion

import cats.{Monoid, Functor}
import rjs.recursion.data.{NatF, Zero, Succ, ListF, NilF, ConsF, ListFA, MyList, Foldable, Fix}
import rjs.recursion.utils.{funzip, fmap}
import rjs.recursion.data.Fixpoint.para
import rjs.recursion.Part1CataForLists.foldr0

object Part5Paramorphisms {
  // extension of hte concept of catamorphism
  // - models primitive recursion over an inductive type
  // - a convenient way of getting access to the original input structures
  // - very useful in practice !

  def fact: Int => Int = {
    val alg: NatF[(Int, Int)] => Int = {
      case Zero => 1                   // 0! = 1
      case Succ((f, n)) => f * (n + 1) // (n + 1)! = n! * (n + 1) // Previous result and and it's input
    }
    para(alg)
  }

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    def empty: Vector[A] = Vector()

    def combine(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }

  def factWithFold(n: Int) = foldr0[Int, Int](i => j => i * j)(1)((1 to n).toList)
  // Not as desirable because of the unfold followed by the fold

  def sliding[A](n: Int): List[A] => List[List[A]] = {
    val alg: ListF[A, (List[List[A]], List[A])] => List[List[A]] = {
      case NilF => List[List[A]]()
      case ConsF(x, (r, xs)) => (x :: xs).take(n) :: r
    }
    para[ListFA[A]#l, List[List[A]], List[A]](alg)
  }

  val tails = schemes.para[ListFA[Int]#l, MyList.T[MyList.T[Int]]] {
    case NilF => MyList.nil
    case ConsF(h, (acc, t)) => MyList.cons(MyList.cons(h, t), acc)
  }
  def runTails = MyList.toList(tails(MyList(1,2,3))).map(MyList.toList)

  def cataTrace[F[_], A](
    alg: F[A] => A
  )(
    implicit
    functor: Functor[F],
    foldable: Foldable[F]
  ): Fix[F] => Vector[(Fix[F], A)] = {
    def phi: F[(Vector[(Fix[F], A)], Fix[F])] => Vector[(Fix[F], A)] = { in =>
      val (traceF, inputF) = funzip(in)
      val input: Fix[F] = Fix[F](inputF)
      val traceSoFar : Vector[(Fix[F], A)] = Foldable[F].fold(traceF)
      val output = alg(fmap[F, Fix[F], A]({_k => traceSoFar.find(_._1 == _k).get._2})(functor)(inputF))
      traceSoFar :+ (input -> output)
    }
    schemes.para(phi)
  }

}
