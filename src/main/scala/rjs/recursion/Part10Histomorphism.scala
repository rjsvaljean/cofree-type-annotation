package rjs.recursion

import rjs.recursion.data.Fixpoint.histo
import rjs.recursion.data._

import scala.annotation.tailrec

object Part10Histomorphism {

  def fib: Int => Int = {
    val unAnnNat = Ann.unAnn[NatF, Int]
    def f: NatF[Ann.T[NatF, Int]] => Int = {
      case Zero => 0
      case Succ(i) => unAnnNat(i) match {
        case (Zero, _) => 1
        case (Succ(_i), m) => unAnnNat(_i) match {
          case (_, n) => n + m
        }
      }
    }
    histo(f)
  }

  def evens[A]: List[A] => List[A] = {
    def alg: ListF[A, Ann.T[ListFA[A]#l, List[A]]] => List[A] = {
      case NilF => List[A]()
      case ConsF(_, i) => Ann.strip[ListFA[A]#l, List[A]](i) match {
        case NilF => List[A]()
        case ConsF(x, y) => x :: Ann.attr[ListFA[A]#l, List[A]](y)
      }
    }
    histo[ListFA[A]#l, List[A], List[A]](alg)
  }

  object ChChChChanges {
    // re-implementation of http://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence
    type Cent = Int
    type ChangePermutationsCount = Int
    val coins: List[Int] = List(50, 25, 10, 5, 1)

    val alg: (NatF[Ann.T[NatF, ChangePermutationsCount]]) ⇒ ChangePermutationsCount = {
      case Zero ⇒ 1
      case curr@Succ(ann) ⇒
        val given = NatF.compress(curr)
        val validCoins = coins.filter(_ <= given)
        val remaining = validCoins.map(given - _)
        val (zeroes, nonZeroes) = remaining.partition(_ == 0)
        val zeroCount = zeroes.length
        println("-" * 40 + " " + given)
        val others = nonZeroes.map(i ⇒ {
          val out = lookup(ann)(i)
          println(s"ann: $ann")
          println(s"looking up $i: $out")
          out
        }).reduceOption(_ * _).getOrElse(0)
        println(s"remaining: $remaining")
        println(s"zeroCount: $zeroCount + others: $others")
        println("-" * 40)
        zeroCount + others
    }

    val changesAlg: (NatF[Ann.T[NatF, Set[Vector[Cent]]]]) ⇒ Set[Vector[Cent]] = {
      case Zero ⇒ Set()
      case curr@Succ(ann) ⇒
        val given = NatF.compress(curr)
        val validCoins = coins.filter(_ <= given)
        val remaining = validCoins.map(c ⇒ (c, given - c))
        val (zeroes, nonZeroes) = remaining.partition(_._2 == 0)
        val singletonChange = zeroes.map(_ ⇒ Vector(given)).toSet
        val others = nonZeroes.toSet[(Int, Int)].flatMap { case (subtracted, i) ⇒
          lookup(ann)(i).map(_ :+ subtracted).map(_.sorted)
        }
        singletonChange ++ others
    }

    def lookup[A](cache: Ann.T[NatF, A])(n: Int): A = {
      @tailrec def go(window: Vector[A], remaining: Ann.T[NatF, A]): Vector[A] = {
        val unfixed = remaining.unFix
        if (unfixed.fr == Zero) window
        else {
          val tail = unfixed.fr.asInstanceOf[Succ[Fix[AnnFA[NatF, A]#l]]].r
          if (window.length < n) go(unfixed.a +: window, tail)
          else if (window.length == n && window.nonEmpty) go(unfixed.a +: window.dropRight(1), tail)
          else go(window, tail)
        }
      }
      go(Vector(), cache).last
    }

    def change(amount: Cent): ChangePermutationsCount = {
      val x = rjs.recursion.schemes.histo[NatF, ChangePermutationsCount](alg)
      x(NatF.expand(amount))
    }

    def changes(amount: Cent): Set[Vector[Cent]] = {
      val x = rjs.recursion.schemes.histo[NatF, Set[Vector[Cent]]](changesAlg)
      x(NatF.expand(amount))
    }
  }
}
