package rjs.recursion

import rjs.recursion.data.Fixpoint.histo
import rjs.recursion.data._

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
}
