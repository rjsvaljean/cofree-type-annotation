package rjs.recursion

import utils.{***, fmap}
import cats.std.option.optionInstance

object Part1CataForLists {
  //`foldr` on list is a specialised catamorphism

  def foldr0[A, B](f: A => B => B)(b: B)(as: List[A]): B = as match {
    case Nil => b
    case a :: _as => foldr0(f)(f(a)(b))(_as)
  }

  // We can express the parameters used above in terms of a single
  // F-algebra `F[B] => B` over a functor `F` and carrier `B`

  // First convert ((A => B => B), B) -> Option[(A, B)] => B

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
    Option[(A, List[A])] ---- mapIdAndFoldAlg ---> Option[(A, B)]
             ^                                           |
             |                                           |
             |                                           |
          unList                                        alg
             |                                           |
             |                                           |
             |                                           v
           List[A] ----------- foldr2(alg) ------------> B
  */

  // Examples:
  // NOTE: No explicit recursion
  def length[A]: (List[A]) => Int = {
    val alg: Option[(A, Int)] => Int = {
      case None => 0
      case Some((_, len)) => len + 1
    }

    foldr2(alg)
  }

  // You can alter the carrier to alter the order of traversal
  def foldl[A, B](f: B => A => B): (List[A]) => (B) => B = {
    val alg: Option[(A, B => B)] => (B => B) = {
      case None => identity[B]
      case Some((a, bToB)) => (b: B) => bToB(f(b)(a))
    }
    foldr2(alg)
  }

  def test = (
    length[Unit](List.fill(10)(())),
    foldl[Int, String] {(str: String) => (i: Int) => str + i}(List(1,2,3))(""),
    foldr2[Int, String]({case None => ""; case Some((i, str)) => str + i})(List(1,2,3))
  )
}
