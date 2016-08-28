package rjs.recursion

import cats.{Functor, Order}
import rjs.recursion.data.{Fix, TreeF, unFix, LeafF, NodeF, TreeFA}
import rjs.recursion.schemes.{ana, cata, hylo}
import rjs.recursion.utils.fmap

object Part4Hylomorphisms {

  // def hylo[F[_]: Functor, A, B](f: F[B] => B)(g: A => F[A]): A => B =
  //   ana(g) andThen cata(f)

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

  object MergeSortExample {

    import Part3Anamorphisms.mergeLists
    def merge[A: Order]: TreeF[A, List[A]] => List[A] = {
      case LeafF(x) => List(x)
      case NodeF(xs, ys) =>
        val merge = mergeLists[A]
        merge(xs, ys)
    }

    object half {
      def unapply[A](as: List[A]): Option[(List[A], List[A])] = Some(as.splitAt(as.length / 2))
    }

    def unflatten[A]: List[A] => TreeF[A, List[A]] = {
      case x :: Nil => LeafF[A, List[A]](x)
      case half(xs, ys) => NodeF[A, List[A]](xs, ys)
    }

    def mSort[A: Order]: List[A] => List[A] =
      hylo[TreeFA[A]#l, List[A], List[A]](merge[A])(unflatten[A])

    def run() = {
      implicit val intOrder = algebra.std.int.intAlgebra
      val sort = mSort[Int]

      val explode = ana[TreeFA[Int]#l, List[Int]](unflatten[Int])

      println(explode(List(7, 6, 3, 1, 5, 4)))
      //Fix(NodeF(
      //  Fix(NodeF(
      //    Fix(LeafF(7)),
      //    Fix(NodeF(
      //      Fix(LeafF(6)),
      //      Fix(LeafF(3))
      //    ))
      //  )),
      //  Fix(NodeF(
      //    Fix(LeafF(1)),
      //    Fix(NodeF(
      //      Fix(LeafF(5)),
      //      Fix(LeafF(4))
      //   ))))))
      println(sort(List(7, 6, 3, 1, 5, 4)))
    }
  }



}
