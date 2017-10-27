package rjs.recursion


import cats.{Functor, Monoid}
import rjs.recursion.data.Ctx.{hole, term}
import rjs.recursion.data._
import rjs.recursion.schemes.futu

object Part11Futumorphism {

  def exch[A]: Stream.T[A] => Stream.T[A] = {
    def coa(xs: Stream.T[A]): StreamF[A, Ctx.T[StreamFA[A]#l, Stream.T[A]]] = StreamF.apply(
      Stream.headS(Stream.tailS(xs)),
      term[StreamFA[A]#l, Stream.T[A]](StreamF.apply(
        Stream.headS(xs),
        hole[StreamFA[A]#l, Stream.T[A]](Stream.tailS(Stream.tailS(xs)))
      ))
    )
    futu[StreamFA[A]#l, Stream.T[A]](coa)
  }

  def run = (
    Stream.takeS(10)(rjs.recursion.Part3Anamorphisms.s1),
    Stream.takeS(10)(exch(rjs.recursion.Part3Anamorphisms.s1))
  )
}

object Horticulture {
  sealed trait Plant[+A]
  case class Root[A](a: A) extends Plant[A]
  case class Stalk[A](a: A) extends Plant[A]
  case class Fork[A](r: A, c: A, l: A) extends Plant[A]
  case object Bloom extends Plant[Nothing]

  object Plant {
    implicit val functor: Functor[Plant] = new Functor[Plant] {
      def map[A, B](fa: Plant[A])(f: (A) ⇒ B) = fa match {
        case Root(a) => Root(f(a))
        case Stalk(a) => Stalk(f(a))
        case Fork(r, c, l) => Fork(f(r), f(c), f(l))
        case Bloom => Bloom
      }
    }
    implicit val foldable: Foldable[Plant] = new Foldable[Plant] {
      def foldMap[M: Monoid, A](f: (A) ⇒ M)(ta: Plant[A]): M = ta match {
        case Root(a) => f(a)
        case Stalk(a) => f(a)
        case Fork(r, c, l) => Monoid[M].combineAll(Seq(f(r), f(c), f(l)))
        case Bloom => Monoid[M].empty
      }
    }
  }

  sealed trait Action
  case object Flower extends Action
  case object Upwards extends Action
  case object Branch extends Action

  case class StdGen()

  case class Seed(height: Int, rng: StdGen)
}
