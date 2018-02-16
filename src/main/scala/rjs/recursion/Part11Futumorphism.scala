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
  import cats.data.State

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

  sealed trait Action extends Product with Serializable
  case object Flower extends Action
  case object Upwards extends Action
  case object Branch extends Action

  case class SimpleRNG(seed: Long) extends AnyVal {
    def nextInt: (SimpleRNG, Int) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (nextRNG, n)
    }
  }

  type RNG[A] = State[SimpleRNG, A]
  val rng: RNG[Int] = State((_: SimpleRNG).nextInt)
  val oneToFive: RNG[Int] = rng.map(i ⇒ math.abs(i % 5))
  val grow: State[Seed, Action] = rng.map {
    case 0 ⇒ Flower
    case 1 ⇒ Branch
    case _ ⇒ Upwards
  }.transformS((_: Seed).rng, (seed: Seed, rng) ⇒ seed.copy(rng = rng))
  case class Seed(height: Int, rng: SimpleRNG)

  type CVCoalgebra[F[_], A] = A ⇒ F[Ctx.T[F, A]]
  val sow: CVCoalgebra[Plant, Seed] = { case (seed @ Seed(height, _)) ⇒
    val (next, action) = grow.run(seed).value
    (action, height) match {
      case (_, 0) ⇒ Root(Ctx.term[Plant, Seed](next))
      case (_, 10) ⇒ Bloom
      case (Flower, _) ⇒ Bloom
      case (Upwards, _) ⇒ Stalk(Ctx.term[Plant, Seed](next))
      case (Branch, _) ⇒ Fork(
        Ctx.hole(Stalk(Ctx.term[Plant, Seed](next))),
        Ctx.hole(Bloom),
        Ctx.hole(Stalk(Ctx.term[Plant, Seed](next)))
      )
    }
  }
}
