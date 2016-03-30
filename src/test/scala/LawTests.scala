package rjs

import algebra.Eq
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.{CartesianTests, ComonadTests, MonadTests}
import cats.std.AllInstances
import cats.syntax.AllSyntax
import org.scalacheck.{Arbitrary, Prop}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline
import IntMonoid._

class LawTests
  extends FunSuite
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Discipline
    with AllInstances
    with AllSyntax
    with WriterInstances
    with ReaderInstances
    with CowriterInstances
    with CoreaderInstances {

  checkAll("Writer Monad", MonadTests[({type l[a] = Writer[Int, a]})#l].monad[Int, Int, Int])
  checkAll("Reader Monad", MonadTests[({type l[a] = Reader[Int, a]})#l].monad[Int, Int, Int])
  checkAll("Coreader Comonad", ComonadTests[({type l[a] = Cowriter[Int, a]})#l].comonad[Int, Int, Int])
  checkAll("Cowriter Comonad", ComonadTests[({type l[a] = Coreader[Int, a]})#l].comonad[Int, Int, Int])
}

trait WriterInstances {

  implicit val arbitraryWriter: Arbitrary[Writer[Int, Int]] = Arbitrary(for {
    i <- Arbitrary.arbitrary[Int]
  } yield Writer[Int, Int](i, i))

  implicit val arbitraryWriterFn: Arbitrary[Writer[Int, Int => Int]] = Arbitrary(for {
    i <- Arbitrary.arbitrary[Int]
  } yield Writer[Int, Int => Int](_ => i, i))

  implicit val writerEq1: Eq[Writer[Int, Int]] = new Eq[Writer[Int, Int]] {
    def eqv(x: Writer[Int, Int], y: Writer[Int, Int]): Boolean = x == y
  }

  implicit val writerEq2: Eq[Writer[Int, (Int, Int, Int)]] = new Eq[Writer[Int, (Int, Int, Int)]] {
    def eqv(x: Writer[Int, (Int, Int, Int)], y: Writer[Int, (Int, Int, Int)]): Boolean = x == y
  }

  implicit val writerIsomorphism: Isomorphisms[({type l[a] = Writer[Int, a]})#l] =
    CartesianTests.Isomorphisms.invariant[({type l[a] = Writer[Int, a]})#l]

}

trait CoreaderInstances {

  implicit val arbitraryCoreader: Arbitrary[Coreader[Int, Int]] = Arbitrary(for {
    i <- Arbitrary.arbitrary[Int]
  } yield Coreader[Int, Int](i, i))

  implicit val coreaderEq1: Eq[Coreader[Int, Int]] = new Eq[Coreader[Int, Int]] {
    def eqv(x: Coreader[Int, Int], y: Coreader[Int, Int]): Boolean = x == y
  }

  implicit val coreaderEq2: Eq[Coreader[Int, Coreader[Int, Int]]] = new Eq[Coreader[Int, Coreader[Int, Int]]] {
    def eqv(x: Coreader[Int, Coreader[Int, Int]], y: Coreader[Int, Coreader[Int, Int]]): Boolean = x == y
  }

  implicit val coreaderEq3: Eq[Coreader[Int, Coreader[Int, Coreader[Int, Int]]]] = new Eq[Coreader[Int, Coreader[Int, Coreader[Int, Int]]]] {
    def eqv(x: Coreader[Int, Coreader[Int, Coreader[Int, Int]]], y: Coreader[Int, Coreader[Int, Coreader[Int, Int]]]): Boolean = x == y
  }
}

trait ReaderInstances {

  implicit val arbitraryReader: Arbitrary[Reader[Int, Int]] = Arbitrary(for {
    i <- Arbitrary.arbitrary[Int]
  } yield Reader[Int, Int](_ => i))

  implicit val arbitraryReaderFn: Arbitrary[Reader[Int, Int => Int]] = Arbitrary(for {
    i <- Arbitrary.arbitrary[Int]
  } yield Reader[Int, Int => Int](_ => _ => i))

  implicit val readerEq1: Eq[Reader[Int, Int]] = new Eq[Reader[Int, Int]] {
    def eqv(x: Reader[Int, Int], y: Reader[Int, Int]): Boolean = x.run(1) == y.run(1)
  }

  implicit val readerEq2: Eq[Reader[Int, (Int, Int, Int)]] = new Eq[Reader[Int, (Int, Int, Int)]] {
    def eqv(x: Reader[Int, (Int, Int, Int)], y: Reader[Int, (Int, Int, Int)]): Boolean = x.run(1) == y.run(1)
  }

  implicit val readerIsomorphism: Isomorphisms[({type l[a] = Reader[Int, a]})#l] =
    CartesianTests.Isomorphisms.invariant[({type l[a] = Reader[Int, a]})#l]

}

trait CowriterInstances {

  implicit val arbitraryCowriter: Arbitrary[Cowriter[Int, Int]] = Arbitrary(for {
    i <- Arbitrary.arbitrary[Int]
  } yield Cowriter[Int, Int](_ => i))

  implicit val cowriterEq1: Eq[Cowriter[Int, Int]] = new Eq[Cowriter[Int, Int]] {
    def eqv(x: Cowriter[Int, Int], y: Cowriter[Int, Int]): Boolean = x.tell(1) == y.tell(1)
  }

  implicit val cowriterEq2: Eq[Cowriter[Int, Cowriter[Int, Int]]] = new Eq[Cowriter[Int, Cowriter[Int, Int]]] {
    def eqv(x: Cowriter[Int, Cowriter[Int, Int]], y: Cowriter[Int, Cowriter[Int, Int]]): Boolean = x.tell(1).tell(1) == y.tell(1).tell(1)
  }

  implicit val cowriterEq3: Eq[Cowriter[Int, Cowriter[Int, Cowriter[Int, Int]]]] = new Eq[Cowriter[Int, Cowriter[Int, Cowriter[Int, Int]]]] {
    def eqv(x: Cowriter[Int, Cowriter[Int, Cowriter[Int, Int]]], y: Cowriter[Int, Cowriter[Int, Cowriter[Int, Int]]]): Boolean = x.tell(1).tell(1).tell(1) == y.tell(1).tell(1).tell(1)
  }
}

object IntMonoid {
  implicit val additiveIntMonoid: cats.Monoid[Int] = new cats.Monoid[Int] {
    val empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y
  }
}