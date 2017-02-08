package rjs.recursion

import cats.Order
import rjs.recursion.data.{Fix, Stream, StreamF, StreamFA, ConsF, ListFA, NilF, MyList}
import schemes.ana

object Part3Anamorphisms {
  def rememberFoldr[A, B](f: Option[(A, B)] => B): List[A] => B = {
    case Nil => f(None)
    case x :: xs => f(Some(x, rememberFoldr(f)(xs)))
  }

  def unfoldR[A, B](f: B => Option[(A, B)]): B => List[A] = { (b: B) =>
    f(b) match {
      case None => Nil
      case Some((a, newB)) => a :: unfoldR(f)(newB)
    }
  }

  def replicate[A](x: A): Int => List[A] = {
    def c: Int => Option[(A, Int)] = { (n: Int) =>
      if (n == 0) None
      else Some((x, n - 1))
    }
    unfoldR[A, Int](c)
  }


  val tails: (List[Int]) => Fix[ListFA[List[Int]]#l] = ana[ListFA[List[Int]]#l, List[Int]]({
    case Nil => NilF
    case h :: t => ConsF(h :: t, t)
  })
  def runTails = MyList.toList(tails(List(1,2,3)))

  def range[A](start: A, end: A, produce: A => A) = {
    val genEndExclusive = ana[ListFA[A]#l, A] {
      case `end` => NilF
      case a => ConsF(a, produce(a))
    }
    val genInclusive = ana[ListFA[A]#l, Option[A]] {
      case Some(`end`) => ConsF(end, None)
      case Some(a) => ConsF(a, Some(produce(a)))
      case None => NilF
    }
    genInclusive(Some(start))
    genEndExclusive(start)
  }
  def runRange = MyList.toList(range[Int](1, 10, _ + 1))

  def splitBy[T](pred: T => Boolean): List[T] => List[List[T]] = {
    def c: List[T] => Option[(List[T], List[T])] = {
      case Nil => None
      case xs =>
        val (chunk, rest) = xs.span(pred andThen(!_))
        Some((chunk, rest.drop(1)))
    }
    unfoldR(c)
  }

  def mergeLists[A: Order]: (List[A], List[A]) => List[A] = {
    val c: ((List[A], List[A])) => Option[(A, (List[A], List[A]))] = {
      case (Nil, Nil) => None
      case (Nil, y :: ys) => Some((y, (Nil, ys)))
      case (x :: xs, Nil) => Some((x, (xs, Nil)))
      case (x :: xs, y :: ys) if Order[A].lteqv(x, y) => Some((x, (xs, y :: ys)))
      case (x :: xs, y :: ys) if Order[A].gt(x, y) => Some((y, (x :: xs, ys)))
    }
    Function.untupled(unfoldR[A, (List[A], List[A])](c))
  }

  // example of co-recursion
  // - produces (potentially infinite) codata
  // - as opposed to recursion which consumes necessarily finite data
  // Using cata or ana only, our program is guaranteed to terminate
  // However not every program can be written in terms of just cata or ana


  // def cata[F[_]: Functor, A](alg: F[A] => A): Fix[F] => A =
  //   alg compose fmap[F, Fix[F], A](cata(alg)) compose unFix[F]


  // def ana[F[_]: Functor, A](coalg: A => F[A]): A => Fix[F] =
  //   coalg andThen fmap[F, A, Fix[F]](ana(coalg)) andThen Fix[F]


  // def ana'[F[_]: Functor, A](coalg: A => F[A]): A => Cofix[F] =
  //   coalg andThen fmap[F, A, Cofix[F]](ana'(coalg)) andThen Cofix[F]

  // There's no enforced distinction between data and codata, so we can use Fix
  // (apparently in Agda and Coq we would be required to make this distinction)
  // However it is often useful to try and enforce this distinction, especially when working with streams
  // ???

  // Also Ref : rjs.recursion.data.Fixpoint.ana
  /*
 F[Cofix[F]] <-- fmap(ana(alg)) --- F[A]
     ^                               ^
     |                               |
     |                               |
  unCofix                          coalg
     |                               |
     |                               |
     |                               |
  Cofix[F] <------ ana coalg ------- A
  */


  def iterateS[A](f: A => A): A => Stream.T[A] = {
    def c(x: A): StreamF[A, A] = StreamF(x, f(x))
    schemes.anaCofix[StreamFA[A]#l, A](c)
  }

  val s1: Stream.T[Int] = iterateS((_: Int) + 1)(1)

}
