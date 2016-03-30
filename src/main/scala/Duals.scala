package rjs

import cats.{Comonad, Monad, Monoid}

import scala.util.Try

// From : http://blog.higher-order.com/blog/2015/06/23/a-scala-comonad-tutorial/

case class Reader[R, A](run: R => A)

object Reader {
  def ask[R] = Reader[R, R](identity)

  implicit def readerMonad[R]: Monad[({type l[a] = Reader[R, a]})#l] =
    new Monad[({type l[a] = Reader[R, a]})#l] {
      def pure[A](x: A): Reader[R, A] = Reader(_ => x)

      def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] = {
        Reader((r: R) => f(fa.run(r)).run(r))
      }
    }

}

case class Coreader[R, A](extract: A, ask: R)

object Coreader {
  implicit def CoreaderComonad[R]: Comonad[({type l[a] = Coreader[R, a]})#l] =
    new Comonad[({type l[a] = Coreader[R, a]})#l] {
      def extract[A](x: Coreader[R, A]): A = x.extract

      def coflatMap[A, B](fa: Coreader[R, A])(f: (Coreader[R, A]) => B): Coreader[R, B] = Coreader(f(fa), fa.ask)

      def map[A, B](fa: Coreader[R, A])(f: (A) => B): Coreader[R, B] = Coreader(f(fa.extract), fa.ask)
    }
}

case class Cowriter[W, A](tell: W => A)(implicit logMonoid: Monoid[W])

object Cowriter {
  implicit def CowriterComonad[W](implicit logMonoid: Monoid[W]): Comonad[({type l[a] = Cowriter[W, a]})#l] =
    new Comonad[({type l[a] = Cowriter[W, a]})#l] {
      def extract[A](x: Cowriter[W, A]): A = x.tell(logMonoid.empty)

      def coflatMap[A, B](fa: Cowriter[W, A])(f: (Cowriter[W, A]) => B): Cowriter[W, B] = Cowriter((w: W) => f(fa))

      def map[A, B](fa: Cowriter[W, A])(f: (A) => B): Cowriter[W, B] = Cowriter(fa.tell andThen f)
    }
}

case class Writer[W, A](value: A, log: W)

object Writer {
  def tell[W, A](w: W): Writer[W, Unit] = Writer((), w)

  implicit def writerMonad[W](implicit logMonoid: Monoid[W]): Monad[({type l[a] = Writer[W, a]})#l] =
    new Monad[({type l[a] = Writer[W, a]})#l] {
      def pure[A](x: A): Writer[W, A] = Writer(x, logMonoid.empty)

      def flatMap[A, B](fa: Writer[W, A])(f: (A) => Writer[W, B]): Writer[W, B] = {
        Writer(f(fa.value).value, logMonoid.combine(f(fa.value).log, fa.log))
      }
    }
}

case class State[S, A](run: S => (S, A))

object State {
  def get[S]: State[S, S] = State((s: S) => (s, s))
  def put[S](s: S): State[S, Unit] = State((_: S) => (s, ()))

  implicit def stateMonad[S]: Monad[({type l[a] = State[S, a]})#l] =
    new Monad[({type l[a] = State[S, a]})#l] {
      def pure[A](x: A): State[S, A] = State((s: S) => (s, x))

      def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = State {(s: S) =>
        val (newS, a) = fa.run(s)
        f(a).run(newS)
      }
    }
}

object Duals {
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def run: Unit = List(
    "Reader",
    for {
      i <- Reader[String, Option[Int]]((r: String) => Try(r.toInt).toOption)
    } yield i.getOrElse(0),

    "Coreader",
    for {
      i <- Coreader[String, Option[Int]](Some(1), "1")
    } yield i.getOrElse(0)
  ).foreach(println)
}
