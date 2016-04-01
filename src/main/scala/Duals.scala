package rjs

import cats.{Comonad, Functor, Monad, Monoid}

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

case class Store[S, A](peek: S => A, cursor: S)

object Store {
  implicit def storeComonad[S]: Comonad[({type l[a] = Store[S, a]})#l] =
    new Comonad[({type l[a] = Store[S, a]})#l] {
      def extract[A](x: Store[S, A]): A = x.peek(x.cursor)

      def coflatMap[A, B](fa: Store[S, A])(f: (Store[S, A]) => B): Store[S, B] = Store(s => f(Store(fa.peek, s)), fa.cursor)

      def map[A, B](fa: Store[S, A])(f: (A) => B): Store[S, B] = coflatMap(fa)(extract[A] _ andThen f)
    }
}

case class NEL[A](head: A, tail: Option[NEL[A]]) {
  def tails: NEL[NEL[A]] = NEL(this, tail.map(_.tails))
}

object NEL {
  implicit val NELComonad: Comonad[NEL] = new Comonad[NEL] {
    def extract[A](x: NEL[A]): A = x.head

    def coflatMap[A, B](fa: NEL[A])(f: (NEL[A]) => B): NEL[B] = map(fa.tails)(f)

    def map[A, B](fa: NEL[A])(f: (A) => B): NEL[B] = NEL(f(fa.head), fa.tail.map(map(_)(f)))
  }
}

case class Tree[A](tip: A, sub: List[Tree[A]]) {
  def subTrees: Tree[Tree[A]] = Tree(this, sub.map(_.subTrees))
}

object Tree {
  implicit val TreeComonad: Comonad[Tree] = new Comonad[Tree] {
    def extract[A](x: Tree[A]): A = x.tip

    def coflatMap[A, B](fa: Tree[A])(f: (Tree[A]) => B): Tree[B] = map(fa.subTrees)(f)

    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = Tree(f(fa.tip), fa.sub.map(map(_)(f)))
  }
}

trait Adjunction[F[_], G[_]] {
  def left[A, B](f: F[A] => B): A => G[B]
  def right[A, B](f: A => G[B]): F[A] => B
  def monad(implicit FunctorG: Functor[G]): Monad[({type l[a] = G[F[a]]})#l] = new Monad[({type l[a] = G[F[a]]})#l] {
    def pure[A](x: A): G[F[A]] = left(identity[F[A]])(x)

    def flatMap[A, B](fa: G[F[A]])(f: (A) => G[F[B]]): G[F[B]] = FunctorG.map(fa)(right(f))
  }
  def comonad(implicit FunctorF: Functor[F]): Comonad[({type l[a] = F[G[a]]})#l] = new Comonad[({type l[a] = F[G[a]]})#l] {
    def extract[A](x: F[G[A]]): A = right(identity[G[A]])(x)

    def coflatMap[A, B](fa: F[G[A]])(f: (F[G[A]]) => B): F[G[B]] = FunctorF.map(fa)(left(f))

    def map[A, B](fa: F[G[A]])(f: (A) => B): F[G[B]] = coflatMap(fa)(extract[A] _ andThen f)
  }
}


object Adjunction {
  def homSetAdj[R] = new Adjunction[({type l[a] = Coreader[R, a]})#l, ({type l[a] = Reader[R, a]})#l] {
    def left[A, B](f: (Coreader[R, A]) => B): (A) => Reader[R, B] = {(a: A) => Reader((r: R) => f(Coreader(a, r)))}

    def right[A, B](f: (A) => Reader[R, B]): (Coreader[R, A]) => B = { in =>
      val Coreader(extract, ask) = in
      f(extract).run(ask)
    }
  }

  def otherAdj[W : Monoid] = new Adjunction[({type l[a] = Writer[W, a]})#l, ({type l[a] = Cowriter[W, a]})#l] {
    def left[A, B](f: (Writer[W, A]) => B): (A) => Cowriter[W, B] = {(a: A) => Cowriter((r: W) => f(Writer(a, r)))}

    def right[A, B](f: (A) => Cowriter[W, B]): (Writer[W, A]) => B = { in =>
      val Writer(value, log) = in
      f(value).tell(log)
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
