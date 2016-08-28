package rjs.recursion.data

import cats.Functor

// Stream with no termination case
class StreamF[A, R](val a: A, _r: => R) {
  lazy val r = _r
}
object StreamF {
  def apply[A, R](a: A, r: => R): StreamF[A, R] = new StreamF[A, R](a, r)
  def unapply[A, R](streamF: StreamF[A, R]): Option[(A, R)] = Some((streamF.a, streamF.r))
  implicit def functor[A]: Functor[StreamFA[A]#l] =
    new Functor[StreamFA[A]#l] {
      def map[R, B](fa: StreamF[A, R])(f: (R) => B): StreamF[A, B] = StreamF(fa.a, f(fa.r))
    }
}


trait StreamFA[A] { type l[r] = StreamF[A, r] }

object Stream {
  type T[A] = Cofix[StreamFA[A]#l]

  def consS[A, R](x: A, xs: Cofix[StreamFA[A]#l]): Stream.T[A] = Cofix[StreamFA[A]#l](StreamF(x, xs))
  def headS[A](s: Stream.T[A]): A = s.unCofix.a
  def tailS[A](s: Stream.T[A]): Cofix[StreamFA[A]#l] = s.unCofix.r

  def takeS[A](n: Int): Stream.T[A] => List[A] = n match {
    case 0 => (_: Stream.T[A]) => Nil
    case _n => { (stream: Stream.T[A]) =>
      val StreamF(x, xs) = stream.unCofix
      x :: takeS(_n - 1)(xs)
    }
  }
}


