package rjs.lens

case class BasicLens[S, A](getter: S => A, setter: S => A => S)

object BasicLens {
  def ix(index: Int): BasicLens[List[Int], Int] = BasicLens[List[Int], Int](
    getter = (_: List[Int]).apply(index),
    setter = curried((_: List[Int]).updated(index, _))
  )

  private def curried[A, B, C](f: (A, B) => C): A => B => C = {a => b => f(a, b)}

  def run = {
    val in = List(1,2,3,4)
    val lens = ix(2)
    lens.setter(in)(lens.getter(in) + 1)
  }
}

object Tea {

}
