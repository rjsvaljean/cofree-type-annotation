package rjs.recursion


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

  def run = {
    Stream.takeS(10)(exch(rjs.recursion.Part3Anamorphisms.s1))
  }
}
