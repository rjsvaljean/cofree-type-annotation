package rjs.recursion.pwd

import scala.collection.mutable

object language {

  sealed trait Alphabet
  case object A extends Alphabet
  case object B extends Alphabet
  case object C extends Alphabet

  sealed trait Language
  case object Empty extends Language
  case object Eps extends Language
  case class Char(value: Alphabet) extends Language

  class Cat(left: => Language, right: => Language) extends Language {
    lazy val getLeft = left
    lazy val getRight = right
  }
  object Cat {
    def apply(left: => Language, right: => Language): Language = new Cat(left, right)
    def unapply(cat: Cat): Option[(Language, Language)] = Some((cat.getLeft, cat.getRight))
  }
  class Alt(dis: => Language, dat: => Language) extends Language {
    lazy val getDis = dis
    lazy val getDat = dat
  }
  object Alt {
    def apply(dis: => Language, dat: => Language): Language = new Alt(dis, dat)
    def unapply(alt: Alt): Option[(Language, Language)] = Some((alt.getDis, alt.getDat))
  }
  class Rep(lang: => Language) extends Language {
    lazy val getLang = lang
  }
  object Rep {
    def apply(lang: => Language): Language = new Rep(lang)
    def unapply(rep: Rep): Option[Language] = Some(rep.getLang)
  }

  /*
  *
  * +          +
  * |       =  |       ◦ {a, b} ∪ ϵ
  * +--' ab    +--' ab
  *
  * */

  val L: Language = Alt(Eps, Cat(L, Alt(Char(A), Char(B))))

  def isNullable(language: => Language): Boolean = language match {
    case Empty => false
    case Eps => true
    case Char(value) => false
    case Alt(dis, dat) => isNullable(dis) || isNullable(dat)
    case Cat(left, right) => isNullable(left) && isNullable(right)
    case Rep(lang) => true
  }

  val memo: mutable.Map[(Alphabet, Language), Language] = mutable.Map()

  def derivative(c: Alphabet, l: Language): Language = l match {
    case Empty => Empty
    case Eps => Empty
    case Char(value) if value == c => Eps
    case Char(value) if value != c => Empty
    case Alt(l1, l2) =>
      Alt(derivative(c, l1), derivative(c, l2))
    case Cat(l1, l2) if isNullable(l1) =>
      Alt(derivative(c, l2), Cat(derivative(c, l1), l2))
    case Cat(l1, l2) =>
      Cat(derivative(c, l1), l2)
    case Rep(l1) =>
      Cat(derivative(c, l1), l)
  }

  def memoizeDerivative(der: (Alphabet, Language) => Language): (Alphabet, Language) => Language = {
    (c: Alphabet, l: Language) =>
      val derived = memo.getOrElse((c, l), der(c, l))
      memo.put((c, l), derived)
      derived
  }

  val memoizedDerivative = memoizeDerivative(derivative)

  def matches(w: Seq[Alphabet], l: Language): Boolean =
    if (w.isEmpty) isNullable(l)
    else matches(w.tail, memoizedDerivative(w.head, l))

  def run() = {
    println(matches(Seq(A), L))
  }
}
