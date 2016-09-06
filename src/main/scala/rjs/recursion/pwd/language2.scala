package rjs.recursion.pwd

import cats.{Applicative, Functor}
import rjs.recursion.data.{Fix, Traversable, unFix}
import rjs.recursion.utils.fix
import rjs.recursion.schemes.para

object language2 {

  sealed trait Alphabet
  case object A extends Alphabet
  case object B extends Alphabet
  case object C extends Alphabet

  sealed trait LanguageF[+R]
  case object Empty extends LanguageF[Nothing]
  case object Eps extends LanguageF[Nothing]
  case class Char(value: Alphabet) extends LanguageF[Nothing]

  class Cat[R](left: => R, right: => R) extends LanguageF[R] {
    lazy val getLeft = left
    lazy val getRight = right
    override def toString: String = s"Cat($getLeft, $getRight)"
    override def equals(other: Any): Boolean =
      other.isInstanceOf[Cat[R]] &&
        other.asInstanceOf[Cat[R]].getLeft == getLeft &&
        other.asInstanceOf[Cat[R]].getRight == getRight
  }
  object Cat {
    def apply[R](left: => R, right: => R): LanguageF[R] = new Cat(left, right)
    def unapply[R](cat: Cat[R]): Option[(R, R)] = Some((cat.getLeft, cat.getRight))
  }
  class Alt[R](dis: => R, dat: => R) extends LanguageF[R] {
    lazy val getDis = dis
    lazy val getDat = dat
    override def toString: String = s"Alt($getDis, $getDat)"
    override def equals(other: Any): Boolean =
      other.isInstanceOf[Alt[R]] &&
        other.asInstanceOf[Alt[R]].getDis == getDis &&
        other.asInstanceOf[Alt[R]].getDat == getDat
  }
  object Alt {
    def apply[R](dis: => R, dat: => R): LanguageF[R] = new Alt(dis, dat)
    def unapply[R](alt: Alt[R]): Option[(R, R)] = Some((alt.getDis, alt.getDat))
  }
  class Rep[R](lang: => R) extends LanguageF[R] {
    lazy val getLang = lang
    override def toString: String = s"Rep($getLang)"
    override def equals(other: Any): Boolean =
      other.isInstanceOf[Rep[R]] &&
        other.asInstanceOf[Rep[R]].getLang == getLang
  }
  object Rep {
    def apply[R](lang: => R): LanguageF[R] = new Rep(lang)
    def unapply[R](rep: Rep[R]): Option[R] = Some(rep.getLang)
  }

  object LanguageF {
    implicit object functor extends Functor[LanguageF] {
      def map[A, B](fa: LanguageF[A])(f: (A) => B): LanguageF[B] = fa match {
        case Empty => Empty
        case Eps => Eps
        case Char(value) => Char(value)
        case Cat(l1, l2) => Cat(f(l1), f(l2))
        case Alt(l1, l2) => Alt(f(l1), f(l2))
        case Rep(l) => Rep(f(l))
      }
    }

    implicit object traversable extends Traversable[LanguageF] {
      def traverse[F[_] : Applicative, A, B](f: => (A => F[B]))(ta: LanguageF[A]): F[LanguageF[B]] = ta match {
        case Empty => Applicative[F].pure(Empty)
        case Eps => Applicative[F].pure(Eps)
        case Char(value) => Applicative[F].pure(Char(value))
        case Cat(l1, l2) => Applicative[F].ap2[B, B, LanguageF[B]](Applicative[F].pure(Cat[B](_, _)))(f(l1), f(l2))
        case Alt(l1, l2) => Applicative[F].ap2[B, B, LanguageF[B]](Applicative[F].pure(Alt[B](_, _)))(f(l1), f(l2))
        case Rep(l) => Applicative[F].map(f(l))(Rep(_))
      }
    }
  }

  type Language = Fix[LanguageF]
  object Language {
    val empty: Language = Fix(Empty: LanguageF[Fix[LanguageF]])
    val eps: Language = Fix(Eps: LanguageF[Fix[LanguageF]])
    def char(value: Alphabet): Language = Fix(Char(value): LanguageF[Fix[LanguageF]])
    def cat(left: => Language, right: => Language): Language = Fix(Cat(left, right))
    def alt(dis: => Language, dat: => Language): Language = Fix(Alt(dis, dat))
    def rep(lang: => Language): Language = Fix(Rep(lang))
  }

  val L: Language = {
    import Language._
    alt(eps, cat(L, alt(char(A), char(B))))
  }

  type Bool[R] = R => Boolean

  def isNullableAlg[R](in: => Bool[Language]): Bool[Language] = unFix[LanguageF] andThen {
    case Empty => false
    case Eps => true
    case Char(value) => false
    case Alt(dis, dat) => in(dis) || in(dat)
    case Cat(left, right) => in(left) && in(right)
    case Rep(lang) => true
  }
  val isNullable: Bool[Language] = fix(isNullableAlg)

  def derivativeAlg[A](c: Alphabet, l: LanguageF[(Language, Language)]): Language = l match {
    case Empty => Language.empty
    case Eps => Language.empty
    case Char(value) if value == c => Language.eps
    case Char(value) if value != c => Language.empty
    case Alt((l1, _l1), (l2, _l2)) =>
      println(s"Alt(${_l1}, ${_l2})")
      Language.alt(l1, l2)
    case Cat((l1, _l1), (l2, _l2)) if isNullable(_l1) =>
      println(s"Cat(${_l1}, ${_l2}) if isNullable(${_l1})")
      Language.alt(l2, Language.cat(l1, _l2))
    case Cat((l1, _l1), (_, _l2)) =>
      println(s"Cat(${_l1}, ${_l2})")
      Language.cat(l1, _l2)
    case Rep((l1, _l1)) =>
      println(s"Rep(${_l1})")
      Language.cat(l1, _l1)
  }

  def derivative(c: Alphabet, l: Language) = {
    val f = rjs.recursion.schemes.paraMemoized[LanguageF, Language](derivativeAlg(c, _))
    f(l)
  }

  def matches(w: Seq[Alphabet], l: Language): Boolean =
    if (w.isEmpty) isNullable(l)
    else matches(w.tail, derivative(w.head, l))

  def run() = {
    import Language._
    val aAndBOrC = cat(char(A), alt(char(B), char(C)))
    println(matches(Seq(A, B), aAndBOrC))
    println(matches(Seq(A), L))

  }
}
