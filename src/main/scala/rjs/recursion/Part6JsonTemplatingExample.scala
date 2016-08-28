package rjs.recursion

import cats.Functor
import fastparse.all._

import rjs.json.pJSValueF
import rjs.recursion.data.{Ctx, CtxF, CtxFA, Fix, Hole, JsValueF, Term, JSValue, JSNull, JSNumber}
import rjs.recursion.schemes.cata
import rjs.recursion.utils.fix

object Part6JsonTemplatingExample {

  def fillHoles[F[_]: Functor, A](g: A => Fix[F]): Ctx.T[F, A] => Fix[F] = {
    val alg: CtxF[F, A, Fix[F]] => Fix[F] = {
      case Term(t) => Fix[F](t)
      case Hole(a) => g(a)
    }
    cata[CtxFA[F, A]#l, Fix[F]](alg)
  }

  type Name = String
  type JSTemplate = Ctx.T[JsValueF, Name]
  def pVar: Parser[Name] = "${" ~ CharIn('a' to 'z').rep.! ~ "}"

  def pJSTemplate: Parser[Ctx.T[JsValueF, Name]] = {
    def f(p: => Parser[Ctx.T[JsValueF, Name]]) = {
      val hole = pVar.map(Hole[JsValueF, Name, Ctx.T[JsValueF, Name]])
      val term = pJSValueF(p).map(Term[JsValueF, Name, Ctx.T[JsValueF, Name]])
      val holeOrTerm = hole | term
      holeOrTerm.map(Fix[CtxFA[JsValueF, Name]#l](_))
    }
    fix(f)
  }

  def parseUnsafe[R](parser: Parser[R]) = parser.parse(_: String).get.value
  val temp1 = parseUnsafe(pJSTemplate)("[{\"foo\":${a}}]")

  def vlookup[A](env: Map[A, JSValue.T]): A => JSValue.T = {
    env.getOrElse(_: A, Fix(new JSNull): JSValue.T)
  }

  val temp2 = {
    val f = fillHoles(vlookup(Map("a" -> Fix(JSNumber(42)))))
    f(temp1)
  }
}
