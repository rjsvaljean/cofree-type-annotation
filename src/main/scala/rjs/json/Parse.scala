package rjs.json

import fastparse.all._
import rjs.recursion.data.{JsValueF, JSNull, JSNumber, JSBool, JSString, JSArray, JSObject}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name

}

class ParseJSValueF[R](parseR: => P[R]) {


  private val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  private val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  private val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  private val space         = P( CharsWhile(Whitespace).? )
  private val digits        = P( CharsWhile(Digits))
  private val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  private val fractional    = P( "." ~ digits )
  private val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

  private val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => JSNumber[R](x.toDouble)
  )

  private val `null`        = P( "null" ).map(_ => new JSNull[R])
  private val `false`       = P( "false" ).map(_ => JSBool[R](false))
  private val `true`        = P( "true" ).map(_ => JSBool[R](true))

  private val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  private val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  private val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  private val strChars = P( CharsWhile(StringChars) )
  private val string =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JSString[R](_))

  private lazy val array: P[JSArray[R]] =
    P( "[" ~/ parseR.rep(sep=",".~/) ~ space ~ "]").map(i => JSArray(i.toVector))

  private lazy val pair: P[(String, R)] = P( string.map(_.asSting) ~/ ":" ~/ parseR )

  private lazy val obj: P[JSObject[R]] =
    P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(i => JSObject[R](i.toVector))

  lazy val jsonExpr: P[JsValueF[R]] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )

  def apply(str: String): JsValueF[R] = jsonExpr.parse(str).get.value

}

object pJSValueF {
  def apply[R](parseR: => Parser[R]): Parser[JsValueF[R]] = new ParseJSValueF(parseR).jsonExpr
}
