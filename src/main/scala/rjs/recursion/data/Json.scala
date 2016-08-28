package rjs.recursion.data

import cats.Functor

import rjs.recursion.utils.fix

sealed trait JsValueF[R]
class JSNull[R] extends JsValueF[R]
case class JSBool[R](asBoolean: Boolean) extends JsValueF[R]
case class JSNumber[R](asDouble: Double) extends JsValueF[R]
case class JSString[R](asSting: String) extends JsValueF[R]
case class JSArray[R](asVector: Vector[R]) extends JsValueF[R]
case class JSObject[R](asAssocList: Vector[(String, R)]) extends JsValueF[R]


object JsValueF {
  implicit object functor extends Functor[JsValueF] {
    def map[A, B](fa: JsValueF[A])(f: (A) => B): JsValueF[B] = fa match {
      case JSBool(asBoolean) => JSBool(asBoolean)
      case JSNumber(asDouble) => JSNumber(asDouble)
      case JSString(asSting) => JSString(asSting)
      case JSArray(asVector) => JSArray(asVector.map(f))
      case JSObject(asAssocList) => JSObject(asAssocList.map { case (k, v) => (k, f(v)) } )
      case _: JSNull[A] => new JSNull
    }
  }
}


object JSValue {
  type T = Fix[JsValueF]

  import fastparse.all._
  def pars(p: => P[JSValue.T]): P[JSValue.T] = rjs.json.pJSValueF(p).map(Fix[JsValueF])
  val pJSValue: P[JSValue.T] = fix[Parser[JSValue.T]](pars)
}

