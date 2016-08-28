package rjs.recursion.data

import cats.Functor

case class Fix[F[_] : Functor](unFix: F[Fix[F]])

object unFix { def apply[F[_]]: Fix[F] => F[Fix[F]] = _.unFix }

case class Cofix[F[_]](unCofix: F[Cofix[F]])

object unCoFix { def apply[F[_]]: Cofix[F] => F[Cofix[F]] = _.unCofix }

