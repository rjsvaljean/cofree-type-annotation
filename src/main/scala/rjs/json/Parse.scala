package rjs.json

import fastparse.all._

object Parse {

  def parseA = P("a")

  def run() = {
    val Parsed.Success(value, successIndex) = parseA.parse("a")
    assert(value == () && successIndex == 1)
  }
}
