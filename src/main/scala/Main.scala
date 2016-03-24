package rjs

object Main {
  def main(args: Array[String]): Unit = {
    println(CoFree.cofreeMu(AST.example))
    println(TypeCheck.attribute(CoFree.cofreeMu(AST.example)))
    println(TypeCheck.typeTree(CoFree.cofreeMu(AST.example)))
  }
}
