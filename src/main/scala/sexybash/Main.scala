package sexybash

import sexybash.AstFunctionBuilder.NOOP

import scala.annotation.tailrec
import scala.io.Source

object Main {
  def main(args: Array[String]) = println(eval(args))

  def eval(args: Array[String]): String =
    if(args.size != 1) {
      Help
    } else {
      parseAst(readFile(args(0))).toString
    }

  def parseAst(s: List[Char]): AstFunction =
    executeBuilder(s, new DefaultAstFunctionBuilder(), NOOP) match {
      case Right(f: AstFunction) => f
      case Left(t) => throw t
    }

  @tailrec
  private def executeBuilder(string: List[Char], builder: AstFunctionBuilder, state: AstFunctionBuilder.State): Either[Throwable, AstFunction] =
    (state, string) match {
      case (AstFunctionBuilder.END, _) | (AstFunctionBuilder.NOOP, Nil) => Right(builder.build)
      case (_, Nil) => Left(new RuntimeException("Reached string end without arriving to END state"))
      case (_, c :: xs) => builder.process(c, state) match {
        case Right((newState, newBuilder)) =>
          executeBuilder(xs, newBuilder, newState)
        case Left(e: Throwable) => throw e
      }
    }

  private def readFile(filename: String) = {
    Source.fromFile(filename, "utf8").getLines().mkString("\n").toList
  }

  val Help = "Usage: sexybash /path/to/file"
}





