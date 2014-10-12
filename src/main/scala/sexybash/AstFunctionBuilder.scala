package sexybash

object AstFunctionBuilder {
  sealed trait State
  case object NOOP extends State
  case object STRINGBLOCKSTARTED extends State
  case object FIRSTINDENTATION extends State
  case object CONTENT extends State
  case object INDENTATION extends State
  case object END extends State
  case object COMMENT extends State

  val Space = ' '
  val Tab = '\t'
  val CarriageReturn = '\r'
  val NewLine = '\n'
  val Comment = '#'
}

abstract class AstFunctionBuilder {
  import AstFunctionBuilder._

  def process(c: Char, state: State): Either[Throwable, (State, AstFunctionBuilder)]
  def build: AstFunction
}
