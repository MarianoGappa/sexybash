package sexybash

class DefaultAstFunctionBuilder extends AstFunctionBuilder {
  import AstFunctionBuilder._

  // TODO mmm I don't have state here
  override def build = new DefaultAstFunction

  def process(c: Char, state: State): Either[Throwable, (State, AstFunctionBuilder)] = (state, c) match {
    case (NOOP, c: Char) if c != '[' ⇒ Right((NOOP, this))
    case (NOOP, Comment)             ⇒ Right((COMMENT, this))
    case (COMMENT, NewLine)          ⇒ Right((NOOP, this))
    case (COMMENT, _)                ⇒ Right((COMMENT, this))
    case (NOOP, '[')                 ⇒ Right((STRINGBLOCKSTARTED, StringBlockAstFunctionBuilder.apply))
  }
}
