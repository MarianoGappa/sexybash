package sexybash

case class StringBlockAstFunctionBuilder(indentation: String, content: String, partialIndentation: String = "") extends AstFunctionBuilder {
  import AstFunctionBuilder._

  // TODO mmm I don't have state here
  override def build = StringBlockAstFunction(content)

  def process(c: Char, state: State): Either[Throwable, (State, AstFunctionBuilder)] = (state, c) match {
    case (s, CarriageReturn)                                ⇒ Right((s, this))
    case (NOOP, '[')                                        ⇒ Right((STRINGBLOCKSTARTED, this))
    case (NOOP, c: Char)                                    ⇒ Left(new RuntimeException(s"Expecting [ in NOOP state but got [$c]."))
    case (STRINGBLOCKSTARTED, Space | Tab)                  ⇒ Right((STRINGBLOCKSTARTED, this))
    case (STRINGBLOCKSTARTED, NewLine)                      ⇒ Right((FIRSTINDENTATION, this))
    case (STRINGBLOCKSTARTED, c: Char)                      ⇒ Left(new RuntimeException(s"Expecting space, tab or newline in STRINGBLOCKSTARTED state but got [$c]."))
    case (FIRSTINDENTATION, Space | Tab)                    ⇒ Right((FIRSTINDENTATION, this.copy(indentation + c)))
    case (FIRSTINDENTATION, NewLine) if indentation.isEmpty ⇒ Right((FIRSTINDENTATION, this.copy("")))
    case (FIRSTINDENTATION, NewLine)                        ⇒ Right((INDENTATION, this))
    case (FIRSTINDENTATION, c: Char)                        ⇒ Right((CONTENT, this.copy(content = content + c)))
    case (CONTENT, NewLine)                                 ⇒ Right((INDENTATION, this.copy(content = content + NewLine)))
    case (CONTENT, c: Char)                                 ⇒ Right((CONTENT, this.copy(content = content + c)))
    case (INDENTATION, ']')                                 ⇒ Right((END, this))
    case (INDENTATION, c: Char) if matchesIndentation(c)    ⇒ Right((CONTENT, this.copy(partialIndentation = "")))

    case (INDENTATION, c: Char) if matchesPartialIndentation(c) ⇒
      Right((INDENTATION, this.copy(partialIndentation = partialIndentation + c)))

    case (INDENTATION, c: Char) ⇒ Left(new RuntimeException(s"Expecting correct indentation in INDENTATION state but got [$c], content is [$content]."))
  }

  private def isIndentation(c: Char) = Set(Space, Tab).contains(c)
  private def matchesIndentation(c: Char) = isIndentation(c) && indentation == partialIndentation + c
  private def matchesPartialIndentation(c: Char) = {
    isIndentation(c) && partialIndentation + c == indentation.substring(0, (partialIndentation + c).size)
  }
}

object StringBlockAstFunctionBuilder {
  def apply: StringBlockAstFunctionBuilder = new StringBlockAstFunctionBuilder("", "", "")
}
