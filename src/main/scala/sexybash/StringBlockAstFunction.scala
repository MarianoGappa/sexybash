package sexybash

case class StringBlockAstFunction(content: String) extends AstFunction {
  override def toString = content
}
