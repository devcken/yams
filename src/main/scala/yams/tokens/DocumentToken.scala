package yams.tokens

case class DocumentToken(node: NodeToken, directiveTokens: List[DirectiveToken] = List()) extends Token {
  def +(directives: List[DirectiveToken]): DocumentToken = DocumentToken(node, directives)
}
