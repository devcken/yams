package yams.tokens

case class DocumentToken(node: FlowNodeToken, directiveTokens: List[DirectiveToken] = List()) extends Token {
  def +(directives: List[DirectiveToken]): DocumentToken = DocumentToken(node, directives)
}
