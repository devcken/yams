package yams.tokens

trait NodeToken extends Token {
  def +(property: Option[NodePropertyToken]): NodeToken
}
