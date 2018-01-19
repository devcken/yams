package yams.tokens

trait FlowNodeToken extends Token {
  def +(property: Option[NodePropertyToken]): FlowNodeToken
}
