package yams.tokens

case class FlowEntryToken(key: FlowNodeToken, value: FlowNodeToken) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): FlowNodeToken = this
}
