package yams.tokens

case class FlowSequenceToken(entries: List[FlowNodeToken]) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): FlowNodeToken = this
}
