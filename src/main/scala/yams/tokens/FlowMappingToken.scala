package yams.tokens

case class FlowMappingToken(entries: List[FlowEntryToken]) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): FlowNodeToken = this
}
