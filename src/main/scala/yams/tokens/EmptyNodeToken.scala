package yams.tokens

case class EmptyNodeToken(property: Option[NodePropertyToken] = None) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): FlowNodeToken = EmptyNodeToken(property)
}
