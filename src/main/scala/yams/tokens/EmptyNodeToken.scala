package yams.tokens

case class EmptyNodeToken(property: Option[NodePropertyToken] = None) extends NodeToken {
  override def +(property: Option[NodePropertyToken]): NodeToken = EmptyNodeToken(property)
}
