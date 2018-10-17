package yams.tokens

case class EntryToken(key: NodeToken, value: NodeToken) extends NodeToken {
  override def +(property: Option[NodePropertyToken]): NodeToken = this
}
