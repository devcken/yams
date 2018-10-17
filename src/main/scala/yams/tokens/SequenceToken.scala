package yams.tokens

case class SequenceToken(entries: List[NodeToken]) extends NodeToken {
  override def +(property: Option[NodePropertyToken]): NodeToken = this
}
