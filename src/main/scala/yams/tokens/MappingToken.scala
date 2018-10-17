package yams.tokens

case class MappingToken(entries: List[EntryToken]) extends NodeToken {
  override def +(property: Option[NodePropertyToken]): NodeToken = this
}
