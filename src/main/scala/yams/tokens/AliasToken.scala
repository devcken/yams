package yams
package tokens

case class AliasToken(anchor: String) extends NodeToken {
  override def +(property: Option[NodePropertyToken]): NodeToken = this
}
