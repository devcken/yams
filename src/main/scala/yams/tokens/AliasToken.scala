package yams
package tokens

case class AliasToken(anchor: String) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): FlowNodeToken = this
}
