package yams
package tokens

case class ScalarToken(value: String, style: Scalar = Plain, property: Option[NodePropertyToken] = None) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): ScalarToken = {
    ScalarToken(value, style, property)
  }
}
