package yams.tokens

sealed trait ScalarStyle

case object DoubleQuoted extends ScalarStyle
case object SingleQuoted extends ScalarStyle
case object Literal extends ScalarStyle
case object Folded extends ScalarStyle
case object Plain extends ScalarStyle

case class ScalarToken(value: String, style: ScalarStyle = Plain, property: Option[NodePropertyToken] = None) extends FlowNodeToken {
  override def +(property: Option[NodePropertyToken]): ScalarToken = {
    ScalarToken(value, style, property)
  }
}
