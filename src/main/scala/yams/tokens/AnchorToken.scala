package yams.tokens

case class Anchor(anchor: String)
case class AnchorToken(anchor: Anchor) extends Token
