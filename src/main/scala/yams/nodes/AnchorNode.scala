package yams.nodes

sealed class AnchorNode(original: Node) extends Node {
  override val kind: NodeKind = Anchor()
}
