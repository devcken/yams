package yams.nodes

sealed class ScalarNode(value: String) extends Node {
  override protected val kind: NodeKind = Scalar()
}
