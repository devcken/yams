package yams.nodes


trait Node {
  protected val kind: NodeKind
}

case class NodeProperty(tag: Option[String] = None, anchor: Option[String] = None)

sealed trait NodeKind

case class Scalar() extends NodeKind
case class Sequence() extends NodeKind
case class Mapping() extends NodeKind
case class Anchor() extends NodeKind
