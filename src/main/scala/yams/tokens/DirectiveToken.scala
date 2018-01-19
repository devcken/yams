package yams.tokens

sealed abstract class DirectiveToken() extends Token
final case class YamlDirectiveToken(version: yams.Version) extends DirectiveToken
final case class TagDirectiveToken(tags: (String, String)) extends DirectiveToken
final case class ReservedDirectiveToken(name: String, parameters: List[String]) extends DirectiveToken
