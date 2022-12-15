package parser

import Parsing.Parser

enum Token[A](val ref: String):
  case Maker extends Token[LabelMetadata]("maker")
  case Name extends Token[LabelMetadata]("name")
  case Key extends Token[KeyMetadata]("key")
  case Tempo extends Token[TempoMetadata]("tempo")

enum Rule:
  case ParsingRule[A](val token: Token[A], val parser: Parser[A]) extends Rule

enum Metadata:
  case ParsingMetadata[A](val token: Token[A], val value: A) extends Metadata