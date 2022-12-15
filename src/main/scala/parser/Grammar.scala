package parser

import Parsing.Parser

enum Token[A](val ref: String):
  case Title extends Token[TitleMetadata]("title")
  case Key extends Token[KeyMetadata]("key")
  case Tempo extends Token[TempoMetadata]("tempo")

enum Rule:
  case ParsingRule[A](val token: Token[A], val parser: Parser[A]) extends Rule

enum Emission:
  case ParsingEmission[A](val token: Token[A], val value: A) extends Emission

type Grammar = List[Rule]
type Output = List[Emission]