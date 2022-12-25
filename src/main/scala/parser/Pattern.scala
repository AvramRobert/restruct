package parser

import Parsing.Parser

enum Pattern[A](val ref: String):
  case Maker extends Pattern[LabelMetadata]("maker")
  case Name extends Pattern[LabelMetadata]("name")
  case Key extends Pattern[KeyMetadata]("key")
  case Tempo extends Pattern[TempoMetadata]("tempo")

enum Token:
  case PatternToken[A](pattern: Pattern[A]) extends Token

enum Rule:
  case ParsingRule[A](val pattern: Pattern[A], val parser: Parser[A]) extends Rule

enum Metadata:
  case ParsingMetadata[A](val pattern: Pattern[A], val value: A) extends Metadata

extension (meta: Metadata)
  def token: Token = meta match {
    case Metadata.ParsingMetadata(token, _) => Token.PatternToken(token)
  }