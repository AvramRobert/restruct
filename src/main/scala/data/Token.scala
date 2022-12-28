package data

import data.{Key, Label, Tempo}

enum Token[A](val ref: String):
  case Maker extends Token[Label]("maker")
  case Name extends Token[Label]("name")
  case Key extends Token[Key]("key")
  case Tempo extends Token[Tempo]("tempo")

enum Pattern:
  case TokenPattern[A](token: Token[A]) extends Pattern

enum Emission:
  case ParsingEmission[A](val token: Token[A], val value: A) extends Emission

extension (meta: Emission)
  def pattern: Pattern = meta match {
    case Emission.ParsingEmission(token, _) => Pattern.TokenPattern(token)
  }