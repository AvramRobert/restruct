package data

import data.{Key, Label, Tempo, Extension}

/**
 * Represent components with certain semantics that can occur within the name
 * of some file or directory
 */
enum NamingToken[A](val ref: String):
  case MakerToken extends NamingToken[Label]("maker")
  case NameToken extends NamingToken[Label]("name")
  case KeyToken extends NamingToken[Key]("key")
  case TempoToken extends NamingToken[Tempo]("tempo")
  case ExtensionToken extends NamingToken[Extension]("extension")

enum Pattern:
  case TokenPattern[A](token: NamingToken[A]) extends Pattern

enum Emission:
  case ParsingEmission[A](val token: NamingToken[A], val value: A) extends Emission

extension (meta: Emission)
  def pattern: Pattern = meta match {
    case Emission.ParsingEmission(token, _) => Pattern.TokenPattern(token)
  }