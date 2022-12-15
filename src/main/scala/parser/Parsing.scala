package parser
import parser.ParsingResult.Failure

import scala.annotation.tailrec
import scala.util.parsing.combinator.*

object Parsing extends RegexParsers {
  override def skipWhitespace: Boolean = false

  private val delimiters = Set(' ', '-', '_')
  private def asRule[A](token: Token[A], parser: Parser[A]): Rule = Rule.ParsingRule(token, parser)
  private def tokenParser[A](t: Token[A]): Parser[Token[A]] = for {
    _ <- whiteSpace.*
    _ <- lt
    _ <- whiteSpace.*
    _ <- anyCase(t.ref)
    _ <- whiteSpace.*
    _ <- gt
  } yield t
  private def noteParser(note: Note): Parser[Note] = literal(note.encoding).map { _ => note }

  def fromGrammar(grammar: List[Rule]): Parser[List[Metadata]] = Parser { input =>
    @tailrec
    def unravel(rules: List[Rule], data: List[Metadata] = List.empty, rem: Input = input): ParseResult[List[Metadata]] = rules match {
      case Rule.ParsingRule(token, parser) :: ts => parser(rem) match {
        case Success(result, next) => unravel(ts, Metadata.ParsingMetadata(token, result) +: data, next)
        case Error(msg, next)      => Error(msg, next)
        case Failure(msg, next)    => Failure(msg, next)
      }
      case Nil => Success(data.reverse, rem)
    }

    unravel(grammar)
  }

  private def anyCase(value: String): Parser[String] = s"(?i)($value)".r

  val lt: Parser[String] = literal("<")

  val gt: Parser[String] = literal(">")

  val number: Parser[Long] = s"[0-9]*".r.flatMap { s =>
    if(s.isBlank) failure("Number regex read nothing. Input did not start with digits.")
    else success(s.toLong)
  }

  val A: Parser[Note] = noteParser(Note.A)
  val B: Parser[Note] = noteParser(Note.B)
  val C: Parser[Note] = noteParser(Note.C)
  val D: Parser[Note] = noteParser(Note.D)
  val E: Parser[Note] = noteParser(Note.E)
  val F: Parser[Note] = noteParser(Note.F)
  val G: Parser[Note] = noteParser(Note.G)

  val maj: Parser[Scale] = (anyCase(Scale.Major.encoding) ||| "".r).map { _ => Scale.Major }
  val min: Parser[Scale] = anyCase(Scale.Minor.encoding).map { _ => Scale.Minor }

  val sharp: Parser[Accidental] = anyCase(Accidental.Sharp.encoding).map { _ => Accidental.Sharp }
  val flat: Parser[Accidental] = anyCase(Accidental.Flat.encoding).map { _ => Accidental.Flat }
  val noAcc: Parser[Accidental] = "".r.map { _ => Accidental.None }

  val note: Parser[Note] = A ||| B ||| C ||| D ||| E ||| F ||| G
  val scale: Parser[Scale] = min ||| maj
  val accidental: Parser[Accidental] = sharp ||| flat ||| noAcc

  val bpm: Parser[String] = whiteSpace.* ~> anyCase("bpm")

  val key: Parser[KeyMetadata] = for {
    _     <- whiteSpace.*
    note  <- note
    _     <- whiteSpace.*
    acc   <- accidental
    _     <- whiteSpace.*
    scale <- scale
  } yield KeyMetadata(note, acc, scale)

  val tempo: Parser[TempoMetadata] = for {
    _   <- whiteSpace.*
    num <- number
    _   <- whiteSpace.*
    _   <- bpm
  } yield TempoMetadata(num)

  val label: Parser[LabelMetadata] = for {
    _           <- whiteSpace.*
    notDelimiter = (c: Char) => !delimiters.contains(c)
    chars       <- acceptIf(notDelimiter)(_.toString).*
  } yield LabelMetadata(chars.mkString(""))

  val makerToken: Parser[Token[LabelMetadata]] = tokenParser(Token.Maker)
  val nameToken: Parser[Token[LabelMetadata]] = tokenParser(Token.Name)
  val keyToken: Parser[Token[KeyMetadata]] = tokenParser(Token.Key)
  val tempoToken: Parser[Token[TempoMetadata]] = tokenParser(Token.Tempo)

  val rule: Parser[Rule] =
      makerToken.map { token => asRule(token, label) } |||
      nameToken.map { token => asRule(token, label) } |||
      keyToken.map { token => asRule(token, key) } |||
      tempoToken.map { token => asRule(token, tempo) }

  val grammar: Parser[List[Rule]] = rule.*

  def run[A](parser: Parser[A], input: String): ParsingResult[A] = parse(parser, input) match {
    case Success(result, _) => ParsingResult.Success(result)
    case Error(msg, _) => ParsingResult.Failure(msg)
    case Failure(msg, _) => ParsingResult.Failure(msg)
  }
}