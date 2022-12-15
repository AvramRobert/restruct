package parser
import parser.Emission.ParsingEmission
import parser.ParsingResult.Failure

import scala.annotation.tailrec
import scala.util.parsing.combinator.*

object Parsing extends RegexParsers {
  private def asRule[A](token: Token[A], parser: Parser[A]): Rule = Rule.ParsingRule(token, parser)
  private def noteParser(note: Note): Parser[Note] = s"[${note.encoding}]".r.map { _ => note }

  def until[A](p: Parser[A]): Parser[String] = Parser { input =>
    val start = input.pos.column - 1

    @tailrec
    def consume(in: Input, offset: Int = start): ParseResult[String] = {
      p(in) match {
        case Failure(_, _) => consume(in.rest, offset + 1)
        case Error(_, _) => consume(in.rest, offset + 1)
        case Success(_, _) => Success(input.source.subSequence(start, offset).toString.trim, in)
      }
    }

    consume(input)
  }

  def fromGrammar(grammar: Grammar): Parser[Output] = Parser { input =>
    @tailrec
    def unravel(rules: Grammar, emissions: Output = List.empty, rem: Input = input): ParseResult[Output] = rules match {
      case Rule.ParsingRule(token, parser) :: ts => parser(rem) match {
        case Success(result, next) => unravel(ts, Emission.ParsingEmission(token, result) +: emissions, next)
        case Error(msg, next) => Error(msg, next)
        case Failure(msg, next) => Failure(msg, next)
      }
      case Nil => Success(emissions.reverse, rem)
    }

    unravel(grammar)
  }

  def anyCase(value: String): Parser[String] = s"(?i)($value)".r

  val percent: Parser[Char] = s"(\\%)".r.map { c => c.head }

  val blank: Parser[Unit] = " ".r.map { _ => () }

  val number: Parser[Long] = s"[0-9]*".r.flatMap { s =>
    if(s.isBlank) failure("Number regex read nothing. Input did not start with digits.")
    else success(s.toLong)
  }

  def token(ref: String): Parser[String] = for {
    _ <- percent
    r <- anyCase(ref)
    _ <- percent
  } yield r

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

  val bpm: Parser[String] = anyCase("bpm")

  val key: Parser[KeyMetadata] = for {
    _     <- blank.*
    note  <- note
    _     <- blank.*
    acc   <- accidental
    _     <- blank.*
    scale <- scale
  } yield KeyMetadata(note, acc, scale)

  val tempo: Parser[TempoMetadata] = for {
    _   <- blank.*
    num <- number
    _   <- blank.*
    _   <- bpm
  } yield TempoMetadata(num)

  val title: Parser[TitleMetadata] = for {
    _     <- blank.*
    label <- until(key)
  } yield TitleMetadata(label)

  val fileName: Parser[FileMetadata] = for {
    title <- title
    key   <- key
    tempo <- tempo
  } yield FileMetadata(title, key, tempo)

  val titleToken: Parser[Token[TitleMetadata]] = token(Token.Title.ref).map { _ => Token.Title }

  val keyToken: Parser[Token[KeyMetadata]] = token(Token.Key.ref).map { _ => Token.Key }

  val tempoToken: Parser[Token[TempoMetadata]] = token(Token.Tempo.ref).map { _ => Token.Tempo }

  val rule: Parser[Rule] =
    titleToken.map { token => asRule(token, title) } |||
      keyToken.map { token => asRule(token, key) } |||
      tempoToken.map { token => asRule(token, tempo) }

  val grammar: Parser[Grammar] = rule.*
  
  def run[A](parser: Parser[A], input: String): ParsingResult[A] = parse(parser, input) match {
    case Success(result, _) => ParsingResult.Success(result)
    case Error(msg, _) => ParsingResult.Failure(msg)
    case Failure(msg, _) => ParsingResult.Failure(msg)
  }
}