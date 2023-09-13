import data.Emission.ParsingEmission
import data.{NamingToken, *}
import data.NamingToken.*

import java.io.File
import scala.annotation.tailrec
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object Parsing extends RegexParsers {
  override def skipWhitespace: Boolean = false

  private val delimiters = Set(' ', '-', '_', '/')

  private def literalIf(p: Elem => Boolean): Parser[String] = acceptIf(p)(_.toString).*.map(_.mkString(""))

  private def noteParser(note: Note): Parser[Note] = literal(note.encoding).map { _ => note }

  private def tokenParser[A](t: NamingToken[A]): Parser[NamingToken[A]] = for {
    _ <- whiteSpace.*
    _ <- lt
    _ <- whiteSpace.*
    _ <- anyCase(t.ref)
    _ <- whiteSpace.*
    _ <- gt
  } yield t

  private def parserFor[A](token: NamingToken[A]): Parser[A] = token match {
    case KeyToken => key
    case MakerToken => label
    case NameToken => label
    case TempoToken => tempo
    case ExtensionToken => extension
  }

  def fromPatterns(patterns: List[Pattern]): Parser[List[Emission]] = Parser { input =>
    @tailrec
    def unravel(rules: List[Pattern], data: List[Emission] = List.empty, rem: Input = input): ParseResult[List[Emission]] = rules match {
      case Pattern.TokenPattern(token) :: ps => parserFor(token)(rem) match {
        case Success(result, next) => unravel(ps, ParsingEmission(token, result) +: data, next)
        case Error(msg, next) => Error(msg, next)
        case Failure(msg, next) => Failure(msg, next)
      }
      case Nil => Success(data.reverse, rem)
    }

    unravel(patterns)
  }

  private def anyCase(value: String): Parser[String] = s"(?i)($value)".r

  val any: Parser[String] = ".".r

  val tick: Parser[String] = literal("`")

  val lt: Parser[String] = literal("<")

  val gt: Parser[String] = literal(">")

  val slash: Parser[String] = literal("/")

  def arg(argument: Argument): Parser[String] = literal(s"--${argument.ref}=")

  val number: Parser[Long] = s"[0-9]*".r.flatMap { s =>
    if (s.isBlank) failure("Number regex read nothing. Input did not start with digits.")
    else success(s.toLong)
  }

  val mp3: Parser[Extension] = literal(".mp3").map(_ => Extension.MP3)
  val wav: Parser[Extension] = literal(".wav").map(_ => Extension.WAV)
  val flacc: Parser[Extension] = literal(".flacc").map(_ => Extension.FLACC)

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
  val noAcc: Parser[Accidental] = "".r.map { _ => Accidental.Natural }

  val note: Parser[Note] = A ||| B ||| C ||| D ||| E ||| F ||| G
  val scale: Parser[Scale] = min ||| maj
  val accidental: Parser[Accidental] = sharp ||| flat ||| noAcc

  val bpm: Parser[String] = whiteSpace.* ~> anyCase("bpm")

  val key: Parser[Key] = for {
    _     <- whiteSpace.*
    note  <- note
    _     <- whiteSpace.*
    acc   <- accidental
    _     <- whiteSpace.*
    scale <- scale
  } yield Key(note, acc, scale)

  val tempo: Parser[Tempo] = for {
    _   <- whiteSpace.*
    num <- number
    _   <- whiteSpace.*
    _   <- bpm
  } yield Tempo(num)

  val label: Parser[Label] = for {
    _     <- whiteSpace.*
    label <- literalIf { c => !delimiters.contains(c) }
  } yield Label(label)

  val extension: Parser[Extension] = for {
    _   <- whiteSpace.*
    ext <- mp3 ||| wav ||| flacc
  } yield ext

  val makerToken: Parser[NamingToken[Label]] = tokenParser(MakerToken)
  val nameToken: Parser[NamingToken[Label]] = tokenParser(NameToken)
  val keyToken: Parser[NamingToken[Key]] = tokenParser(KeyToken)
  val tempoToken: Parser[NamingToken[Tempo]] = tokenParser(TempoToken)

  val pattern: Parser[Pattern] =
    makerToken.map(Pattern.TokenPattern(_)) |||
      nameToken.map(Pattern.TokenPattern(_)) |||
      keyToken.map(Pattern.TokenPattern(_)) |||
      tempoToken.map(Pattern.TokenPattern(_))

  val pathPattern: Parser[Pattern] = slash.? ~> pattern

  private val escapedFolder: Parser[String] = for {
    _       <- tick
    content <- literalIf { _ != '`' }
    _       <- tick
  } yield s"`$content`"

  private def subFolder: Parser[String] = for {
    right  <- literalIf { c => c != '`' && c != ' ' }
    middle <- escapedFolder.?
    left   <- middle match {
      case Some(p) => subFolder.map { r => p + r }
      case None => success("")
    }
  } yield right + left

  val dirArg: Parser[File] = for {
    _    <- arg(Argument.Dir)
    path <- subFolder
  } yield File(path.mkString(""))

  val filePatternArg: Parser[List[Pattern]] = for {
    _     <- arg(Argument.FilePattern)
    rules <- pattern.*
  } yield rules

  val dirStructureArg: Parser[List[Pattern]] = for {
    _       <- arg(Argument.DirStructure)
    pattern <- pathPattern.*
  } yield pattern

  val renamePatternArg: Parser[List[Pattern]] = for {
    _     <- arg(Argument.RenamePattern)
    rules <- pattern.*
  } yield rules

  // The arguments have to be sorted when read
  val cliArguments: Parser[CliArguments] = for {
    filePattern   <- filePatternArg
    _             <- whiteSpace.*
    dir           <- dirArg
    _             <- whiteSpace.*
    dirStructure  <- dirStructureArg
    _             <- whiteSpace.*
    renamePattern <- renamePatternArg
  } yield CliArguments(dir, filePattern, dirStructure, renamePattern)

  def run[A](parser: Parser[A], input: String): Try[A] = parse(parser, input) match {
    case Success(result, _) => scala.util.Success(result)
    case Error(msg, _) => scala.util.Failure(Throwable(msg))
    case Failure(msg, _) => scala.util.Failure(Throwable(msg))
  }
}
