package parser

import parser.ParsingResult.Failure
import parser.Rule.TitleRule

import scala.annotation.tailrec
import scala.util.parsing.combinator.*

enum Rule(val reference: String):
  case TitleRule extends Rule("title")
  case KeyRule extends Rule("key")
  case TempoRule extends Rule("tempo")

enum Note(val encoding: String):
  case A extends Note("A")
  case B extends Note("B")
  case C extends Note("C")
  case D extends Note("D")
  case E extends Note("E")
  case F extends Note("F")
  case G extends Note("G")

enum Scale(val encoding: String):
  case Major extends Scale("maj")
  case Minor extends Scale("min")

enum Accidental(val encoding: String):
  case None extends Accidental("")
  case Flat extends Accidental("♭")
  case Sharp extends Accidental("#")

case class Key(note: Note, accidental: Accidental, scale: Scale) {
  val encoding: String = s"$note$accidental$scale"
}

case class Tempo(bpm: Long) {
  val encoding: String = s"${bpm.toFloat.floor.toLong}s bpm"
}

case class FileData(title: String, key: Key, tempo: Tempo)

object Parser extends RegexParsers {
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

  def anyCase(value: String): Parser[String] = s"(?i)($value)".r

  val percent: Parser[Char] = s"(\\%)".r.map { c => c.head }

  val blank: Parser[Unit] = " ".r.map { _ => () }

  val number: Parser[Long] = s"[0-9]*".r.flatMap { s =>
    if(s.isBlank) failure("Number regex read nothing. Input did not start with digits.")
    else success(s.toLong)
  }

  val word: Parser[String] = ".+?( |-)".r

  def grammarToken(ref: String): Parser[String] = for {
    _ <- percent
    r <-  anyCase(ref)
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

  val key: Parser[Key] = for {
    _     <- blank.*
    note  <- note
    _     <- blank.*
    acc   <- accidental
    _     <- blank.*
    scale <- scale
  } yield Key(note, acc, scale)

  val tempo: Parser[Tempo] = for {
    _ <- blank.*
    num <- number
    _ <- blank.*
    _ <- bpm
  } yield Tempo(num)

  val title: Parser[String] = for {
    _ <- blank.*
    title <- until(key)
  } yield title

  val fileName: Parser[FileData] = for {
    title <- title
    key   <- key
    tempo <- tempo
  } yield FileData(title, key, tempo)

  val titleRule: Parser[Rule] = grammarToken(Rule.TitleRule.reference).map { _ => Rule.TitleRule }
  val keyRule: Parser[Rule] = grammarToken(Rule.KeyRule.reference).map { _ => Rule.KeyRule }
  val tempoRule: Parser[Rule] = grammarToken(Rule.TempoRule.reference).map { _ => Rule.TempoRule }

  val rule: Parser[Rule] = titleRule ||| keyRule ||| tempoRule

  val grammar: Parser[List[Rule]] = rule.*

  def run[A](parser: Parser[A], input: String): ParsingResult[A] = parse(parser, input) match {
    case Success(result, _) => ParsingResult.Success(result)
    case Error(msg, _) => ParsingResult.Failure(msg)
    case Failure(msg, _) => ParsingResult.Failure(msg)
  }
}