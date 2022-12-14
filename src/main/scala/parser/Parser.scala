package parser

import parser.ParsingResult.Failure
import scala.annotation.tailrec
import scala.util.parsing.combinator.*

case class Tempo(bpm: String)

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

case class Key(note: Note, accidental: Accidental, scale: Scale)

object Parser extends RegexParsers {
  private def noteParser(note: Note): Parser[Note] = s"[${note.encoding}]".r.map { _ => note }

  val emptyString: Parser[Unit] = " ".r.map { _ => () }

  def anyCase(value: String): Parser[String] = s"(?i)($value)".r

  val stringNumber: Parser[String] = s"[0-9]*".r
  
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
    _     <- emptyString.*
    note  <- note
    _     <- emptyString.*
    acc   <- accidental
    _     <- emptyString.*
    scale <- scale
  } yield Key(note, acc, scale)

  val tempo: Parser[Tempo] = for {
    _ <- emptyString.*
    num <- stringNumber
    _ <- emptyString.*
    _ <- bpm
  } yield Tempo(num)

  def run[A](parser: Parser[A], input: String): ParsingResult[A] = parse(parser, input) match {
    case Success(result, _) => ParsingResult.Success(result)
    case Error(msg, _) => ParsingResult.Failure(msg)
    case Failure(msg, _) => ParsingResult.Failure(msg)
  }
}