import ParsingResult.Failure

import scala.annotation.tailrec
import scala.util.parsing.combinator.*

enum ParsingResult[+A]:
  case Success(result: A) extends ParsingResult[A]
  case Failure(message: String) extends ParsingResult[Nothing]

extension [A] (result: ParsingResult[A])
  def isFailure: Boolean = result match {
    case Failure(_) => true
    case _ => false
  }
  def isSuccess: Boolean = !isFailure
  def failure: Option[String] = result match {
    case Failure(msg) => Some(msg)
    case _ => None
  }
  def success: Option[A] = result match {
    case ParsingResult.Success(result) => Some(result)
    case _ => None
  }

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

val allNotes: List[Key] = for {
  note       <- Note.values.toList
  scale      <- Scale.values.toList
  accidental <- Accidental.values.toList
} yield Key(note, accidental, scale)

object KeyParser extends RegexParsers {
  def A: Parser[Note] = noteParser(Note.A)
  def B: Parser[Note] = noteParser(Note.B)
  def C: Parser[Note] = noteParser(Note.C)
  def D: Parser[Note] = noteParser(Note.D)
  def E: Parser[Note] = noteParser(Note.E)
  def F: Parser[Note] = noteParser(Note.F)
  def G: Parser[Note] = noteParser(Note.G)

  def maj: Parser[Scale] = s"(${Scale.Major.encoding})".r.map { _ => Scale.Major } ||| "".r.map { _ => Scale.Major }
  def min: Parser[Scale] = s"(${Scale.Minor.encoding})".r.map { _ => Scale.Minor }

  def sharp: Parser[Accidental] = s"(${Accidental.Sharp.encoding})".r.map { _ => Accidental.Sharp }
  def flat: Parser[Accidental] = s"(${Accidental.Flat.encoding})".r.map { _ => Accidental.Flat }
  def noAcc: Parser[Accidental] = "".r.map { _ => Accidental.None }

  def note: Parser[Note] = A ||| B ||| C ||| D ||| E ||| F ||| G
  def scale: Parser[Scale] = min ||| maj
  def accidental: Parser[Accidental] = sharp ||| flat ||| noAcc

  def key: Parser[Key] = for {
    note <- note
    acc <- accidental
    scale <- scale
  } yield Key(note, acc, scale)

  def run[A](parser: Parser[A], input: String): ParsingResult[A] = parse(parser, input) match {
    case Success(result, _) => ParsingResult.Success(result)
    case Error(msg, _) => ParsingResult.Failure(msg)
    case Failure(msg, _) => ParsingResult.Failure(msg)
  }
  
  def parserFrom[A](parser: Parser[A]): String => ParsingResult[A] = input => run(parser, input)
  
  private def noteParser(note: Note): Parser[Note] = s"[${note.encoding}]".r.map { _ => note }
}