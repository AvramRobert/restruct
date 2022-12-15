package parser

import parser.*
import parser.ParsedElem.TypedElem
import parser.Token.TypedToken

import scala.util.parsing.combinator.*
class ParserTest extends munit.FunSuite {

  test("Parses until another parser is successful") {
    testParser(
      data = Map(
        "hello   1" -> "hello",
        "     this is a complete sentence25" -> "this is a complete sentence",
        "    i can have blanks anywhere    24" -> "i can have blanks anywhere"
      ),
      parser = Parser.until(Parser.number)
    )
  }

  test("Can parse notes") {
    testParser(
      data = Map(
        "A" -> Note.A,
        "B" -> Note.B,
        "C" -> Note.C,
        "D" -> Note.D,
        "E" -> Note.E,
        "F" -> Note.F,
        "G" -> Note.G
      ),
      parser = Parser.note
    )
  }

  test("Can parse scales") {
    testParser(
      data = Map(
        "min" -> Scale.Minor,
        "maj" -> Scale.Major,
        "" -> Scale.Major
      ),
      parser = Parser.scale
    )
  }

  test("Can parse accidentals") {
    testParser(
      data = Map(
        "#" -> Accidental.Sharp,
        "♭" -> Accidental.Flat,
        "" -> Accidental.None
      ),
      parser = Parser.accidental
    )
  }

  test("Can parse keys") {
    val singular = Map(
      "A" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "A   " -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "    A" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "   A   " -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
    )
    val scaled = Map(
      "AMin" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Minor),
      "Amaj" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "Amaj    " -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "    Amaj" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "  A Maj" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "A maj" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
    )
    val accidentalSingular = Map(
      "A#" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "   A#" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A#    " -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "   A#    " -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
    )

    val accidentalScaled = Map(
      "A#maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "  A#maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A#maj     " -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A# maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "  A# maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A # maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "  A # maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major)
    )
    testParser(
      data = singular ++ scaled ++ accidentalSingular ++ accidentalScaled,
      parser = Parser.key
    )
  }

  test("Can parse bpm") {
    testParser(
      data = Map(
        "bpm" -> "bpm",
        "     bpm" -> "bpm",
        "bpm    " -> "bpm",
        "   bpm   " -> "bpm"
      ),
      parser = Parser.bpm
    )
  }

  test("Can parse string numbers") {
    testParser(
      data = Map(
        "1" -> 1,
        "42" -> 42,
        "602" -> 602,
        "1052" -> 1052
      ),
      parser = Parser.number
    )
  }

  test("Can parse tempo") {
    testParser(
      data = Map(
        "120bpm" -> Tempo(120),
        "   120bpm" -> Tempo(120),
        "120bpm    " -> Tempo(120),
        "   120bpm" -> Tempo(120),
        "120    bpm" -> Tempo(120),
        "    120    bpm" -> Tempo(120),
        "120    bpm    " -> Tempo(120),
        "   120    bpm   " -> Tempo(120)
      ),
      parser = Parser.tempo
    )
  }

  test("can parse file names") {
    testParser(
      data = Map(
        "file-name E maj 125 bpm" -> FileData(title = "file-name", key = Key(Note.E, Accidental.None, Scale.Major), Tempo(125))
      ),
      parser = Parser.fileName
    )
  }

  test("Can parse grammar rules") {
    testParser(
      data = Map(
        "%title%" -> Rule.TitleRule,
        "%key%" -> Rule.KeyRule,
        "%tempo%" -> Rule.TempoRule
      ),
      parser = Parser.rule
    )
  }

  test("Can parse a file name grammar") {
    testParser(
      data = Map(
        "%tempo% %title% %title% %key%" -> List(Rule.TempoRule, Rule.TitleRule, Rule.TitleRule, Rule.KeyRule)
      ),
      parser = Parser.grammar
    )
  }

  test("Can parse a typed grammar") {
    testParser(
      data = Map(
        "%tempo% %title% %key%" -> List(TypedToken(GrammarRef.TempoRef), TypedToken(GrammarRef.TitleRef), TypedToken(GrammarRef.KeyRef))
      ),
      parser = Parser.tokens
    )
  }

  test("Can parse a based on dynamic grammar") {
    val tokens = Parser.run(Parser.tokens, "%tempo% %title% %key%").success.get
    testParser(
      data = Map(
        "86bpm my world is yours C#maj" -> List(TypedElem(Tempo(86)), TypedElem("my world is yours"), TypedElem(Key(Note.C, Accidental.Sharp, Scale.Major)))
      ),
      parser = Parser.grammarParser(tokens)
    )
  }

  private def testParser[A](data: Map[String, A], parser: Parser.Parser[A]): Unit = {
    val actual = data
      .map { case (input, expected) => Parser.run(parser, input) -> expected }

    val failed =
      actual
        .keys
        .filter { result => result.isFailure }
        .map { result => result.failure.get }

    val succeeded =
      actual
        .filter { case (actual, _) => actual.isSuccess }
        .map { case (actual, expected) => actual.success.get -> expected }

    if (failed.nonEmpty) fail(s"Could not convert. Reason: \n${failed.mkString("\n")}\n")
    else succeeded.foreach { case (actual, expected) => assertEquals(actual, expected) }
  }
}
