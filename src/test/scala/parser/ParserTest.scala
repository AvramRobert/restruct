package parser

import parser.*

import scala.util.parsing.combinator.*
class ParserTest extends munit.FunSuite {

  test("Parses until another parser is successful") {
    testParser(
      data = Map(
        "hello   1" -> "hello",
        "     this is a complete sentence25" -> "this is a complete sentence",
        "    i can have blanks anywhere    24" -> "i can have blanks anywhere"
      ),
      parser = Parsing.until(Parsing.number)
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
      parser = Parsing.note
    )
  }

  test("Can parse scales") {
    testParser(
      data = Map(
        "min" -> Scale.Minor,
        "maj" -> Scale.Major,
        "" -> Scale.Major
      ),
      parser = Parsing.scale
    )
  }

  test("Can parse accidentals") {
    testParser(
      data = Map(
        "#" -> Accidental.Sharp,
        "♭" -> Accidental.Flat,
        "" -> Accidental.None
      ),
      parser = Parsing.accidental
    )
  }

  test("Can parse keys") {
    val singular = Map(
      "A" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "A   " -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "    A" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "   A   " -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
    )
    val scaled = Map(
      "AMin" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Minor),
      "Amaj" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "Amaj    " -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "    Amaj" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "  A Maj" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
      "A maj" -> KeyMetadata(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
    )
    val accidentalSingular = Map(
      "A#" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "   A#" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A#    " -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "   A#    " -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
    )

    val accidentalScaled = Map(
      "A#maj" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "  A#maj" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A#maj     " -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A# maj" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "  A# maj" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "A # maj" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major),
      "  A # maj" -> KeyMetadata(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major)
    )
    testParser(
      data = singular ++ scaled ++ accidentalSingular ++ accidentalScaled,
      parser = Parsing.key
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
      parser = Parsing.bpm
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
      parser = Parsing.number
    )
  }

  test("Can parse tempo") {
    testParser(
      data = Map(
        "120bpm" -> TempoMetadata(120),
        "   120bpm" -> TempoMetadata(120),
        "120bpm    " -> TempoMetadata(120),
        "   120bpm" -> TempoMetadata(120),
        "120    bpm" -> TempoMetadata(120),
        "    120    bpm" -> TempoMetadata(120),
        "120    bpm    " -> TempoMetadata(120),
        "   120    bpm   " -> TempoMetadata(120)
      ),
      parser = Parsing.tempo
    )
  }

  test("can parse file names") {
    testParser(
      data = Map(
        "file-name E maj 125 bpm" -> FileMetadata(title = TitleMetadata("file-name"), key = KeyMetadata(Note.E, Accidental.None, Scale.Major), TempoMetadata(125))
      ),
      parser = Parsing.fileName
    )
  }

  test("Can parse a list of rules (grammar)") {
    testParser(
      data = Map(
        "%tempo% %title% %key%" -> List(
          Rule.ParsingRule(Token.Tempo, Parsing.tempo),
          Rule.ParsingRule(Token.Title, Parsing.title),
          Rule.ParsingRule(Token.Key, Parsing.key)
        )
      ),
      parser = Parsing.grammar
    )
  }

  test("Can parse input based on dynamic grammar") {
    val grammar = Parsing.run(Parsing.grammar, "%tempo% %title% %key%").success.get
    testParser(
      data = Map(
        "86bpm my world is yours C#maj" -> List(
          Emission.ParsingEmission(Token.Tempo, TempoMetadata(86)),
          Emission.ParsingEmission(Token.Title, TitleMetadata("my world is yours")),
          Emission.ParsingEmission(Token.Key, KeyMetadata(Note.C, Accidental.Sharp, Scale.Major)))
      ),
      parser = Parsing.fromGrammar(grammar)
    )
  }

  private def testParser[A](data: Map[String, A], parser: Parsing.Parser[A]): Unit = {
    val actual = data
      .map { case (input, expected) => Parsing.run(parser, input) -> expected }

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
