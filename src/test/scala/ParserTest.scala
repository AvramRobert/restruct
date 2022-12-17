import parser.*

import java.io.File
import scala.util.parsing.combinator.*
class ParserTest extends munit.FunSuite {

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

  test("Can parse bpm literal") {
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

  test("can parse labels") {
    testParser(
      data = Map(
        "first " -> LabelMetadata("first"),
        "first-" -> LabelMetadata("first"),
        "first_" -> LabelMetadata("first"),
        " first " -> LabelMetadata("first")
      ),
      parser = Parsing.label
    )
  }

  test("Can parse a list of rules (grammar)") {
    testParser(
      data = Map(
        "<maker> <name> <tempo> <key>" -> List(
          Rule.ParsingRule(Token.Maker, Parsing.label),
          Rule.ParsingRule(Token.Name, Parsing.label),
          Rule.ParsingRule(Token.Tempo, Parsing.tempo),
          Rule.ParsingRule(Token.Key, Parsing.key)
        )
      ),
      parser = Parsing.grammar
    )
  }

  test("Can parse input based on dynamic grammar") {
    val grammar = Parsing.run(Parsing.grammar, "<tempo> <maker> <maker> <maker> <key>").success.get
    testParser(
      data = Map(
        "86bpm brave new world C#maj" -> List(
          Metadata.ParsingMetadata(Token.Tempo, TempoMetadata(86)),
          Metadata.ParsingMetadata(Token.Maker, LabelMetadata("brave")),
          Metadata.ParsingMetadata(Token.Maker, LabelMetadata("new")),
          Metadata.ParsingMetadata(Token.Maker, LabelMetadata("world")),
          Metadata.ParsingMetadata(Token.Key, KeyMetadata(Note.C, Accidental.Sharp, Scale.Major)))
      ),
      parser = Parsing.fromGrammar(grammar)
    )
  }


  test("Can parse path arguments") {
    testParser(
      data = Map(
        "--path=/home/my.self/this" -> File("/home/my.self/this").toPath,
        "--path=/home/`this is a path with spaces`/what" -> File("/home/`this is a path with spaces`/what").toPath,
        "--path=/`start here`/`because it's`/as.easy/as/`you'd like`" -> File("/`start here`/`because it's`/as.easy/as/`you'd like`").toPath,
        "--path=this/should/`stop after`/this hello" -> File("this/should/`stop after`/this").toPath

      ),
      parser = Parsing.pathArg
    )
  }

  test("Can parse pattern arguments") {
    testParser(
      data = Map(
        "--pattern=<maker> <name> <key> <tempo>" -> List(
          Rule.ParsingRule(Token.Maker, Parsing.label),
          Rule.ParsingRule(Token.Name, Parsing.label),
          Rule.ParsingRule(Token.Key, Parsing.key),
          Rule.ParsingRule(Token.Tempo, Parsing.tempo)
        )
      ),
      parser = Parsing.grammarArg
    )
  }

  test("Can parse cli arguments") {
    testParser(
      data = Map(
        "--pattern=<name> <key> --path=C:/this/is/some/path" -> CliArguments(
          path = File("C:/this/is/some/path").toPath,
          grammar = List(Rule.ParsingRule(Token.Name, Parsing.label), Rule.ParsingRule(Token.Key, Parsing.key))
        )
      ),
      parser = Parsing.cliArguments
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
