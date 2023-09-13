import data.*
import data.Pattern.TokenPattern
import data.NamingToken.*

import java.io.File
import scala.util.parsing.combinator.*
class ParsingTest extends munit.FunSuite {

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
        "b" -> Accidental.Flat,
        "" -> Accidental.Natural
      ),
      parser = Parsing.accidental
    )
  }

  test("Can parse keys") {
    val singular = Map(
      "A" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "A   " -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "    A" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "   A   " -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
    )
    val scaled = Map(
      "AMin" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Minor),
      "Amaj" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "Amaj    " -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "    Amaj" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "  A Maj" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
      "A maj" -> Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major),
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
        "120bpm" -> Tempo(120),
        "   120bpm" -> Tempo(120),
        "120bpm    " -> Tempo(120),
        "   120bpm" -> Tempo(120),
        "120    bpm" -> Tempo(120),
        "    120    bpm" -> Tempo(120),
        "120    bpm    " -> Tempo(120),
        "   120    bpm   " -> Tempo(120)
      ),
      parser = Parsing.tempo
    )
  }

  test("can parse labels") {
    testParser(
      data = Map(
        "first " -> Label("first"),
        "first-" -> Label("first"),
        "first_" -> Label("first"),
        " first " -> Label("first"),
        "first/" -> Label("first")
      ),
      parser = Parsing.label
    )
  }

  test("Can parse extensions") {
    testParser(
      data = Map(
        ".mp3" -> Extension.MP3,
        "        .mp3" -> Extension.MP3,
        ".wav" -> Extension.WAV,
        "        .wav" -> Extension.WAV,
        ".flacc" -> Extension.FLACC,
        "        .flacc" -> Extension.FLACC
      ),
      parser = Parsing.extension
    )
  }

  test("Can parse maker patterns") {
    testParser(
      data = Map(
        "<maker>" -> TokenPattern(MakerToken),
        "   <maker>" -> TokenPattern(MakerToken),
        "<maker>   " -> TokenPattern(MakerToken)
      ),
      parser = Parsing.pattern
    )
  }

  test("Can parse name patterns") {
    testParser(
      data = Map(
        "<name>" -> TokenPattern(NameToken),
        "   <name>" -> TokenPattern(NameToken),
        "<name>   " -> TokenPattern(NameToken)
      ),
      parser = Parsing.pattern
    )
  }

  test("Can parse key patterns") {
    testParser(
      data = Map(
        "<key>" -> TokenPattern(KeyToken),
        "   <key>" -> TokenPattern(KeyToken),
        "<key>   " -> TokenPattern(KeyToken)
      ),
      parser = Parsing.pattern
    )
  }

  test("Can parse tempo patterns") {
    testParser(
      data = Map(
        "<tempo>" -> TokenPattern(TempoToken),
        "   <tempo>" -> TokenPattern(TempoToken),
        "<tempo>   " -> TokenPattern(TempoToken)
      ),
      parser = Parsing.pattern
    )
  }

  test("Can parse list of patterns") {
    testParser(
      data = Map(
        "<maker> <name> <tempo> <key>" -> List(
          TokenPattern(MakerToken),
          TokenPattern(NameToken),
          TokenPattern(TempoToken),
          TokenPattern(KeyToken)
        )
      ),
      parser = Parsing.pattern.*
    )
  }

  test("Can parse input based on dynamic pattern") {
    val patterns = Parsing.run(Parsing.pattern.*, "<tempo> <maker> <maker> <maker> <key>").get
    testParser(
      data = Map(
        "86bpm brave new world C#maj" -> List(
          Emission.ParsingEmission(TempoToken, Tempo(86)),
          Emission.ParsingEmission(MakerToken, Label("brave")),
          Emission.ParsingEmission(MakerToken, Label("new")),
          Emission.ParsingEmission(MakerToken, Label("world")),
          Emission.ParsingEmission(KeyToken, Key(Note.C, Accidental.Sharp, Scale.Major)))
      ),
      parser = Parsing.fromPatterns(patterns)
    )
  }

  test("Can parse dir arguments") {
    testParser(
      data = Map(
        "--dir=/home/my.self/this" -> File("/home/my.self/this"),
        "--dir=/home/`this is a path with spaces`/what" -> File("/home/`this is a path with spaces`/what"),
        "--dir=/`start here`/`because it's`/as.easy/as/`you'd like`" -> File("/`start here`/`because it's`/as.easy/as/`you'd like`"),
        "--dir=this/should/`stop after`/this hello" -> File("this/should/`stop after`/this")
      ),
      parser = Parsing.dirArg
    )
  }

  test("Can parse file pattern arguments") {
    testParser(
      data = Map(
        "--file-pattern=<maker> <name> <key> <tempo>" -> List(
          TokenPattern(MakerToken),
          TokenPattern(NameToken),
          TokenPattern(KeyToken),
          TokenPattern(TempoToken)
        )
      ),
      parser = Parsing.filePatternArg
    )
  }

  test("Can parse dir structure arguments") {
    testParser(
      data = Map(
        "--dir-structure=<maker>/<name>/<key>/<tempo>" -> List(
          TokenPattern(MakerToken),
          TokenPattern(NameToken),
          TokenPattern(KeyToken),
          TokenPattern(TempoToken)
        ),
        "--dir-structure=/<maker>/<name>/<key>/<tempo>" -> List(
          TokenPattern(MakerToken),
          TokenPattern(NameToken),
          TokenPattern(KeyToken),
          TokenPattern(TempoToken)
        ),
      ),
      parser = Parsing.dirStructureArg
    )
  }

  test("Can parse rename pattern arguments") {
    testParser(
      data = Map(
        "--rename-pattern=<maker> <name> <key> <tempo>" -> List(
          TokenPattern(MakerToken),
          TokenPattern(NameToken),
          TokenPattern(KeyToken),
          TokenPattern(TempoToken)
        )
      ),
      parser = Parsing.renamePatternArg
    )
  }

  test("Can parse cli arguments") {
    testParser(
      data = Map(
        "--file-pattern=<name> <key> --dir=C:/this/is/some/path --dir-structure=<name>/<key> --rename-pattern=<key> <name>" -> CliArguments(
          directory = File("C:/this/is/some/path"),
          filePattern = List(TokenPattern(NameToken), TokenPattern(KeyToken)),
          dirStructure = List(TokenPattern(NameToken), TokenPattern(KeyToken)),
          renamePattern = List(TokenPattern(KeyToken), TokenPattern(NameToken))
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
        .map { result => result.get }

    val succeeded =
      actual
        .filter { case (actual, _) => actual.isSuccess }
        .map { case (actual, expected) => actual.get -> expected }

    if (failed.nonEmpty) fail(s"Could not convert. Reason: \n${failed.mkString("\n")}\n")
    else succeeded.foreach { case (actual, expected) => assertEquals(actual, expected) }
  }
}
