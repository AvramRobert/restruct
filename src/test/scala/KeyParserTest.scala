import scala.util.parsing.combinator._
class KeyParserTest extends munit.FunSuite {
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
      parser = KeyParser.note
    )
  }

  test("Can parse scales") {
    testParser(
      data = Map(
        "min" -> Scale.Minor,
        "maj" -> Scale.Major,
        "" -> Scale.Major
      ),
      parser = KeyParser.scale
    )
  }

  test("Can parse accidentals") {
    testParser(
      data = Map(
        "#" -> Accidental.Sharp,
        "♭" -> Accidental.Flat,
        "" -> Accidental.None
      ),
      parser = KeyParser.accidental
    )
  }

  test("Can parse keys") {
    testParser(
      data = Map(
        "Amaj" -> Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major),
        "A#maj" -> Key(note = Note.A, accidental = Accidental.Sharp, scale = Scale.Major)
      ),
      parser = KeyParser.key
    )
  }

  private def testParser[A](data: Map[String, A], parser: KeyParser.Parser[A]): Unit = {
    val actual = data
      .map { case (input, expected) => KeyParser.run(parser, input) -> expected }

    val failed =
      actual
        .keys
        .filter { result => result.isFailure }
        .map { result => result.failure.get }

    val succeeded =
      actual
        .filter { case (actual, _) => actual.isSuccess }
        .map { case (actual, expected) => actual.success.get -> expected }


    if (failed.nonEmpty) fail(s"Could not convert: ${failed.mkString("\n")}")
    else succeeded.foreach { case (actual, expected) => assertEquals(actual, expected) }
  }
}
