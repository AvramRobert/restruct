
import data.*
import util.*
import java.io.File

class CoreTest extends munit.FunSuite {

  // TODO: Add a `type` pattern => type of sound
  // TODO: Add a `subtype` pattern => hard/soft
  // TODO: Add a `extension` pattern => for .wav, .mp3
  // TODO: Add list of emissions for FileData
  test("reads tokens occurring singularly") {
    val testFileSystem = fileSystem(
      List(
      "/pack1/808s/pack1 808 Cmin.wav",
      "/pack1/808s/pack1 808 C#min.wav",
      "/pack1/808s/synthy/pack1 808 A.wav"
    ))

    inFileSystem(testFileSystem) { dir =>
      val subdir = dir.asParent("/pack1/808s")
      val args = CliArguments(
        directory = subdir,
        filePattern = List(
          makerPattern,
          namePattern,
          keyPattern
        ),
        dirStructure = List(
          keyPattern,
          makerPattern,
        ),
        renamePattern = List(
          makerPattern,
          namePattern
        )
      )
      val expectedData: FileData = FileData(
        metaData = Map(
          keyPattern -> List(
            Emission.ParsingEmission(Token.Key, Key(note = Note.C, accidental = Accidental.None, scale = Scale.Minor)),
            Emission.ParsingEmission(Token.Key, Key(note = Note.C, accidental = Accidental.Sharp, scale = Scale.Minor)),
            Emission.ParsingEmission(Token.Key, Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major))
          ),

          makerPattern -> List(
            Emission.ParsingEmission(Token.Maker, Label("pack1")),
            Emission.ParsingEmission(Token.Maker, Label("pack1")),
            Emission.ParsingEmission(Token.Maker, Label("pack1"))
          ),

          namePattern -> List(
            Emission.ParsingEmission(Token.Name, Label("808")),
            Emission.ParsingEmission(Token.Name, Label("808")),
            Emission.ParsingEmission(Token.Name, Label("808"))
          )
        ),

        contentData = Map(
          Emission.ParsingEmission(Token.Key, Key(note = Note.C, accidental = Accidental.None, scale = Scale.Minor)) -> List(
            dir.asParent("/pack1/808s/pack1 808 Cmin.wav")
          ),
          Emission.ParsingEmission(Token.Key, Key(note = Note.C, accidental = Accidental.Sharp, scale = Scale.Minor)) -> List(
            dir.asParent("/pack1/808s/pack1 808 C#min.wav")
          ),
          Emission.ParsingEmission(Token.Key, Key(note = Note.A, accidental = Accidental.None, scale = Scale.Major)) -> List(
            dir.asParent("/pack1/808s/synthy/pack1 808 A.wav")
          ),
          Emission.ParsingEmission(Token.Maker, Label("pack1")) -> List(
            dir.asParent("/pack1/808s/pack1 808 Cmin.wav"),
            dir.asParent("/pack1/808s/pack1 808 C#min.wav"),
            dir.asParent("/pack1/808s/synthy/pack1 808 A.wav")
          ),
          Emission.ParsingEmission(Token.Name, Label("808")) -> List(
            dir.asParent("/pack1/808s/pack1 808 Cmin.wav"),
            dir.asParent("/pack1/808s/pack1 808 C#min.wav"),
            dir.asParent("/pack1/808s/synthy/pack1 808 A.wav")
          )
        )
      )
      val files = FileSystem.listFiles(subdir)
      val structure = Core.readDirectoryStructure(files, args)

      assertEquals(structure.isSuccess, true)
      assertEquals(structure.get.fileData, expectedData)
    }
  }

}
