
import data.Emission.ParsingEmission
import data.{NamingToken, *}
import data.NamingToken.{KeyToken, MakerToken, NameToken}
import util.*

import java.io.File

class CoreTest extends munit.FunSuite {

  // TODO: Add a `type` pattern => type of sound
  // TODO: Add a `subtype` pattern => hard/soft
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
            ParsingEmission(KeyToken, Key(note = Note.C, accidental = Accidental.Natural, scale = Scale.Minor)),
            ParsingEmission(KeyToken, Key(note = Note.C, accidental = Accidental.Sharp, scale = Scale.Minor)),
            ParsingEmission(KeyToken, Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major))
          ),

          makerPattern -> List(
            ParsingEmission(MakerToken, Label("pack1")),
            ParsingEmission(MakerToken, Label("pack1")),
            ParsingEmission(MakerToken, Label("pack1"))
          ),

          namePattern -> List(
            ParsingEmission(NameToken, Label("808")),
            ParsingEmission(NameToken, Label("808")),
            ParsingEmission(NameToken, Label("808"))
          )
        ),

        contentData = Map(
          ParsingEmission(KeyToken, Key(note = Note.C, accidental = Accidental.Natural, scale = Scale.Minor)) -> List(
            dir.asParent("/pack1/808s/pack1 808 Cmin.wav")
          ),
          ParsingEmission(KeyToken, Key(note = Note.C, accidental = Accidental.Sharp, scale = Scale.Minor)) -> List(
            dir.asParent("/pack1/808s/pack1 808 C#min.wav")
          ),
          ParsingEmission(KeyToken, Key(note = Note.A, accidental = Accidental.Natural, scale = Scale.Major)) -> List(
            dir.asParent("/pack1/808s/synthy/pack1 808 A.wav")
          ),
          ParsingEmission(MakerToken, Label("pack1")) -> List(
            dir.asParent("/pack1/808s/pack1 808 Cmin.wav"),
            dir.asParent("/pack1/808s/pack1 808 C#min.wav"),
            dir.asParent("/pack1/808s/synthy/pack1 808 A.wav")
          ),
          ParsingEmission(NameToken, Label("808")) -> List(
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
