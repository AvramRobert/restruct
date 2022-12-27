
import data.*
import util.*
import java.io.File

class CoreTest extends munit.FunSuite {

  // TODO: Add a `type` pattern => type of sound
  // TODO: Add a `subtype` pattern => hard/soft
  // TODO: Add a `extension` pattern => for .wav, .mp3

  test("reads file data semantically") {
    val keys = List(
      "/pack1/808s/pack1 808 hard Cmin.wav",
      "/pack1/808s/pack1 808 soft C#min.wav",
    )

    val keysAndTempo = List(
      "/pack1/vocals/shouts/pack1 female shout oh-yeah Amin 140BPM.wav",
      "/pack1/vocals/shouts/pack1 male shout oh-yeah A 150BPM.wav"
    )

    val testFileSystem = fileSystem(keys ++ keysAndTempo)

    inFileSystem(testFileSystem) { dir =>
      val args = CliArguments(
        directory = File(s"${dir.getAbsolutePath}/pack1/808s"),
        filePattern = List(
          Pattern.TokenPattern(Token.Maker),
          Pattern.TokenPattern(Token.Name),
          Pattern.TokenPattern(Token.Name),
          Pattern.TokenPattern(Token.Key)
        ),
        dirStructure = List(
          Pattern.TokenPattern(Token.Key),
          Pattern.TokenPattern(Token.Maker),
        ),
        renamePattern = List(
          Pattern.TokenPattern(Token.Maker),
          Pattern.TokenPattern(Token.Name)
        )
      )
      val files = FileSystem.listFiles(dir)
      val structure = Core.readDirectoryStructure(files, args)

      assertEquals(structure.isSuccess, true)
    }
  }

}
