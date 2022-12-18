
import java.io.File
import java.nio.file.Path
import util.*

class FileResolutionTest extends munit.FunSuite {

  test("Recurses through repositories and retrieves files") {
    val fs = fileSystem(
      "/first/first-1/first-1-1/file-1-1-a",
      "/first/first-1/first-1-1/file-1-1-b",
      "/first/first-1/first-1-2/first-1-2-1/file-1-2-1-a",
      "/first/first-1/first-1-2/first-1-2-2/file-1-2-2-a",
      "/first/first-1/first-1-2/first-1-2-2/file-1-2-2-b",
      "/first/first-2/file-2-a",
      "/first/first-3/first-3-1/file-3-1-a",
      "/first/first-3/first-3-1/file-3-1-b",
      "/first/first-3/first-3-1/file-3-1-c",
      "/second/file-a")

    inFileSystem(fileSystem = fs, where = tempTestDir) { () =>
      assertEquals(list(tempTestDir.toPath).toSet, fs.files.map(tempTestDir.asParent).toSet)
    }
  }

}
