
import java.io.File
import java.nio.file.Path
import filesystem.*
import util.*

class FileSystemTest extends munit.FunSuite {

  test("Recurses through repositories and retrieves files") {
    val first11Files = List(
      "/first/first-1/first-1-1/file-1-1-a",
      "/first/first-1/first-1-1/file-1-1-b",
    )

    val first121Files = List(
      "/first/first-1/first-1-2/first-1-2-1/file-1-2-1-a",
    )

    val first122Files = List(
      "/first/first-1/first-1-2/first-1-2-2/file-1-2-2-a",
      "/first/first-1/first-1-2/first-1-2-2/file-1-2-2-b",
    )

    val first2Files = List(
      "/first/first-2/file-2-a"
    )

    val first31Files = List(
      "/first/first-3/first-3-1/file-3-1-a",
      "/first/first-3/first-3-1/file-3-1-b",
      "/first/first-3/first-3-1/file-3-1-c",
    )

    val secondFiles = List(
      "/second/file-a"
    )

    val fs = fileSystem(
      first11Files ++
      first121Files ++
      first122Files ++
      first2Files ++
      first31Files ++
      secondFiles)

    inFileSystem(fileSystem = fs) { parent =>
      val actualAll = listFiles(parent).files
      val expectedAll = fs.files.map(parent.asParent)

      val actualFirst = listFiles(parent.asParent("/first")).files
      val expectedFirst = (first11Files ++ first121Files ++ first122Files ++ first2Files ++ first31Files).map(parent.asParent)

      val actualFirst1 = listFiles(parent.asParent("/first/first-1")).files
      val expectedFirst1 = (first11Files ++ first121Files ++ first122Files).map(parent.asParent)

      val actualFirst11 = listFiles(parent.asParent("/first/first-1/first-1-1")).files
      val expectedFirst11 = first11Files.map(parent.asParent)

      val actualFirst12 = listFiles(parent.asParent("/first/first-1/first-1-2")).files
      val expectedFirst12 = (first121Files ++ first122Files).map(parent.asParent)

      val actualFirst3 = listFiles(parent.asParent("/first/first-3")).files
      val expectedFirst3 = first31Files.map(parent.asParent)

      val actualSecond = listFiles(parent.asParent("/second")).files
      val expectedSecond = secondFiles.map(parent.asParent)

      assertEquals(actualAll.toSet, expectedAll.toSet)
      assertEquals(actualFirst.toSet, expectedFirst.toSet)
      assertEquals(actualFirst1.toSet, expectedFirst1.toSet)
      assertEquals(actualFirst11.toSet, expectedFirst11.toSet)
      assertEquals(actualFirst12.toSet, expectedFirst12.toSet)
      assertEquals(actualFirst3.toSet, expectedFirst3.toSet)
      assertEquals(actualSecond.toSet, expectedSecond.toSet)
    }
  }
}
