package util

import java.io.File
import java.nio.file.{Files, Path}
import java.util.Comparator
import java.util.UUID
import scala.util.{ Try, Success, Failure }

case class TestFileSystem(files: List[File], dirs: List[File])

val tempTestDir = File("target/test")

def inFileSystem[A](fileSystem: TestFileSystem, parentDir: File = tempTestDir)(f: File => A): Unit = {
  val local = File(s"${parentDir.getAbsolutePath}/test_dir_${UUID.randomUUID}")
  local.mkdirs()

  val subdirs = fileSystem.dirs.map(local.asParent)
  val subfiles = fileSystem.files.map(local.asParent)

  subdirs.foreach(_.mkdirs())
  subfiles.foreach(_.createNewFile())

  Try(f(local)) match {
    case Success(_) => deleteDir(local)
    case Failure(err) =>
      deleteDir(local)
      throw err
  }
}

def fileSystem(paths: List[String]): TestFileSystem = paths
  .map { File(_) }
  .foldLeft(TestFileSystem(List.empty, List.empty)) { (fs, file) =>
    fs.copy(files = file +: fs.files, dirs = file.getParentFile +: fs.dirs)
  }

def deleteDir(dir: File): Unit =
  if (dir.exists()) {
    Files
      .walk(dir.toPath)
      .sorted(Comparator.reverseOrder[Path]())
      .forEach(_.toFile.delete())
  } else ()

extension (parent: File)
  def asParent(that: File): File =
    File(s"${parent.getPath}${that.getPath}")
  def asParent(path: String): File = asParent(File(path))  
