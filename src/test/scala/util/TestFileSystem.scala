package util

import java.io.File

case class TestFileSystem(files: List[File], dirs: List[File])

val tempTestDir = File("target/test")

def inFileSystem[A](fileSystem: TestFileSystem, where: File = tempTestDir)(f: File => A): Unit = {
  where.mkdirs()
  val subdirs = fileSystem.dirs.map(where.asParent)
  val subfiles = fileSystem.files.map(where.asParent)

  subdirs.foreach(_.mkdirs())
  subfiles.foreach(_.createNewFile())
  f(where)
  where.delete()
}

def fileSystem(paths: List[String]): TestFileSystem = paths
  .map { File(_) }
  .foldLeft(TestFileSystem(List.empty, List.empty)) { (fs, file) =>
    fs.copy(files = file +: fs.files, dirs = file.getParentFile +: fs.dirs)
  }

extension (parent: File)
  def asParent(that: File): File =
    File(s"${parent.getPath}${that.getPath}")
  def asParent(path: String): File = asParent(File(path))  
