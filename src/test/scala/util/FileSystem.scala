package util

import java.io.File

case class FileSystem(files: List[File], dirs: List[File])

val tempTestDir = File("target/test")

def inFileSystem[A](fileSystem: FileSystem, where: File = tempTestDir)(f: () => A): Unit = {
  where.mkdirs()
  val subdirs = fileSystem.dirs.map(where.asParent)
  val subfiles = fileSystem.files.map(where.asParent)

  subdirs.foreach(_.mkdirs())
  subfiles.foreach(_.createNewFile())
  f()
  where.delete()
}

def fileSystem(paths: String*): FileSystem = paths
  .map { File(_) }
  .foldLeft(FileSystem(List.empty, List.empty)) { (fs, file) =>
    fs.copy(files = file +: fs.files, dirs = file.getParentFile +: fs.dirs)
  }

extension (parent: File)
  def asParent(that: File): File =
    File(s"${parent.getPath}${that.getPath}")
