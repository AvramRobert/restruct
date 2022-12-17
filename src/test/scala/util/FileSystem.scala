package util

import java.io.File

case class FileSystem(files: List[File], dirs: List[File])

val tempTestDir = File("target/test")

def inFileSystem[A](fileSystem: FileSystem, where: File = tempTestDir)(f: () => A): Unit = {
  where.mkdirs()
  val subdirs = fileSystem.dirs.map(f => where.toPath.resolve(f.toPath).toFile)
  val subfiles = fileSystem.files.map(f => where.toPath.resolve(f.toPath).toFile)
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
