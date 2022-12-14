import parser.{FileData, Parser}

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers
import parser.*

@main def main: Unit = list("path")
  .pipe(dissect)
  .pipe(restructure)

case class FileEntry(file: File, fileData: FileData)

def restructure(files: List[FileEntry]): Unit =
  files
    .foreach { entry =>
      val dir: Path = Paths.get(entry.fileData.key.encoding, entry.fileData.tempo.encoding)
      val newFilePath = dir.resolve(s"${entry.fileData.title} - ${entry.fileData.tempo.bpm}")
      dir.toFile.mkdirs()
      Files.copy(entry.file.toPath, newFilePath, StandardCopyOption.REPLACE_EXISTING)
    }

def dissect(files: List[File]): List[FileEntry] =
  files
    .map { file => Parser
      .run(Parser.fileName, file.getName)
      .map { data => FileEntry(file, data) }
    }
    .filter { value => value.isSuccess }
    .map { value => value.success.get }

def list(path: String): List[File] = {
  @tailrec
  def recurse(dirs: List[File], files: List[File] = List.empty): List[File] = dirs match {
    case dir :: dirs if dir.isFile => recurse(dirs, dir +: files)
    case dir :: dirs => recurse(dir.listFiles().toList ++ dirs, files)
    case Nil => files
  }

  val file = File(path)
  if (file.isFile) List(file)
  else recurse(file.listFiles().toList)
}