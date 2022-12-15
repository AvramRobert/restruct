import parser.{Metadata, Parsing}

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers
import parser.{given, *}

@main def main(): Unit = ()

case class FileMetadata(file: File, metadata: List[Metadata])

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