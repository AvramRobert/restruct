import parser.{Metadata, Parsing}

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers
import parser.{given, *}

object Main {
  def main(args: Array[String]): Unit = {
    val input = args.toList.sorted.mkString(" ")

    Parsing
      .run(Parsing.cliArguments, input)
      .map { args =>
        list(args.path)
          .pipe { files => derive(files, args) }
      }
  }
}
case class FileMetadata(file: File, metadata: List[Metadata])

def derive(files: List[File], args: CliArguments): Option[List[FileMetadata]] = {
  val parser = Parsing.fromGrammar(args.grammar)

  @tailrec
  def acc(files: List[File], result: List[FileMetadata] = List.empty): Option[List[FileMetadata]] = files match {
    case f :: fs => Parsing.run(parser, f.getName) match {
      case ParsingResult.Success(metadata) => acc(fs, FileMetadata(f, metadata) +: result)
      case ParsingResult.Failure(msg) => None
    }
    case Nil => Some(result)
  }

  acc(files)
}

def list(path: Path): List[File] = {
  @tailrec
  def recurse(dirs: List[File], files: List[File] = List.empty): List[File] = dirs match {
    case dir :: dirs if dir.isFile => recurse(dirs, dir +: files)
    case dir :: dirs => recurse(dir.listFiles().toList ++ dirs, files)
    case Nil => files
  }

  val file = path.toFile
  if (file.isFile) List(file)
  else recurse(file.listFiles().toList)
}