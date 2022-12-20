import parser.{Metadata, Parsing}

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers
import parser.{given, *}
import filesystem.*

object Main {
  def main(args: Array[String]): Unit = {
    val input = args.toList.sorted.mkString(" ")

    Parsing
      .run(Parsing.cliArguments, input)
      .map { args =>
        listFiles(args.path.toFile)
          .pipe { fsd => derive(fsd.files, args) }
      }
  }
}
case class FileMetadata(file: File, metadata: List[Metadata])

def derive(files: List[File], args: CliArguments): Option[List[FileMetadata]] = {
  val parser = Parsing.fromGrammar(args.filePattern)

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