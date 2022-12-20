import parser.{Metadata, Parsing}

import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers
import parser.{*, given}
import filesystem.*
import scala.util.{Failure, Success, Try}

case class DirectoryStructure(content: Map[Rule, Map[File, Metadata]],
                              structure: List[Rule],
                              reversionSchema: Map[File, File])
object Main {
  def main(args: Array[String]): Unit =
    readArgs(args)
      .flatMap(run)
      .fold(err => err.printStackTrace(), _ => println("success"))
}

def readArgs(args: Array[String]): Try[CliArguments] = args
  .toList
  .sorted
  .mkString(" ")
  .pipe(args => Parsing.run(Parsing.cliArguments, args))
  .pipe {
    case ParsingResult.Success(result) => Success(result)
    case ParsingResult.Failure(msg) => Failure(Throwable(msg))
  }

def readDirectoryStructure(files: List[File], args: CliArguments): Try[DirectoryStructure] = {
  val parser = Parsing.fromGrammar(args.filePattern)

  @tailrec
  def acc(files: List[File],
          content: Map[Rule, Map[File, Metadata]] = Map.empty): Try[Map[Rule, Map[File, Metadata]]] = files match {
    case f :: fs => Parsing.run(parser, f.getName) match {
      case ParsingResult.Success(metadata) => acc(
        files = fs,
        content = metadata.foldLeft(content) { (newContent, m) =>
          newContent.updatedWith(rule(m)) {
            case Some(items) => Some(items.updatedWith(f) { _ => Some(m) })
            case None => Some(Map(f -> m))
          }
        }
      )
      case ParsingResult.Failure(msg) => Failure(Throwable(msg))
    }
    case Nil => Success(content)
  }

  acc(files)
    .map(content => DirectoryStructure(
      content = content,
      structure = args.dirStructure,
      reversionSchema = Map.empty))
}

def restructure(structure: DirectoryStructure): Unit = ???

def rule(metadata: Metadata): Rule = ???

def run(args: CliArguments): Try[Unit] =
  listFiles(args.directory)
    .pipe { files => readDirectoryStructure(files, args) }
    .map { structure => restructure(structure) }
