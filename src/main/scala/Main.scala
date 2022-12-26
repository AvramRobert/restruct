import parser.Parsing

import data.*
import java.nio.file.{Files, Path, Paths}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers
import parser.{*, given}
import filesystem.*

import scala.util.{Failure, Success, Try}

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

def readDirectoryStructure(files: List[File], args: CliArguments): Try[DirectoryStructure] = {
  val parser = Parsing.fromPatterns(args.filePattern)

  @tailrec
  def acc(files: List[File],
          content: Map[Pattern, Map[File, Emission]] = Map.empty): Try[Map[Pattern, Map[File, Emission]]] = files match {
    case Nil => Success(content)
    case f :: fs => Parsing
      .run(parser, f.getName)
      .map { emissions =>
        emissions.foldLeft(content) { (newContent, emission) =>
          newContent.updatedWith(emission.pattern) {
            case Some(items) => Some(items + (f -> emission))
            case None => Some(Map(f -> emission))
          }
        }
      } match {
      case Success(value) => acc(fs, value)
      case e              => e
    }
  }

  acc(files)
    .map(content => DirectoryStructure(
      content = content,
      structure = args.dirStructure,
      reversionSchema = Map.empty))
}

def restructure(structure: DirectoryStructure): Unit = ???

def run(args: CliArguments): Try[Unit] =
  listFiles(args.directory)
    .pipe { files => readDirectoryStructure(files, args) }
    .map { structure => restructure(structure) }
