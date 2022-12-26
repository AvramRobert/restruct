
import data.*
import java.nio.file.{Files, Path, Paths}
import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers

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


def run(args: CliArguments): Try[Unit] =
  FileSystem
    .listFiles(args.directory)
    .pipe { files => FileSystem.readDirectoryStructure(files, args) }
    .map { structure => () }
