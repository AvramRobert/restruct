import java.io.File
import data.*

import Parsing.Parser
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

object Core {
  @tailrec
  private def readFileData(files: List[File], parser: Parser[List[Emission]], output: FileData = Map.empty): Try[FileData] = {
    files match
      case Nil => Success(output)
      case f :: fs => Parsing
        .run(parser, f.getName)
        .map { emissions =>
          emissions.foldLeft(output) { (newOutput, emission) =>
            newOutput.updatedWith(emission.pattern) {
              case Some(items) => Some(items + (f -> emission))
              case None => Some(Map(f -> emission))
            }
          }
        } match {
        case Success(value) => readFileData(fs, parser, value)
        case e => e
      }
  }

  def readDirectoryStructure(files: List[File], args: CliArguments): Try[DirectoryStructure] =
    for {
      parser   <- Success(Parsing.fromPatterns(args.filePattern))
      fileData <- readFileData(files, parser)
    } yield DirectoryStructure(
      fileData = fileData,
      structure = args.dirStructure,
      reversionSchema = Map.empty
    )
}
