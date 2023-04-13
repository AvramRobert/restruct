import java.io.File
import data.*

import Parsing.Parser
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}

object Core {
  @tailrec
  private def readFileData(files: List[File],
                            parser: Parser[List[Emission]],
                            meta: Map[Pattern, List[Emission]] = Map.empty,
                            content: Map[Emission, List[File]] = Map.empty): Try[FileData] =
    files match
      case Nil => Success(FileData(meta, content))
      case f :: fs => Parsing
        .run(parser, f.getName)
        .map { emissions =>
          val newMeta = emissions.foldLeft(meta) { (newMeta, e) =>
            newMeta.updatedWith(e.pattern) {
              case Some(es) => Some(e :: es)
              case None => Some(e :: Nil)
            }
          }

          val newContent = emissions.foldLeft(content) { (newContent, e) =>
            newContent.updatedWith(e) {
              case Some(fs) => Some(f :: fs)
              case None => Some(f :: Nil)
            }
          }
          (newMeta, newContent)
        } match {
        case Success((newMeta, newContent)) => readFileData(fs, parser, newMeta, newContent)
        case Failure(err) => Failure(err)
      }

  def readDirectoryStructure(files: List[File], args: CliArguments): Try[DirectoryStructure] = for {
      parser <- Success(Parsing.fromPatterns(args.filePattern))
      fileData <- readFileData(files, parser)
    } yield DirectoryStructure(
      fileData = fileData,
      structure = args.dirStructure,
      reversionSchema = Map.empty
    )

    def createRestructureSchema(dir: DirectoryStructure): List[(File, File)] = Nil
}
