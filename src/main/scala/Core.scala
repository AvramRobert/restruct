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
      folderStructure = args.dirStructure,
      renamePattern = args.filePattern,
      reversionSchema = Map.empty
    )

    def createRestructureSchema(dir: DirectoryStructure): List[File] = traverse(dir, dir.folderStructure)

    private def traverse(dir: DirectoryStructure,
                         patterns: List[Pattern],
                         path: List[Emission] = List.empty, // unique concrete path derived from structure
                         files: Set[File] = Set.empty, // files in the unique path
                         ): List[File] = patterns match {
      case Nil => deriveFiles(dir, path.reverse, files)
      case p :: ps => dir.fileData.metaData.get(p) match {
        case Some(emissions) => emissions.flatMap { emission =>
          dir.fileData.contentData.get(emission) match {
            case Some(efs) => traverse(dir, ps, emission :: path, files.intersect(efs.toSet))
            case None => Nil
          }
        }
        case None => Nil
      }
    }

    private def deriveFiles(dir: DirectoryStructure,
                            path: List[Emission],
                            files: Set[File]): List[File] = Nil // TODO
}
