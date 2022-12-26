import data.{CliArguments, DirectoryStructure}

import java.io.File
import scala.annotation.tailrec
import scala.util.{Try, Success, Failure}
import data.*

object FileSystem {
  def listFiles(dir: File): List[File] = {
    @tailrec
    def recurse(dirs: List[File], files: List[File] = List.empty): List[File] = dirs match {
      case dir :: dirs if dir.isFile => recurse(dirs, dir +: files)
      case dir :: dirs => recurse(dir.listFiles().toList ++ dirs, files)
      case Nil => files
    }

    if (dir.isFile) List(dir)
    else recurse(dir.listFiles().toList)
  }

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
        case e => e
      }
    }

    acc(files)
      .map(content => DirectoryStructure(
        content = content,
        structure = args.dirStructure,
        reversionSchema = Map.empty))
  }
}
