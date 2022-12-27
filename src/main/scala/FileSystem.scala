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
}
