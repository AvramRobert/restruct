package filesystem

import java.io.File
import scala.annotation.tailrec

case class FilesystemData(files: List[File])

def listFiles(dir: File): FilesystemData = {
  @tailrec
  def recurse(dirs: List[File], files: List[File] = List.empty): List[File] = dirs match {
  case dir :: dirs if dir.isFile => recurse (dirs, dir +: files)
  case dir :: dirs => recurse (dir.listFiles ().toList ++ dirs, files)
  case Nil => files
}

  if (dir.isFile) FilesystemData(List(dir))
  else FilesystemData(recurse(dir.listFiles().toList))
}
