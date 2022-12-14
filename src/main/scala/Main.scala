import java.io.File
import scala.annotation.tailrec
import scala.util.chaining.*
import scala.util.parsing.combinator.RegexParsers

@main def hello: Unit =
  println("Hello world!")

def filesInDir(path: String): List[File] = {
  @tailrec
  def recurse(dirs: List[File], files: List[File] = List.empty): List[File] = dirs match {
    case dir :: dirs if dir.isFile => recurse(dirs, dir +: files)
    case dir :: dirs => recurse(dir.listFiles().toList ++ dirs, files)
    case Nil => files
  }

  val file = File(path)
  if (file.isFile) List(file)
  else recurse(file.listFiles().toList)
}