package parser

import java.nio.file.Path

enum Argument(val priority: Int, val ref: String):
  case Path extends Argument(0, "path")
  case FilePattern extends Argument(1, "file-pattern")
  case DirStructure extends Argument(2, "dir-structure")
  case RenamePattern extends Argument(3, "rename-pattern")

case class CliArguments(path: Path, filePattern: List[Rule], dirStructure: List[Rule], renamePattern: List[Rule])