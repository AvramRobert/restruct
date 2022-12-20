package parser

import java.nio.file.Path

enum Argument(val priority: Int, val ref: String):
  case Path extends Argument(0, "path")
  case Pattern extends Argument(1, "file-pattern")
  case Rename extends Argument(2, "rename-pattern")

case class CliArguments(path: Path, filePattern: List[Rule], renamePattern: List[Rule])