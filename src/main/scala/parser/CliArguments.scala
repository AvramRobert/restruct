package parser

import java.nio.file.Path

enum Argument(val priority: Int, val ref: String):
  case Path extends Argument(0, "path")
  case Pattern extends Argument(1, "pattern")

case class CliArguments(path: Path, grammar: List[Rule])