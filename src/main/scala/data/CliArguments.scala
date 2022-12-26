package data

import java.io.File

enum Argument(val priority: Int, val ref: String):
  case Dir extends Argument(0, "dir")
  case FilePattern extends Argument(1, "file-pattern")
  case DirStructure extends Argument(2, "dir-structure")
  case RenamePattern extends Argument(3, "rename-pattern")

case class CliArguments(directory: File, 
                        filePattern: List[Pattern], 
                        dirStructure: List[Pattern], 
                        renamePattern: List[Pattern])