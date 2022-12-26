package data

import java.io.File

case class DirectoryStructure(content: Map[Pattern, Map[File, Emission]],
                              structure: List[Pattern],
                              reversionSchema: Map[File, File])
