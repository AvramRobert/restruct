package data

import java.io.File

type FileData = Map[Pattern, Map[File, Emission]]

case class DirectoryStructure(fileData: FileData, structure: List[Pattern], reversionSchema: Map[File, File])
