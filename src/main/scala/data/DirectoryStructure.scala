package data

import java.io.File

case class FileData(metaData: Map[Pattern, List[Emission]], contentData: Map[Emission, List[File]])
case class DirectoryStructure(fileData: FileData, structure: List[Pattern], reversionSchema: Map[File, File])