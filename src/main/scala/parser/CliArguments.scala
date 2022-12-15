package parser

import java.nio.file.Path

case class CliArguments(path: Path, grammar: List[Rule])