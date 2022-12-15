package parser

enum Note(val encoding: String):
  case A extends Note("A")
  case B extends Note("B")
  case C extends Note("C")
  case D extends Note("D")
  case E extends Note("E")
  case F extends Note("F")
  case G extends Note("G")

enum Scale(val encoding: String):
  case Major extends Scale("maj")
  case Minor extends Scale("min")

enum Accidental(val encoding: String):
  case None extends Accidental("")
  case Flat extends Accidental("♭")
  case Sharp extends Accidental("#")

case class KeyMetadata(note: Note, accidental: Accidental, scale: Scale) 

case class TempoMetadata(bpm: Long)

case class LabelMetadata(label: String)

given Encoding[Note] with
  def encode(note: Note): String = note.encoding

given Encoding[Scale] with
  def encode(scale: Scale): String = scale.encoding

given Encoding[Accidental] with
  def encode(accidental: Accidental): String = accidental.encoding

given Encoding[KeyMetadata] with
  def encode(keyMetadata: KeyMetadata): String = {
    s"${parser.encode(keyMetadata.note)}${parser.encode(keyMetadata.accidental)} ${parser.encode(keyMetadata.scale)}"
  }
  
given Encoding[TempoMetadata] with
  def encode(tempo: TempoMetadata): String = s"${tempo.bpm.toFloat.floor.toLong}s bpm"
  
given Encoding[LabelMetadata] with
  def encode(title: LabelMetadata): String = title.label