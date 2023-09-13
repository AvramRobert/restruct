package data

import typeclasses.Encoding

import java.security.cert.Extension

enum Note(val encoding: String):
  case A extends Note("A")
  case B extends Note("B")
  case C extends Note("C")
  case D extends Note("D")
  case E extends Note("E")
  case F extends Note("F")
  case G extends Note("G")

// Music theory defines more than just these two scales
// I assume kits only limit themselves to these because their sort-of.. simple
enum Scale(val encoding: String):
  case Major extends Scale("maj")
  case Minor extends Scale("min")

enum Accidental(val encoding: String):
  case Natural extends Accidental("")
  case Flat extends Accidental("b")
  case Sharp extends Accidental("#")

case class Key(note: Note, accidental: Accidental, scale: Scale)

case class Tempo(bpm: Long)

case class Label(label: String)

enum Extension(val encoding: String):
  case MP3 extends Extension("mp3")
  case WAV extends Extension("wav")
  case FLACC extends Extension("flacc")

given Encoding[Note] with
  def encode(note: Note): String = note.encoding

given Encoding[Scale] with
  def encode(scale: Scale): String = scale.encoding

given Encoding[Accidental] with
  def encode(accidental: Accidental): String = accidental.encoding

given Encoding[Key] with
  def encode(keyMetadata: Key): String = {
    s"${typeclasses.encode(keyMetadata.note)}${typeclasses.encode(keyMetadata.accidental)} ${typeclasses.encode(keyMetadata.scale)}"
  }

given Encoding[Tempo] with
  def encode(tempo: Tempo): String = s"${tempo.bpm.toFloat.floor.toLong}s bpm"

given Encoding[Label] with
  def encode(title: Label): String = title.label
