package typeclasses

trait Encoding[A]:
  def encode(elem: A): String

def encode[A](value: A)(using Encoding[A]): String = encoding[A].encode(value)
def encoding[A](using inline: Encoding[A]): Encoding[A] = summon[Encoding[A]]