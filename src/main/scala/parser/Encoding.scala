package parser

trait Encoding[A]:
  def encode(elem: A): String

def encoder[A](using inline: Encoding[A]): Encoding[A] = summon[Encoding[A]]
def encode[A](value: A)(using Encoding[A]): String = encoder[A].encode(value)