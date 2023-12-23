package common

object Directions extends Enumeration {
  type Dir = Value
  def flip(dir: Dir): Dir = if (dir == L) R else L
  val L: Dir = Value("L")
  val R: Dir = Value("R")
}
