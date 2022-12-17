package common

case class Pos(row: Int, col: Int) {
  def +(rhs: Pos): Pos = Pos(row + rhs.row, col + rhs.col)
  def -(rhs: Pos): Pos = Pos(row - rhs.row, col - rhs.col)

  def max(rhs: Pos): Pos = Pos(Math.max(row, rhs.row), Math.max(col, rhs.col))
  def min(rhs: Pos): Pos = Pos(Math.min(row, rhs.row), Math.min(col, rhs.col))
}
object Pos {
  // \ | /
  // -   -
  // / | \
  val DOWN: Pos = Pos(1, 0)
  val UP: Pos = Pos(-1, 0)
  val LEFT: Pos = Pos(0, -1)
  val RIGHT: Pos = Pos(0, 1)
  val UP_LEFT: Pos = UP + LEFT
  val UP_RIGHT: Pos = UP + RIGHT
  val DOWN_LEFT: Pos = DOWN + LEFT
  val DOWN_RIGHT: Pos = DOWN + RIGHT
}
