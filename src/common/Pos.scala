package common

case class Pos(row: Int, col: Int) {
  def *(n: Int): Pos = Pos(row * n, col * n)
  def +(rhs: Pos): Pos = Pos(row + rhs.row, col + rhs.col)
  def -(rhs: Pos): Pos = Pos(row - rhs.row, col - rhs.col)
  def unary_-(): Pos = Pos(-row, -col)

  def max(rhs: Pos): Pos = Pos(Math.max(row, rhs.row), Math.max(col, rhs.col))
  def min(rhs: Pos): Pos = Pos(Math.min(row, rhs.row), Math.min(col, rhs.col))

  // manhattan distance
  def dist(rhs: Pos): Int = Math.abs(row - rhs.row) + Math.abs(col - rhs.col)

  def to(rhs: Pos): Iterator[Pos] = {
    val stepRow = if (row <= rhs.row) 1 else -1
    val stepCol = if (col <= rhs.col) 1 else -1
    (row to rhs.row by stepRow).iterator.flatMap{i =>
      (col to rhs.col by stepCol).iterator.map{j => Pos(i, j) }
    }
  }

  // Rotates (counterclockwise)
  def t: Pos = this match {
    case Pos.LEFT => Pos.DOWN
    case Pos.RIGHT => Pos.UP
    case Pos.UP => Pos.LEFT
    case Pos.DOWN => Pos.RIGHT
  }
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

  // All non-diagonal deltas
  val nondiag: List[Pos] = List(DOWN, UP, LEFT, RIGHT)

  /// All position deltas
  val all: List[Pos] = List(DOWN, UP, LEFT, RIGHT, UP_LEFT, UP_RIGHT, DOWN_LEFT, DOWN_RIGHT)
}
