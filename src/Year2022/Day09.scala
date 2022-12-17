package Year2022

object Day09 extends App {
  case class Pos(row: Int, col: Int) {
    def +(dir: Char): Pos = dir match {
      case 'L' => Pos(row, col - 1)
      case 'R' => Pos(row, col + 1)
      case 'U' => Pos(row - 1, col)
      case 'D' => Pos(row + 1, col)
    }

    def -(rhs: Pos): Pos = Pos(row - rhs.row, col - rhs.col)

    def +(rhs: Pos): Pos = Pos(row + rhs.row, col + rhs.col)
  }

  case class State(chain: List[Pos], visited: Set[Pos])

  val file = scala.io.Source.fromFile("data/2022/09")
  val lines = file.getLines().toArray

  def move(head: Pos, tail: Pos) = {
    val diff = head - tail
    if (Math.abs(diff.row) <= 1 && Math.abs(diff.col) <= 1)
      Pos(0, 0)
    else
      Pos(Math.signum(diff.row).toInt, Math.signum(diff.col).toInt)
  }
  def compute(n: Int): Int = {
    val chain = List.fill(n){ Pos(0, 0) }
    val start = State(chain, Set(Pos(0, 0)))
    val end = lines.foldLeft(start) { case (state, line) =>
      (0 until line.drop(2).toInt).foldLeft(state) { case (state, _) =>
        val h = state.chain.head + line.head
        val t = state.chain.tail.scanLeft(h){case (h, t) => t + move(h, t) }
        State(h +: t.tail, state.visited + t.last)
      }
    }
    end.visited.size
  }
  val part1 = compute(2)
  println(s"Part1: $part1")
  val part2 = compute(10)
  println(s"Part2: $part2")
}
