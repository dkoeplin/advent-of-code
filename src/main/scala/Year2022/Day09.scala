package Year2022

import common.immutable.Pos.Idx

object Day09 extends common.AoC(9, 2022) {
  case class State(chain: List[Idx], visited: Set[Idx])

  val lines = data.getLines().toArray

  def move(head: Idx, tail: Idx): Idx = {
    val diff = head - tail
    if (Math.abs(diff.r) <= 1 && Math.abs(diff.c) <= 1)
      Idx(0, 0)
    else
      Idx(math.signum(diff.r), math.signum(diff.c))
  }
  def compute(n: Int): Int = {
    val chain = List.fill(n){ Idx(0, 0) }
    val start = State(chain, Set(Idx(0, 0)))
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
