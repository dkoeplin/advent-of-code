package Year2023

import common.Pos

object Day18 extends Year2023(18) {
  case class Instruction(dir: Pos, n: Int) {
    // Returns the holes dug by this instruction starting from 'start' in reverse order
    def get(start: Pos): List[Pos] = List.tabulate(n){i => start + dir*(n - i) }
  }
  object Instruction {
    private val dirs = Map("U" -> Pos.UP, "D" -> Pos.DOWN, "L" -> Pos.LEFT, "R" -> Pos.RIGHT)
    def parse1(line: String): Option[Instruction] = Some(line.split(' ')).filter(_.length == 3).map{
      case Array(dir, n, c) => Instruction(dirs(dir), n.toInt)
    }
  }

  case class Lagoon(frontier: List[Pos], dug: Set[Pos], min: Pos, max: Pos) {
    private def withFrontier(start: List[Pos]): Lagoon = Lagoon(start, dug, min, max)

    private def fillOne(start: Pos): Lagoon =
      LazyList.iterate(withFrontier(List(start))){current =>
        val next = Pos.nondiag.iterator.map(_ + current.frontier.head).filterNot(current.dug.contains).toList
        val outside = next.exists{p => (p min min) != min || (p max max) != max }
        if (outside) withFrontier(Nil)
        else Lagoon(next ++ current.frontier.tail, current.dug ++ next, min, max)
      }.dropWhile(_.frontier.nonEmpty).head

    def filled(): Lagoon = dug.foldLeft(this){(current, pos) => current.fillOne(pos) }
  }
  object Lagoon {
    def start: Lagoon = Lagoon(List(Pos(0,0)), Set(Pos(0,0)), Pos(0, 0), Pos(0,0))
    def apply(insts: Iterable[Instruction]): Lagoon = insts.foldLeft(Lagoon.start){(state, inst) =>
      val next = inst.get(state.frontier.head)
      Lagoon(frontier = List(next.head),
        dug = state.dug ++ next,
        min = next.fold(state.min)(_ min _),
        max = next.fold(state.max)(_ max _))
    }
  }

  val insts = data.getLines().flatMap(Instruction.parse1).toArray
  val trench = Lagoon(insts)
  val filled = trench.filled()
  println(s"Part 1: ${filled.dug.size}")
}
