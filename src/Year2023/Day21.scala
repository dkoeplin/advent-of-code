package Year2023

import common.Pos
import common.immutable.Matrix

object Day21 extends Year2023(21) {
  val map = Matrix[Char](data.getLines().map(_.toArray))
  val start = map.posIterator().find{p => map(p) == 'S' }.get
  def next(pos: Pos): Iterator[Pos]
    = Pos.nondiag.iterator.map(_ + pos).filter{p => map.get(p).exists{c => c == '.' || c == 'S'}}

  def reachableAt(n: Int): Set[Pos] = (1 to n).foldLeft(Set(start)){(f,_) => f.flatMap(next) }

  val part1 = reachableAt(64)
  println(s"Part 1: ${part1.size}")
}
