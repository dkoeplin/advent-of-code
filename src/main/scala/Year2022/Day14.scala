package Year2022

import common.immutable.Pos.Idx
import common.mutable.SparseMatrix

object Day14 extends common.AoC(14, 2022) {
  // pair is x, y, matrix uses y, x
  val paths = data.getLines().map{line => line.split(" -> ").map(Idx.parse).map(_.reverse) }.toArray
  val grid = new SparseMatrix[Char]('.')
  paths.foreach{path => path.sliding(2).foreach{case Array(a,b) => (a to b).iterator.foreach{i => grid(i) = '#' }}}

  def flood(floor: Option[Long]): Long = {
    def inBounds(pos: Idx): Boolean = if (floor.nonEmpty) pos.r < floor.get else pos isIn (grid.min to grid.max)
    def isAir(pos: Idx): Boolean = !floor.contains(pos.r) && grid(pos) == '.'

    var continue: Boolean = true
    val moves = Seq(Idx.D2.D, Idx.D2.D + Idx.D2.L, Idx.D2.D + Idx.D2.R)
    var count: Int = 0
    while (continue) {
      var sand: Idx = Idx(0, 500)
      var next: Option[Idx] = Some(sand)
      while (next.exists(inBounds)) {
        next = moves.iterator.map(_ + sand).find(isAir)
        sand = next.getOrElse(sand)
      }
      continue = inBounds(sand)
      if (continue) {
        grid(sand) = 'o'
        count += 1
        continue = continue && (sand != Idx(0, 500))
      }
    }
    count
  }
  println("== Part 1 ==")
  val maxY = grid.max.r
  val part1 = flood(floor=None)
  println(s"Part1: $part1")

  println("== Part 2 ==")
  val part2 = flood(floor=Some(maxY + 2))
  println(s"Part2: ${part1 + part2}")
}
