package Year2023

import common.algorithm.Dijkstra
import common.immutable.Matrix
import common.immutable.Pos.Idx
import common.parse

object Day17 extends common.AoC(17, 2023) {
  // A pair of position + path history (number of times we've moved in the same direction)
  case class Loc(pos: Idx, dir: Option[(Idx, Int)] = None) {
    private def allowedDirs(min: Int, max: Int): Iterator[Idx] = dir match {
      case Some((d, i)) if i < min => Iterator(d)
      case Some((Idx.D2.L | Idx.D2.R, `max`)) => Iterator(Idx.D2.U, Idx.D2.D)
      case Some((Idx.D2.U | Idx.D2.D, `max`)) => Iterator(Idx.D2.L, Idx.D2.R)
      case _ => Iterator(Idx.D2.L, Idx.D2.R, Idx.D2.U, Idx.D2.D).filterNot{ d => dir.exists(_._1 == -d) }
    }
    def allowedDirs(ultra: Boolean): Iterator[Idx] = if (ultra) allowedDirs(4, 10) else allowedDirs(-1, 3)
  }

  class Solver(grid: Matrix[Char], start: Loc, ultra: Boolean) extends Dijkstra[Loc](start) {
    private def count(next: Idx, prev: Option[(Idx, Int)]): Option[(Idx, Int)]
      = prev.filter(_._1 == next).map{case (dir,n) => (dir, n+1) }.orElse(Some((next, 1)))

    def isEnd(loc: Loc): Boolean = { loc.pos == grid.volume.max }
    def next(loc: Loc): Iterator[Loc]
      = loc.allowedDirs(ultra).map{d => Loc(loc.pos + d, count(d, loc.dir)) }
        .filter{l => grid.has(l.pos) }
    override def weight(loc: Loc): Int = grid.getOrElse(loc.pos, '0').asDigit

    def minCost: Int = {
      val paths = solve()
      val pathEnd = paths.dist.iterator.filter{p => isEnd(p._1)}.minBy(_._2)._1
      paths.pathTo(pathEnd).map(_.pos).map(grid.apply).sum
    }
  }

  val grid = parse.chars(data).to[Matrix]
  val start = Loc(Idx(0, 0))
  println(s"Part 1: ${new Solver(grid, start, ultra = false).minCost}")
  println(s"Part 2: ${new Solver(grid, start, ultra = true).minCost}")
}
