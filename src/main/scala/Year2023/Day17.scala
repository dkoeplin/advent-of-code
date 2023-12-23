package Year2023

import common.Pos
import common.immutable.Matrix
import common.algorithm.Dijkstra

object Day17 extends Year2023(17) {
  // A pair of position + path history (number of times we've moved in the same direction)
  case class Loc(pos: Pos, dir: Option[(Pos, Int)] = None) {
    private def allowedDirsBasic: Iterator[Pos] = dir match {
      case Some((Pos.LEFT | Pos.RIGHT, 3)) => Iterator(Pos.UP, Pos.DOWN)
      case Some((Pos.UP | Pos.DOWN, 3)) => Iterator(Pos.LEFT, Pos.RIGHT)
      case _ => Iterator(Pos.LEFT, Pos.RIGHT, Pos.UP, Pos.DOWN).filterNot{d => dir.exists(_._1 == -d) }
    }
    private def allowedDirsUltra: Iterator[Pos] = dir match {
      case Some((d, i)) if i < 4 => Iterator(d)
      case Some((Pos.LEFT | Pos.RIGHT, 10)) => Iterator(Pos.UP, Pos.DOWN)
      case Some((Pos.UP | Pos.DOWN, 10)) => Iterator(Pos.LEFT, Pos.RIGHT)
      case _ => Iterator(Pos.LEFT, Pos.RIGHT, Pos.UP, Pos.DOWN).filterNot{d => dir.exists(_._1 == -d) }
    }
    def allowedDirs(ultra: Boolean): Iterator[Pos] = if (ultra) allowedDirsUltra else allowedDirsBasic
  }

  // Naming is hard
  class Solver(grid: Matrix[Int], start: Loc, ultra: Boolean)
    extends Dijkstra[Loc](start) {
    private def count(next: Pos, prev: Option[(Pos, Int)]): Option[(Pos, Int)]
      = prev.filter(_._1 == next).map{case (dir,n) => (dir, n+1) }.orElse(Some((next, 1)))

    def isEnd(loc: Loc): Boolean = loc.pos.row == grid.rows - 1 && loc.pos.col == grid.cols - 1
    def next(loc: Loc): Iterator[Loc]
      = loc.allowedDirs(ultra).map{d => Loc(loc.pos + d, count(d, loc.dir)) }
        .filter{l => grid.get(l.pos).nonEmpty }
    def weight(loc: Loc): Int = grid.getOrElse(loc.pos, 0)

    def minCost: Int = {
      val paths = solve()
      val pathEnd = paths.dist.iterator.filter{p => isEnd(p._1)}.minBy(_._2)._1
      paths.pathTo(pathEnd).map(_.pos).map(grid.apply).sum
    }
  }

  val grid = Matrix[Int](data.getLines().map(_.map(_.asDigit).toArray))
  val start = Loc(Pos(0, 0))
  println(s"Part 1: ${new Solver(grid, start, ultra = false).minCost}")
  println(s"Part 2: ${new Solver(grid, start, ultra = true).minCost}")
}
