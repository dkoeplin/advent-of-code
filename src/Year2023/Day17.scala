package Year2023

import common.Pos
import common.immutable.Matrix
import common.algorithm.{Dijkstra, PathSummary}

object Day17 extends Year2023(17) {
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

    def ==(rhs: Loc): Boolean = pos == rhs.pos && (dir == rhs.dir || dir.isEmpty || rhs.dir.isEmpty)
    def !=(rhs: Loc): Boolean = !(this == rhs)
  }

  val grid = Matrix[Int](data.getLines().map(_.map(_.asDigit).toArray))
  val start = Loc(Pos(0, 0))
  val end: Loc = Loc(Pos(grid.rows - 1, grid.cols - 1))
  class Solver(start: Loc, end: Loc, ultra: Boolean) extends Dijkstra[Loc](start, end) {
    private def count(next: Pos, prev: Option[(Pos, Int)]): Option[(Pos, Int)]
      = prev.filter(_._1 == next).map{case (dir,n) => (dir, n+1) }.orElse(Some((next, 1)))
    def next(loc: Loc): Iterator[Loc]
      = loc.allowedDirs(ultra).map{d => Loc(loc.pos + d, count(d, loc.dir)) }
                              .filter{l => grid.get(l.pos).nonEmpty }
    def weight(loc: Loc): Int = grid.getOrElse(loc.pos, 0)
  }
  def minCost(paths: PathSummary[Loc]): Int = {
    val pathEnd = paths.dist.iterator.filter(_._1 == paths.end).minBy(_._2)._1
    paths.pathTo(pathEnd).map(_.pos).map(grid.apply).sum
  }
  val part1 = new Solver(start, end, ultra = false).solve()
  println(s"Part 1: ${minCost(part1)}")

  val part2 = new Solver(start, end, ultra = true).solve()
  println(s"Part 2: ${minCost(part2)}")
}
