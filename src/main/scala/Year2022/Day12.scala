package Year2022

import common.algorithm.Dijkstra
import common.immutable.Pos.Idx
import common.mutable.Matrix
import common.parse

object Day12 extends common.AoC(12, 2022) {
  val matrix: Matrix[Char] = parse.chars(data).to[Matrix]
  val start = matrix.indices.find{pos => matrix(pos) == 'S' }.get
  val end = matrix.indices.find{pos => matrix(pos) == 'E' }.get
  matrix(start) = 'a'
  matrix(end) = 'z'

  class Solver(matrix: Matrix[Char], start: Idx, end: Idx) extends Dijkstra[Idx](start) {
    override def isEnd(v: Idx): Boolean = v == end
    override def next(pos: Idx): Iterator[Idx] = {
      val src = matrix(pos).toInt
      Idx.D2.nondiag.map(_ + pos).filter{pos => matrix.get(pos).exists{dest => dest.toInt - src <= 1 }}
    }
  }

  val part1 = new Solver(matrix, start, end).solve()
  println(s"Part1: ${part1.distanceTo(end)}")

  val part2 = matrix.indices.filter{i => matrix(i) == 'a'}
                            .map{pos => new Solver(matrix, pos, end).solve().distanceTo(end) }.min
  println(s"Part2: $part2")
}
