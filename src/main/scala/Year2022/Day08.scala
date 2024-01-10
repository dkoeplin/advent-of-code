package Year2022

import common.immutable.Matrix
import common.immutable.Pos.Idx
import common.parse

object Day08 extends common.AoC(8, 2022) {
  val matrix = parse.digits(data).to[Matrix]
  def scan(i: Int, dir: Idx): Set[Idx] = {
    var max: Int = -1
    var set: Set[Idx] = Set.empty
    var pos: Idx = dir match {
      case Idx.D2.L => Idx(i, matrix.W - 1)
      case Idx.D2.R => Idx(i, 0)
      case Idx.D2.U => Idx(matrix.H - 1, i)
      case Idx.D2.D => Idx(0, i)
    }
    while (matrix.has(pos)) {
      if (matrix(pos) > max) {
        max = matrix(pos)
        set += pos
      }
      pos += dir
    }
    set
  }
  def score(i: Idx, dir: Idx): Int = {
    val find = matrix(i)
    var count: Int = 0
    var pos: Idx = i + dir
    while (matrix.has(pos)) {
      count += 1
      pos = if (matrix(pos) >= find) Idx(-1, -1) else pos + dir
    }
    count
  }

  val h = matrix.hIterator.flatMap{i => scan(i, Idx.D2.L) ++ scan(i, Idx.D2.R) }.toSet
  val v = matrix.wIterator.flatMap{i => scan(i, Idx.D2.U) ++ scan(i, Idx.D2.D) }.toSet
  val x = h ++ v
  val part1 = x.size
  println(s"Part1: $part1")

  val part2 = matrix.indices.iterator.map{i => Idx.D2.nondiag.map{d => score(i, d)}.product }.max
  println(s"Part2: $part2")
}
