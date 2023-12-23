package Year2023

import common.immutable.Matrix

object Day13 extends Year2023(13) {
  case class Slice(i: Int, iter: IndexedSeq[(Int, Int)]) {
    def isVerticalReflection(p: Pattern, smudges: Int): Boolean
      = p.rowIndices.map{i => iter.count{case (j0, j1) => p(i,j0) != p(i,j1) }}.sum == smudges
    def isHorizontalReflection(p: Pattern, smudges: Int): Boolean
      = p.colIndices.map{j => iter.count{case (i0, i1) => p(i0, j) != p(i1, j) }}.sum == smudges
  }
  case class Pattern(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    private def slices(n: Int): Iterator[Slice] = (0 to n-2).iterator.map{ i =>
      val len = math.min(i+1, n - i - 1)
      Slice(i, (i-len+1 to i).zip(i + len to i + 1 by -1))
    }
    def colOfVerticalReflection(smudges: Int): Option[Int]
      = slices(cols).find(_.isVerticalReflection(this, smudges)).map(_.i + 1)
    def rowOfHorizontalReflection(smudges: Int): Option[Int]
      = slices(rows).find(_.isHorizontalReflection(this, smudges)).map(_.i + 1)
    def reflectionScore(smudges: Int): Int
      = 100 * rowOfHorizontalReflection(smudges).getOrElse(0) + colOfVerticalReflection(smudges).getOrElse(0)
  }
  val patterns: Array[Pattern] = data.getLines().foldLeft(Array(Array.empty[String])){(list, line) =>
    if (line.isEmpty) Array.empty[String] +: list else (list.head :+ line) +: list.tail
  }.map{lines => Pattern(lines.iterator.map(_.toArray)) }

  val part1 = patterns.iterator.map(_.reflectionScore(smudges=0)).sum
  println(s"Part 1: $part1")

  val part2 = patterns.iterator.map(_.reflectionScore(smudges=1)).sum
  println(s"Part 2: $part2")
}
