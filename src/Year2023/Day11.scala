package Year2023

import common.Pos
import common.immutable.Matrix

object Day11 extends Year2023(11) {
  case class Image(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    lazy val galaxies: List[Pos] = posIterator().filter{pos => get(pos).contains('#') }.toList
    lazy val emptyCols: Set[Int] = (0 until cols).filterNot{j => galaxies.exists(_.col == j) }.toSet
    lazy val emptyRows: Set[Int] = (0 until rows).filterNot{i => galaxies.exists(_.row == i) }.toSet
    def dist(x: Pos, y: Pos): (Long, Long) = {
      val (horz1, horz2) = ((x.col min y.col) until (x.col max y.col)).iterator.partition(emptyCols.contains)
      val (vert1, vert2) = ((x.row min y.row) until (x.row max y.row)).iterator.partition(emptyRows.contains)
      (horz1.size + vert1.size, horz2.size + vert2.size)
    }
    val (empty, nonEmpty) = galaxies.combinations(2).map{case List(x, y) => dist(x, y) }
                                    .reduce{(a,b) => (a._1 + b._1, a._2 + b._2)}
  }
  val img = Image(data.getLines().map(_.toArray))
  println(s"Part 1: ${img.empty*2 + img.nonEmpty}")
  println(s"Part 2: ${img.empty*1000000 + img.nonEmpty}")
}
