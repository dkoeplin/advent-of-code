package Year2023

import common.immutable.Pos.Idx
import common.immutable.{Constructible, Matrix, Pos, Volume}
import common.implicits.IteratorOps._
import common.parse

object Day11 extends common.AoC(11, 2023) {
  case class Image(volume: Volume[Int], data: Array[Char]) extends Matrix[Char](volume, data) {
    lazy val galaxies: List[Idx] = indices.filter{pos => get(pos).contains('#') }.toList
    lazy val emptyCols: Set[Int] = wIterator.filterNot{j => galaxies.exists(_.w == j) }.toSet
    lazy val emptyRows: Set[Int] = hIterator.filterNot{i => galaxies.exists(_.h == i) }.toSet
    def dist(x: Idx, y: Idx): Idx = {
      ((x.w min y.w) until (x.w max y.w)).iterator.total(emptyCols.contains) +
      ((x.h min y.h) until (x.h max y.h)).iterator.total(emptyRows.contains)
    }
    val Pos(nonEmpty, empty) = galaxies.combinations(2).collect{case List(x, y) => dist(x, y) }.reduce(_ + _)
  }
  implicit object Image extends Constructible[Char,Image]
  val img = parse.chars(data).to[Image]
  println(s"Part 1: ${img.empty*2 + img.nonEmpty}")
  println(s"Part 2: ${img.empty*1000000 + img.nonEmpty}")
}
