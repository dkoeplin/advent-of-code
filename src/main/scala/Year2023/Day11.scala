package Year2023

import common.immutable.Pos.Idx
import common.immutable.{Box, Constructable, Matrix, Pos}
import common.implicits.IteratorOps._
import common.parse

object Day11 extends common.AoC(11, 2023) {
  case class Image(vol: Box[Int], data: Array[Char]) extends Matrix[Char](vol, data) {
    lazy val galaxies: List[Idx] = indices.filter{pos => get(pos).contains('#') }.toList
    lazy val emptyCols: Set[Int] = wIterator.filterNot{j => galaxies.exists(_.c == j) }.toSet
    lazy val emptyRows: Set[Int] = hIterator.filterNot{i => galaxies.exists(_.r == i) }.toSet
    def dist(x: Idx, y: Idx): Idx = {
      ((x.c min y.c) until (x.c max y.c)).iterator.total(emptyCols.contains) +
      ((x.r min y.r) until (x.r max y.r)).iterator.total(emptyRows.contains)
    }
    val Pos(nonEmpty, empty) = galaxies.combinations(2).collect{case List(x, y) => dist(x, y) }.reduce(_ + _)
  }
  implicit object Image extends Constructable[Char,Image]
  val img = parse.chars(data).to[Image]
  println(s"Part 1: ${img.empty*2 + img.nonEmpty}")
  println(s"Part 2: ${img.empty*1000000 + img.nonEmpty}")
}
