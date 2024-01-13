package Year2021

import common.immutable.Pos.Idx
import common.immutable.{Constructable, Cube}
import common.mutable.Matrix
import common.parse

object Day11 extends common.AoC(11, 2021) {
  case class Grid(vol: Cube[Int], data: Array[Int]) extends Matrix[Int](vol, data) {
    var flashes: Int = 0

    def increment(i: Idx): Boolean = {
      update(i, get(i).map(_ + 1))
      get(i).contains(10)
    }

    def flash(i: Idx): Iterable[Idx] = {
      flashes += 1
      Idx.D2.adjacent.map(_ + i).filter(increment).toSeq
    }

    def step(): Unit = {
      var flashers: Seq[Idx] = indices.filter(increment).toSeq
      while (flashers.nonEmpty) {
        flashers = flashers.flatMap(flash)
      }
      indices.foreach{i => update(i, get(i).filter(_ >= 10).map(_ => 0)) }
    }
  }
  implicit object Grid extends Constructable[Int,Grid]

  val grid = parse.digits(data).to[Grid]

  (1 to 100).foreach{_ => grid.step() }
  println(s"Part 1: Flashes: ${grid.flashes}")

  val part2 = (101 to 1000).find{_ => grid.step(); grid.indices.forall{i => grid(i) == 0 } }
  println(s"Part 2: Step: ${part2.map(_.toString).getOrElse("> 1000")}")
}
