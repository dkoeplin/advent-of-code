package Year2021

import common.implicits.BooleanOps._

object Day01 extends common.AoC(1, 2021) {
  val lines = data.getLines().map(_.trim().toInt).toArray

  val part1 = lines.sliding(2).map{case Array(prev, current) => (current > prev).toInt }.sum
  println(s"Part 1: $part1")

  val part2 = lines.sliding(3, 1).map(_.sum).sliding(2).map{case Seq(prev, current) => (current > prev).toInt }.sum
  println(s"Part 2: $part2")
}
