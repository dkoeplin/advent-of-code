package Year2023

import common.immutable.Pos.Idx

object Day09 extends common.AoC(9, 2023) {
  def probablyAI(pts: Array[Int]): Idx = {
    LazyList.iterate((pts, Idx(pts.head, pts.last), -1, true)){case (prev, Idx(totalL, totalR), i, _) =>
      val (delta, zeros) = prev.sliding(2).foldLeft((Array.empty[Int], true)){
        case ((list, z), Array(a, b)) => (list :+ (b - a), z && b == a)
      }
      (delta, Idx(totalL + i*delta.head, totalR + delta.last), -i, !zeros)
    }.dropWhile(_._4).head._2
  }
  val Idx(part2, part1) = data.getLines().map{line => line.split(' ').map(_.toInt)}
                           .map(probablyAI)
                           .reduce{_ + _}
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
