package Year2022

import common.immutable.Pos

object Day18 extends common.AoC(18, 2022) {
  val spots = data.getLines().map(Pos.parse[Int]).toSet
  val part1 = spots.iterator.map{s => Pos.adjacent[Int](3).count{d => !spots.contains(s + d) }}.sum
  println(s"Part1: $part1")
  val min = spots.reduce(_ min _) - Pos(1, 1, 1)
  val max = spots.reduce(_ max _) + Pos(1, 1, 1)
  var outside: Set[Pos[Int]] = Set.empty
  outside ++= (min to Pos(max.x, min.y, min.z)).iterator
  outside ++= (min to Pos(min.x, max.y, min.z)).iterator
  outside ++= (min to Pos(min.x, min.y, max.z)).iterator
  outside ++= (Pos(min.x, max.y, max.z) to max).iterator
  outside ++= (Pos(max.x, min.y, max.z) to max).iterator
  outside ++= (Pos(max.x, max.y, min.z) to max).iterator
  var worklist = outside
  while (worklist.nonEmpty) {
    val next = worklist.flatMap{p => Pos.adjacent[Int](3).map(_ + p)
                        .filter(_.isIn(min to max))
                        .filterNot(spots.contains)
                        .filterNot(outside.contains)}
    outside ++= next
    worklist = next
  }
  val part2 = spots.iterator.map{s => Pos.adjacent[Int](3).count{d => outside.contains(s + d) }}.sum
  println(s"Part2: $part2")
}
