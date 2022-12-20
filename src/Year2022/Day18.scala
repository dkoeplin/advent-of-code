package Year2022

import common.Pos3

object Day18 extends App {
  val file = scala.io.Source.fromFile("data/2022/18")
  val spots = file.getLines().map(Pos3.parse).toSet
  val part1 = spots.iterator.map{s => Pos3.adjs.count{d => !spots.contains(s + d) }}.sum
  println(s"Part1: $part1")
  val min = spots.reduce(_ min _) - Pos3(1, 1, 1)
  val max = spots.reduce(_ max _) + Pos3(1, 1, 1)
  var outside: Set[Pos3] = Set.empty
  outside ++= (min to Pos3(max.x, min.y, min.z))
  outside ++= (min to Pos3(min.x, max.y, min.z))
  outside ++= (min to Pos3(min.x, min.y, max.z))
  outside ++= (Pos3(min.x, max.y, max.z) to max)
  outside ++= (Pos3(max.x, min.y, max.z) to max)
  outside ++= (Pos3(max.x, max.y, min.z) to max)
  var worklist = outside
  while (worklist.nonEmpty) {
    val next = worklist.flatMap{p => Pos3.adjs.iterator.map(_ + p)
                        .filter(_.isIn(min, max))
                        .filterNot(spots.contains)
                        .filterNot(outside.contains)}
    outside ++= next
    worklist = next
  }
  val part2 = spots.iterator.map{s => Pos3.adjs.count{d => outside.contains(s + d) }}.sum
  println(s"Part2: $part2")
}
