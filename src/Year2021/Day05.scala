package Year2021

import common.{Line, Pos}

object Day05 extends App {
  val Pattern = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
  val file = scala.io.Source.fromFile("./data/5")
  val repeats = file.getLines().map { case Pattern(x1, y1, x2, y2) => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt) }
    .foldLeft((Set.empty[Pos], Set.empty[Pos])) { (previous, line) =>
      val points = line.points.toSet
      (previous._1 ++ points, previous._2 ++ (previous._1 intersect points))
    }._2.size

  println(s"repeats: $repeats")
}
