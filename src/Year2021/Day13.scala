package Year2021

import scala.collection.mutable

case class Point(x: Int, y: Int)
case class Fold(x: Boolean, coord: Int)

object Day13 extends App {
  val PointPattern = "([0-9]+),([0-9]+)".r
  val FoldPattern = "fold along ([xy])=([0-9]+)".r
  val file = scala.io.Source.fromFile("./data/13")
  val points: mutable.Set[Point] = mutable.Set.empty[Point]
  val folds: mutable.Buffer[Fold] = mutable.Buffer.empty[Fold]
  file.getLines().foreach {
    case PointPattern(x, y) => points += Point(x.toInt, y.toInt)
    case FoldPattern(x, c) => folds += Fold(x == "x", c.toInt)
    case _ =>
  }

  def fold(pt: Int, axis: Int): Int = if (pt < axis) pt else 2*axis - pt
  def makeFolds(folds: Iterator[Fold]): Set[Point] = folds.foldLeft(points.toSet){case (pts, Fold(xAxis, coord)) =>
    pts.map{
      case Point(x, y) if xAxis => Point(fold(x, coord), y)
      case Point(x, y) if !xAxis => Point(x, fold(y, coord))
    }
  }

  val part1 = makeFolds(folds.iterator.take(1))
  val part2 = makeFolds(folds.iterator)

  def visualize(points: Set[Point]): Unit = {
    val xMax = points.map(_.x).max
    val yMax = points.map(_.y).max
    (0 to yMax).foreach{y =>
      val line = (0 to xMax).map{x =>
        if (points.contains(Point(x, y))) '#' else ' '
      }.mkString("")
      println(line)
    }
  }

  println(s"Part 1: ${part1.size}")
  println(s"Part 2: ${part2.size}")
  visualize(part2) // Cheating
}
