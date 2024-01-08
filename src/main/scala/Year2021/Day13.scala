package Year2021

import common.immutable.Pos.Idx

import scala.collection.mutable

object Day13 extends common.AoC(13, 2021) {
  case class Fold(x: Boolean, coord: Int)

  val PointPattern = "([0-9]+),([0-9]+)".r
  val FoldPattern = "fold along ([xy])=([0-9]+)".r
  val points: mutable.Set[Idx] = mutable.Set.empty[Idx]
  val folds: mutable.Buffer[Fold] = mutable.Buffer.empty[Fold]
  data.getLines().foreach {
    case PointPattern(x, y) => points += Idx(x.toInt, y.toInt)
    case FoldPattern(x, c) => folds += Fold(x == "x", c.toInt)
    case _ =>
  }

  def fold(pt: Int, axis: Int): Int = if (pt < axis) pt else 2*axis - pt
  def makeFolds(folds: Iterator[Fold]): Set[Idx] = folds.foldLeft(points.toSet){case (pts, Fold(xAxis, coord)) =>
    pts.map{
      case Idx(x, y) if xAxis => Idx(fold(x, coord), y)
      case Idx(x, y) if !xAxis => Idx(x, fold(y, coord))
    }
  }

  val part1 = makeFolds(folds.iterator.take(1))
  val part2 = makeFolds(folds.iterator)

  def visualize(points: Set[Idx]): Unit = {
    val xMax = points.map(_.x).max
    val yMax = points.map(_.y).max
    (0 to yMax).foreach{y =>
      val line = (0 to xMax).map{x =>
        if (points.contains(Idx(x, y))) '#' else ' '
      }.mkString("")
      println(line)
    }
  }

  println(s"Part 1: ${part1.size}")
  println(s"Part 2: ${part2.size}")
  visualize(part2) // Cheating
}
