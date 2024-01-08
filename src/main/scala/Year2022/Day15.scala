package Year2022

import scala.util.matching.Regex

object Day15 extends common.AoC(15, 2022) {
  case class Pos(row: Long, col: Long)
  def manhattan(x: Pos, y: Pos): Long = Math.abs(x.row - y.row) + Math.abs(x.col - y.col)
  case class Range(min: Long, max: Long) {
    def merge(rhs: Range): Range = Range(Math.min(min, rhs.min), Math.max(max, rhs.max))
    def intersect(rhs: Range): Range = Range(Math.max(min, rhs.min), Math.min(max, rhs.max))
    def iterator: Iterator[Long] = (min to max).iterator
    def overlaps(rhs: Range): Boolean = !(rhs.min > max + 1 || rhs.max < min - 1)
    def drop(rhs: Range): Seq[Range] = if (this.overlaps(rhs)) {
        val left = Range(min, rhs.min - 1)
        val right = Range(rhs.max + 1, max)
        Seq(left, right).filter(_.isValid)
    } else Seq(this)
    def isValid: Boolean = max >= min
  }
  case class Opening(ranges: Seq[Range]) {
    def drop(range: Range): Opening = Opening(ranges.flatMap(_.drop(range)))
    def isEmpty: Boolean = ranges.isEmpty
    override def toString: String = ranges.map{r => s"${r.min}:${r.max}"}.mkString(" ")
  }
  case class Pair(sensor: Pos, beacon: Pos) {
    val radius: Long = manhattan(sensor, beacon)
    val xrange: Range = Range(sensor.col - radius, sensor.col + radius)
    def contains(pos: Pos): Boolean = manhattan(sensor, pos) <= radius
    def covered(pos: Pos): Boolean = manhattan(sensor, pos) <= radius && pos != beacon
    def range(y: Long): Option[Range] = {
      val dist = Math.abs(sensor.row - y)
      if (dist < radius) Some(Range(xrange.min + dist, xrange.max - dist)) else None
    }
  }
  object Pair {
    val pattern: Regex = "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)".r
    def parse(line: String): Pair = pattern.findFirstMatchIn(line).map{m =>
      Pair(Pos(m.group(2).toInt, m.group(1).toInt), Pos(m.group(4).toInt, m.group(3).toInt))
    }.getOrElse(throw new Exception(s"Unable to parse $line"))
  }

  val pairs = data.getLines().map(Pair.parse).toArray.sortBy(_.sensor.col)
  val xrange = pairs.foldLeft(pairs.head.xrange){(curr,pair) => curr merge pair.xrange }

  def covered(y: Long): Long = xrange.iterator.count{x => pairs.exists(_.covered(Pos(y, x))) }
  val part1 = covered(2000000)
  println(s"Part1: $part1 [${xrange.min}, ${xrange.max}]")

  var y: Long = -1
  var x: Option[Long] = None
  while (y <= 4000000 && x.isEmpty) {
    y += 1
    var opening: Opening = Opening(Seq(Range(0, 4000000)))
    val iter = pairs.iterator
    while (iter.hasNext && !opening.isEmpty) {
      iter.next().range(y).foreach{range => opening = opening.drop(range) }
    }
    x = opening.ranges.headOption.map(_.min)
  }
  println(s"Part2: ${4000000 * x.get + y}: (y=$y, x=${x.get})")
}
