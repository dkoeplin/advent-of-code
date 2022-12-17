package Year2022

import common.Pos
import scala.util.matching.Regex

object Day15 extends App {
  case class Pair(sensor: Pos, beacon: Pos)
  object Pair {
    val pattern: Regex = "Sensor at x=([-0-9]+), y=([-0-9]+): closest beacon is at x=([-0-9]+), y=([-0-9]+)".r
    def parse(line: String): Pair = pattern.findFirstMatchIn(line).map{m =>
      Pair(Pos(m.group(2).toInt, m.group(1).toInt), Pos(m.group(4).toInt, m.group(3).toInt))
    }.getOrElse(throw new Exception(s"Unable to parse $line"))
  }

  val file = scala.io.Source.fromFile("example/2022/15")
}
