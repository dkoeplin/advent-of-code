package Year2022

case class Range(start: Int, end: Int) {
  def contains(rhs: Range): Boolean = rhs.start >= start && rhs.end <= end
  def overlaps(rhs: Range): Boolean = rhs.start <= end && rhs.end >= start
}
object Range {
  def parse(x: String): Range = {
    val split = x.split("-")
    Range(split(0).toInt, split(1).toInt)
  }
  def parse_pair(x: String): (Range, Range) = {
    val pair = x.split(",")
    (parse(pair(0)), parse(pair(1)))
  }
}
object Day04 extends App {
  val file = scala.io.Source.fromFile("data/2022/04")
  val lines = file.getLines().map(Range.parse_pair).toArray
  val part1 = lines.count{case (a,b) => a.contains(b) || b.contains(a) }
  val part2 = lines.count{case (a,b) => a.overlaps(b) }
  println(s"Part1: $part1")
  println(s"Part2: $part2")
}
