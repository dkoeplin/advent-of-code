package Year2022

object Day03 extends App {
  def priority(x: Char) = if (x.isLower) x - 'a' + 1 else x - 'A' + 27

  val file = scala.io.Source.fromFile("data/full/03")
  val lines = file.getLines().toArray
  val part1 = lines.map{line =>
    val c1 = line.substring(0, line.length/2).toSet
    val c2 = line.substring(line.length/2, line.length).toSet
    (c1 intersect c2).map(priority).sum
  }.sum
  println(s"Part1: $part1")

  val part2 = lines.grouped(3).map{group =>
    group.map(_.toSet).reduce{(a,b) => a intersect b}.map(priority).sum }.sum
  println(s"Part2: $part2")
}
