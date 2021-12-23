
object Day01 extends App {
  val file = scala.io.Source.fromFile("./data/1")
  val lines = file.getLines().map(_.trim().toInt).toArray
  file.close()

  val part1 = lines.sliding(2).map{case Array(prev, current) => if (current > prev) 1 else 0 }.sum
  println(s"Part 1: $part1")

  val part2 = lines.sliding(3,1).map(_.sum).sliding(2).map{case Seq(prev, current) => if (current > prev) 1 else 0 }.sum
  println(s"Part 2: $part2")
}
