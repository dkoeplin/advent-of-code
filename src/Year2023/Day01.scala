package Year2023

object NamedDigits {
  private val digits = List(
    "zero", "0",
    "one", "1",
    "two", "2",
    "three", "3",
    "four", "4",
    "five", "5",
    "six", "6",
    "seven", "7",
    "eight", "8",
    "nine", "9")
  def unapply(x: String): Option[Int] = {
    val first = digits.zipWithIndex.map{case (d, i) => (x.indexOf(d), i) }.filter(_._1 >= 0).minByOption(_._1)
    val last = digits.zipWithIndex.map{case (d, i) => (x.lastIndexOf(d), i)}.filter(_._1 >= 0).maxByOption(_._1)
    first.zip(last).map{case (a,b) => a._2/2 * 10 + b._2/2 }
  }
}

object Day01 extends Year2023(1) {
  val lines = data.getLines().toArray
  val part1 = lines.map{line =>
    val i = line.indexWhere(_.isDigit)
    val j = line.lastIndexWhere(_.isDigit)
    (if (i >= 0) line(i).asDigit else 0) * 10 + (if (j >= 0) line(j).asDigit else 0)
  }.sum
  println(s"Part 1: $part1")

  val part2 = lines.collect{case NamedDigits(n) => n}.sum
  println(s"Part 2: $part2")
}