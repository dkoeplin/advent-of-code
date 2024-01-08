package Year2022

object Day06 extends common.AoC(6, 2022) {
  def first_unique_n(str: String, n: Int): Option[Int] = str.sliding(n).zipWithIndex.find(_._1.toSet.size == n).map(_._2 + n)
  val lines = data.getLines().toArray
  lines.zipWithIndex.foreach{case (line, i) =>
    println(s"Part1:${i+1}: ${first_unique_n(line, 4).getOrElse(-1)}")
  }
  lines.zipWithIndex.foreach{case (line, i) =>
    println(s"Part1:${i+1}: ${first_unique_n(line, 14).getOrElse(-1)}")
  }
}
