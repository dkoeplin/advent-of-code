package Year2022

object Day06 extends App {
  def first_unique_n(str: String, n: Int) = str.sliding(n).zipWithIndex.find(_._1.toSet.size == n).map(_._2 + n)
  val file = scala.io.Source.fromFile("data/2022/06")
  val lines = file.getLines().toArray
  lines.zipWithIndex.foreach{case (line, i) =>
    println(s"Part1:${i+1}: ${first_unique_n(line, 4).getOrElse(-1)}")
  }
  lines.zipWithIndex.foreach{case (line, i) =>
    println(s"Part1:${i+1}: ${first_unique_n(line, 14).getOrElse(-1)}")
  }
}
