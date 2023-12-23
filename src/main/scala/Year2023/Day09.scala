package Year2023

object Day09 extends Year2023(9) {
  case class Pair(left: Int, right: Int) {
    def +(rhs: Pair): Pair = Pair(left + rhs.left, right + rhs.right)
  }
  def probablyAI(pts: Array[Int]): Pair = {
    LazyList.iterate((pts, Pair(pts.head, pts.last), -1, true)){case (prev, Pair(totalL, totalR), i, _) =>
      val (delta, zeros) = prev.sliding(2).foldLeft((Array.empty[Int], true)){
        case ((list, z), Array(a, b)) => (list :+ (b - a), z && b == a)
      }
      (delta, Pair(totalL + i*delta.head, totalR + delta.last), -i, !zeros)
    }.dropWhile(_._4).head._2
  }
  val Pair(part2, part1) = data.getLines().map{line => line.split(' ').map(_.toInt)}
                           .map(probablyAI)
                           .reduce{_ + _}
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
