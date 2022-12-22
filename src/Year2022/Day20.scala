package Year2022

object Day20 extends App {
  def gather(a: Array[Long], map: Map[Int,Int]): Array[Long] = {
    val b = Array.fill(a.length)(0L)
    map.foreach{case (i, i3) => b(i3) = a(i) }
    b
  }
  def move(x: Long, i: Int, length: Int): Int = {
    val raw = i + x
    val dir = Ordering[Long].compare(x, 0)
    val remain = Math.abs(x) - (if (x >= 0) (length - i - 1) else i)
    // Wrapping is "free" but looks like a count in modular arithmetic, e.g. 0 => 6 is "free"
    val wraps = if (raw < 0 || raw >= length) 1 + (remain / (length - 1)) else 0
    Math.floorMod(raw + wraps * dir, length.toLong).toInt
  }
  assert(move(-1, 0, 7) == 5)
  assert(move(-2, 0, 7) == 4)
  assert(move(-3, 0, 7) == 3)
  assert(move(-14, 0, 7) == 4)
  assert(move(14, 1, 7) == 3)
  assert(move(205, 4, 7) == 5)
  assert(move(-93, 6, 7) == 3)

  def mix(a: Array[Long], n: Int, verbose: Boolean = false): Array[Long] = {
    if (verbose) println(a.mkString(" "))
    val map = (0 until n).foldLeft(Map.empty[Int,Int]){(map, iter) =>
      println(s"Mixing #$iter")
      a.indices.iterator.foldLeft(map) { (map, i) =>
        val i2 = map.getOrElse(i, i)
        val i3 = move(a(i), i2, a.length)
        val min = Math.min(i2, i3)
        val max = Math.max(i2, i3)
        val map2 = a.indices.iterator.map { j =>
          val j3 = map.getOrElse(j, j)
          if (i == j) i -> i3
          else if (j3 >= min && j3 <= max && i2 < i3) j -> (j3 - 1)
          else if (j3 >= min && j3 <= max && i3 < i2) j -> (j3 + 1)
          else j -> j3
        }.toMap
        if (verbose) println(s"Move ${a(i)} from $i2 to $i3: ${gather(a, map2).mkString(" ")}")
        map2
      }
    }
    gather(a, map)
  }
  def coord(a: Array[Long]): Long = {
    val idx0 = a.indexOf(0)
    (1000 to 3000 by 1000).map{i => a((idx0 + i) % a.length) }.sum
  }

  val file = scala.io.Source.fromFile("data/2022/20")
  val nums = file.getLines().map(_.toLong).toArray
  val part1 = coord(mix(nums, n=1))
  println(s"Part1: $part1")
  val part2 = coord(mix(nums.map(_ * 811589153), n=10))
  println(s"Part2: $part2")
}
