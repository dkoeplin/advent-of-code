package Year2022

object Day20 extends common.AoC(20, 2022) {
  def gather(a: Array[Long], map: Map[Int,Int]): Array[Long] = {
    val b = Array.fill(a.length)(0L)
    a.indices.foreach{i => b(map.getOrElse(i, i)) = a(i) }
    b
  }
  def move(x: Long, i: Int, length: Int): Int = {
    val raw = i + x
    val dir = Ordering[Long].compare(x, 0)
    val remain = Math.abs(x) - (if (x >= 0) length - i - 1 else i)
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

  case class Mapping(forward: Map[Int, Int] = Map.empty, backward: Map[Int, Int] = Map.empty) {
    def add(i: Int, i2: Int): Mapping = Mapping(forward + (i -> i2), backward + (i2 -> i))
    def remap(i2: Int, i3: Int): Mapping = add(bwd(i2), i3)
    def fwd(i: Int): Int = forward.getOrElse(i, i)
    def bwd(i2: Int): Int = backward.getOrElse(i2, i2)
  }
  def mix(a: Array[Long], n: Int, verbose: Boolean = false): Array[Long] = {
    val map = (0 until n).foldLeft(Mapping()){(map, iter) =>
      a.indices.iterator.foldLeft(map){(map, i) =>
        val i2 = map.fwd(i)
        val i3 = move(a(i), i2, a.length)
        val dir = if (i2 > i3) -1 else 1
        (i2 until i3 by dir).foldLeft(map){(map, j2) => map.remap(j2 + dir, j2) }.add(i, i3)
      }
    }
    gather(a, map.forward)
  }

  def mix2(a: Array[Long], n: Int, verbose: Boolean = false): Array[Long] = {
    val array = a.clone
    val fwd = scala.collection.mutable.Map.empty[Int, Int] ++ array.indices.map{i => i -> i}
    val bwd = scala.collection.mutable.Map.empty[Int, Int] ++ array.indices.map{i => i -> i}
    def relocate(i2: Int, i3: Int): Unit = {
      array(i3) = array(i2)
      val i = bwd(i2)
      fwd(i) = i3
      bwd(i3) = i
    }
    (0 until n).foreach{n =>
      a.indices.iterator.foreach{i =>
        val i2 = fwd(i)
        val x = array(i2)
        val i3 = move(x, i2, a.length)
        val dir = if (i2 > i3) -1 else 1
        (i2 until i3 by dir).foreach{j2 => relocate(j2 + dir, j2) }
        fwd(i) = i3
        bwd(i3) = i
        array(i3) = x
      }
    }
    array
  }

  def coord(a: Array[Long]): Long = {
    val idx0 = a.indexOf(0)
    (1000 to 3000 by 1000).map{i => a((idx0 + i) % a.length) }.sum
  }

  val nums = data.getLines().map(_.toLong).toArray
  val start1 = System.currentTimeMillis()
  println(s"Part1: ${coord(mix(nums, n=1))}")
  println(s"Part2: ${coord(mix(nums.map(_ * 811589153L), n=10))}")
  println(s"Functional: ${System.currentTimeMillis() - start1} ms")

  val start2 = System.currentTimeMillis()
  println(s"Part1: ${coord(mix2(nums, n=1))}")
  println(s"Part2: ${coord(mix2(nums.map(_ * 811589153L), n=10))}")
  println(s"Imperative: ${System.currentTimeMillis() - start2} ms")
}
