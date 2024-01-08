package Year2021

case class BitCount(ones: Array[Int] = Array.empty, total: Int = 0) {
  def +(str: String): BitCount = {
    val rhs = BitCount(str)
    if (ones.isEmpty) BitCount(rhs.ones, 1) else BitCount(ones.zip(rhs.ones).map{case (a,b) => a + b}, total + 1)
  }
  def bits(): String = ones.map{count => if (count >= total / 2) 1 else 0 }.mkString("")
  def flippedBits(): String = bits().map{c => if (c == '1') '0' else '1'}
  def toGamma: Int = Integer.parseInt(bits(), 2)
  def toAlpha: Int = Integer.parseInt(flippedBits(), 2)
  println(s"${ones.mkString(",")}; total: $total")
}
object BitCount {
  def apply(str: String) = new BitCount(str.trim().map{c => if (c == '1') 1 else 0}.toArray)
}

object Day03 extends common.AoC(3, 2021) {
  val lines = data.getLines().toArray
  val counts = lines.foldLeft(BitCount()){(gamma, line) => gamma + line }
  val gamma = counts.toGamma
  val alpha = counts.toAlpha

  println(s"Part 1 (Gamma): ${counts.bits()}: $gamma")
  println(s"Part 1 (Alpha): ${counts.flippedBits()}: $alpha")
  println(s"Part 1: ${gamma * alpha}")

  def filter(lines: Array[Int], length: Int, mostCommon: Boolean): Int = {
    var one: Int = 0x1 << (length - 1)
    var candidates: Array[Int] = lines
    while (candidates.length > 1 && one > 0) {
      val ones = candidates.count{bits => (bits & one) > 0 }
      val bit = if ((ones > 0 && (2 * ones >= candidates.length) == mostCommon) || ones == candidates.length)
        one
      else 0
      candidates = candidates.filter{bits => (bits & one) == bit }
      one = one >> 1
    }
    assert(candidates.length == 1)
    candidates(0)
  }

  var length = 0
  val part2 = lines.map{line =>
    length = Math.max(length, line.length)
    Integer.parseUnsignedInt(line, 2)
  }

  val oxygen = filter(part2, length, mostCommon = true)
  val scrubber = filter(part2, length, mostCommon = false)
  println(s"Part 2: ${oxygen * scrubber}")
}
