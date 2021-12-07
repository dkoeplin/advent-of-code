
case class BitCount(counts: Seq[Int] = Array.fill(32)(0), total: Int = 0) {
  def bits(): String =
  def flippedBits(): String = bits().map{c => if (c == '1') '0' else '1'}
  def toGamma: Int = Integer.parseInt(bits(), 2)
  def toAlpha: Int = Integer.parseInt(flippedBits(), 2)
}

object Main extends App {
  val file = scala.io.Source.fromFile("./data/3")

  var length = 0
  var total = 0
  val counts = file.getLines().zipWithIndex.map{case (line, i) =>
    Integer.parseInt(_,2) 
  }
    .foldLeft(Seq.fill(32)(0)){(accum, value: Int) =>
      total += 1
      (31 to 0 by -1).map{i => accum(31 - i) + ((value & 0x1 << i) >> i) }
  }
  val gamma = counts.map{count => if (2 * count >= total) 1 else 0 }.mkString("")
  val alpha = counts.toAlpha
  file.close()

  println(s"${counts.bits()}: $gamma")
  println(s"${counts.flippedBits()}: $alpha")

  println(gamma * alpha)
}
