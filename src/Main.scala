

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

object Main extends App {
  val file = scala.io.Source.fromFile("./data/3")

  val counts = file.getLines().foldLeft(BitCount()){(gamma, line) => gamma + line }
  val gamma = counts.toGamma
  val alpha = counts.toAlpha
  file.close()

  println(s"${counts.bits()}: $gamma")
  println(s"${counts.flippedBits()}: $alpha")

  println(gamma * alpha)
}
