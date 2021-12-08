object Main extends App {
  def fuelCost(dist: Long): Long = dist*(dist + 1)/2 // sum of 1...N

  val Num = "([0-9]+)".r
  val file = scala.io.Source.fromFile("./data/7")
  var minPos: Int = Int.MaxValue
  var maxPos: Int = Int.MinValue
  val crabs = file.getLines().next().split(",").flatMap{
    case Num(x) =>
      val pos = x.toInt
      minPos = Math.min(minPos, pos)
      maxPos = Math.max(maxPos, pos)
      Some(pos)
    case _ => None
  }
  val min = (minPos to maxPos).foldLeft((Long.MaxValue,Long.MaxValue)){(accum,pos) =>
      val cost = crabs.foldLeft(0L){(accum, crab) => accum + fuelCost(Math.abs(crab - pos)) }
      println(s"$pos: $cost")
      if (cost < accum._2) (pos, cost) else accum
  }

  println(s"Min: $min")
}
