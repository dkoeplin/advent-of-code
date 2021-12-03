
object Main extends App {
  val file = scala.io.Source.fromFile("./data/1.1")
  val increased = file.getLines().map(_.trim().toInt).sliding(2).map{case Seq(prev, current) =>
    if (current > prev) 1 else 0
  }.sum
  file.close()
  println(increased)
}
