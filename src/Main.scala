case class Pos(x: Int, y: Int)

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
  def points: Seq[Pos] = {
    val length = Math.max(Math.abs(x2 - x1), Math.abs(y2 - y1))
    val x_step = if (x1 > x2) -1 else if (x1 < x2) 1 else 0
    val y_step = if (y1 > y2) -1 else if (y1 < y2) 1 else 0
    (0 to length).map{i => Pos(x1 + x_step*i, y1 + y_step*i) }
  }
}

object Main extends App {
  val Pattern = "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)".r
  val file = scala.io.Source.fromFile("./data/5")
  val repeats = file.getLines().map{case Pattern(x1,y1,x2,y2) => Line(x1.toInt,y1.toInt,x2.toInt,y2.toInt) }
    .foldLeft((Set.empty[Pos], Set.empty[Pos])){(previous, line) =>
      val points = line.points.toSet
      (previous._1 ++ points, previous._2 ++ (previous._1 intersect points))
    }._2.size

  println(s"repeats: $repeats")
}
