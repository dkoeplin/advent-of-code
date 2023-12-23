package common

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
  def points: Seq[Pos] = {
    val length = Math.max(Math.abs(x2 - x1), Math.abs(y2 - y1))
    val x_step = if (x1 > x2) -1 else if (x1 < x2) 1 else 0
    val y_step = if (y1 > y2) -1 else if (y1 < y2) 1 else 0
    (0 to length).map{i => Pos(x1 + x_step*i, y1 + y_step*i) }
  }
}
