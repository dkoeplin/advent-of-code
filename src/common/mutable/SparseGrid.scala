package common.mutable

import common.Pos

case class SparseGrid[T](default: T) {
  def apply(i: Int, j: Int): T = map.getOrElse(Pos(i, j), default)
  def apply(x: Pos): T = map.getOrElse(x, default)
  def update(x: Pos, value: T): Unit = map(x) = value
  def update(i: Int, j: Int, value: T): Unit = map(Pos(i, j)) = value

  def start(): Pos = {
    val min = map.keys.reduceLeft{(a, b) => a min b }
    Pos(Math.min(min.row, minY.getOrElse(min.row)), Math.min(min.col, minX.getOrElse(min.col)))
  }
  def end(): Pos = {
    val max = map.keys.reduceLeft{(a, b) => a max b }
    Pos(Math.max(max.row, maxY.getOrElse(max.row)), Math.max(max.col, maxX.getOrElse(max.col)))
  }

  override def toString: String = {
    val min = start()
    val max = end()
    val ss = StringBuilder.newBuilder
    (min.row to max.row).foreach { i =>
      (min.col to max.col).foreach { j =>
        ss ++= apply(i, j).toString
      }
      if (i < max.row) ss += '\n'
    }
    ss.mkString
  }

  private val map = scala.collection.mutable.Map.empty[Pos, T]
  var minX: Option[Int] = None
  var minY: Option[Int] = None
  var maxX: Option[Int] = None
  var maxY: Option[Int] = None
}
