package common.mutable

import common.Pos

case class SparseGrid[T](default: T) {
  def apply(i: Int, j: Int): T = map.getOrElse(Pos(i, j), default)
  def apply(x: Pos): T = map.getOrElse(x, default)
  def update(x: Pos, value: T): Unit = {
    min = min.map(_ min x).orElse(Some(x))
    max = max.map(_ max x).orElse(Some(x))
    map(x) = value
  }
  def update(i: Int, j: Int, value: T): Unit = update(Pos(i, j), value)

  def start(): Pos = min.get
  def end(): Pos = max.get

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

  def keys: Iterator[Pos] = map.keysIterator

  private val map = scala.collection.mutable.Map.empty[Pos, T]
  private var min: Option[Pos] = None
  private var max: Option[Pos] = None
}
