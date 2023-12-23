package common.mutable

import common.Pos3

case class Sparse3D[T](default: T) {
  def apply(i: Int, j: Int, z: Int): T = map.getOrElse(Pos3(i, j, z), default)

  def apply(x: Pos3): T = map.getOrElse(x, default)

  def update(x: Pos3, value: T): Unit = {
    min = min.map(_ min x).orElse(Some(x))
    max = max.map(_ max x).orElse(Some(x))
    map(x) = value
  }

  def update(i: Int, j: Int, z: Int, value: T): Unit = update(Pos3(i, j, z), value)

  def start(): Pos3 = min.get

  def end(): Pos3 = max.get

  private val map = scala.collection.mutable.Map.empty[Pos3, T]
  private var min: Option[Pos3] = None
  private var max: Option[Pos3] = None
}
