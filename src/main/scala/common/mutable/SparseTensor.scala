package common.mutable

import common.immutable.Pos.Idx
import common.immutable.{Pos, Volume}

class SparseTensor[T](rank: Int, default: T) {
  def volume: Volume[Int] = vol.getOrElse(Volume.unit(Pos.zero[Int](rank)))
  def min: Idx = volume.min
  def max: Idx = volume.max

  def apply(x: Idx): T = map.getOrElse(x, default)

  def update(x: Idx, value: T): Unit = {
    vol = vol.map(_ union Volume.unit(x)).orElse(Some(Volume.unit(x)))
    map(x) = value
  }

  def keys: Iterator[Idx] = map.keysIterator

  private val map = scala.collection.mutable.Map.empty[Idx, T]
  private var vol: Option[Volume[Int]] = None
}
