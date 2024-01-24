package common.mutable

import common.immutable.Pos.Idx
import common.immutable.{Box, Pos}

class SparseTensor[T](rank: Int, default: T) {
  def volume: Box[Int] = vol.getOrElse(Box.unit(Pos.zero[Int](rank)))
  def min: Idx = volume.min
  def max: Idx = volume.max

  def apply(x: Idx): T = map.getOrElse(x, default)

  def update(x: Idx, value: T): Unit = {
    vol = vol.map(_ union Box.unit(x)).orElse(Some(Box.unit(x)))
    map(x) = value
  }

  def keys: Iterator[Idx] = map.keysIterator

  private val map = scala.collection.mutable.Map.empty[Idx, T]
  private var vol: Option[Box[Int]] = None
}
