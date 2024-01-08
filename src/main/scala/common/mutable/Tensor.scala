package common.mutable

import common.immutable.Pos.Idx
import common.immutable.Volume

trait MutableTensorLike[T] { this: common.immutable.Tensor[T] =>
  def update(i: Idx, elem: T): Unit = array(flatten(i)) = elem
  def updateIf(i: Idx, elem: T): Unit = if (has(i)) update(i, elem)
  def update(i: Idx, elem: Option[T]): Unit = elem.foreach{x => update(i, x) }
}

class Tensor[T](volume: Volume[Int], data: Array[T])
  extends common.immutable.Tensor[T](volume, data) with MutableTensorLike[T]

object Tensor extends common.immutable.StaticTensorOps[Tensor] {
  def apply[A](volume: Volume[Int], data: Array[A]): Tensor[A] = new Tensor(volume, data)
}
