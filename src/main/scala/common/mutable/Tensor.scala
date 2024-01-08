package common.mutable

import common.immutable.Pos.Idx
import common.immutable.Volume

trait MutableTensorLike[A] { this: common.immutable.Tensor[A] =>
  def update(i: Idx, elem: A): Unit = array(flatten(i)) = elem
  def updateIf(i: Idx, elem: A): Unit = if (has(i)) update(i, elem)
  def update(i: Idx, elem: Option[A]): Unit = elem.foreach{x => update(i, x) }
}

class Tensor[A](volume: Volume[Int], data: Array[A])
  extends common.immutable.Tensor[A](volume, data) with MutableTensorLike[A]

object Tensor extends common.immutable.StaticTensorOps[Tensor] {
  def apply[A](volume: Volume[Int], data: Array[A]): Tensor[A] = new Tensor(volume, data)
}
