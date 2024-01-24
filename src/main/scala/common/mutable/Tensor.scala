package common.mutable

import common.immutable.Box
import common.immutable.Pos.Idx

trait MutableTensor[A] { this: common.immutable.Tensor[A] =>
  def update(i: Idx, elem: A): Unit = raw(flatten(i)) = elem
  def updateIf(i: Idx, elem: A): Unit = if (has(i)) update(i, elem)
  def update(i: Idx, elem: Option[A]): Unit = elem.foreach{x => update(i, x) }
}

class Tensor[A](vol: Box[Int], data: Array[A]) extends common.immutable.Tensor[A](vol, data) with MutableTensor[A]

object Tensor extends common.immutable.StaticTensorMethods[Tensor] {
  def apply[A](volume: Box[Int], data: Array[A]): Tensor[A] = new Tensor(volume, data)
}
