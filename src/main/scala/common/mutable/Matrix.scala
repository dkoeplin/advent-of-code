package common.mutable

import common.immutable.Box

class Matrix[A](vol: Box[Int], data: Array[A]) extends common.immutable.Matrix[A](vol, data) with MutableTensor[A]

object Matrix extends common.immutable.StaticTensorMethods[Matrix] {
  def apply[A](volume: Box[Int], data: Array[A]): Matrix[A] = new Matrix(volume, data)
}
