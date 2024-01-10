package common.mutable

import common.immutable.Volume

class Matrix[A](vol: Volume[Int], data: Array[A]) extends common.immutable.Matrix[A](vol, data) with MutableTensor[A]

object Matrix extends common.immutable.StaticTensorMethods[Matrix] {
  def apply[A](volume: Volume[Int], data: Array[A]): Matrix[A] = new Matrix(volume, data)
}
