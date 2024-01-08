package common.mutable

import common.immutable.Pos.Idx
import common.immutable.{Constructible, StaticTensorOps, Volume}

import scala.language.implicitConversions
import scala.reflect.ClassTag

class Matrix[T](volume: Volume[Int], data: Array[T])
  extends common.immutable.Matrix[T](volume, data) with MutableTensorLike[T]

object Matrix extends StaticTensorOps[Matrix] {
  def apply[A](volume: Volume[Int], data: Array[A]): Matrix[A] = new Matrix(volume, data)
  def empty[A:ClassTag](rows: Int, cols: Int, value: A): Matrix[A] = apply(Idx(rows, cols), Array.fill(rows*cols)(value))
  implicit def matrixIsConstructible[A]: Constructible[A,Matrix[A]] = apply
  implicit def matrixIsMutableTensor[A](matrix: Matrix[A]): Tensor[A] = Tensor(matrix.vol, matrix.rawData)
}
