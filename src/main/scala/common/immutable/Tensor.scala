package common.immutable

import common.immutable.Pos.Idx

import scala.language.implicitConversions
import scala.reflect.ClassTag

abstract class TensorLike[T](volume: Volume[Int], data: IterableOnce[T]) {
  def vol: Volume[Int] = volume

  def rank: Int = volume.rank
  def H: Int = volume.shape.getOrElse(rank - 2, 1)
  def W: Int = volume.shape.getOrElse(rank - 1, 1)

  def has(pos: Idx): Boolean = pos isIn volume

  def iterator: Iterator[T] = data.iterator

  def indices: Iterator[Idx] = volume.iterator
  def reverseIndices: Iterator[Idx] = volume.reverseIterator

  def sum(implicit num: Numeric[T]): T = iterator.reduce(num.plus)

  def indexWhere(cond: T => Boolean): Option[Idx] = iterator.zip(indices).find{case (d,_) => cond(d) }.map(_._2)
}

case class TensorView[A](volume: Volume[Int], data: Iterator[A]) extends TensorLike[A](volume, data) {
  def to[T](implicit a: ClassTag[A], c: Constructible[A,T]): T = c(volume, data.toArray)
  def to[T[B]<:Tensor[B]](implicit a: ClassTag[A], c: Constructible[A,T[A]]): T[A] = c(volume, data.toArray)
}
object TensorView {
  implicit def make[A:ClassTag,T](view: TensorView[A])(implicit c: Constructible[A,T]): T = view.to[T]
}

class Tensor[A](volume: Volume[Int], data: Array[A]) extends TensorLike[A](volume, data) {
  protected def array: Array[A] = data
  protected def flatten(i: Idx): Int = ((i - volume.min) * strides).sum

  def rawData: Array[A] = data

  def shape: Pos[Int] = volume.shape
  def strides: Pos[Int] = volume.shape.strides

  def get(pos: Idx): Option[A] = if (has(pos)) Some(apply(pos)) else None
  def getOrElse(inds: Int*)(default: => A): A = get(Pos(inds)).getOrElse(default)
  def getOrElse(pos: Idx, default: => A): A = get(pos).getOrElse(default)
  def apply(inds: Int*): A = apply(Pos(inds))
  def apply(i: Idx): A = data(flatten(i))

  def reverseIterator: Iterator[A] = data.reverseIterator
  def lastIndexWhere(cond: A => Boolean): Option[Idx]
    = reverseIterator.zip(reverseIndices).find{case (x,_) => cond(x) }.map(_._2)

  def t: TensorView[A] = new TensorView[A](volume.t, volume.t.iterator.map(_.t).map(apply))
}

trait Constructible[A,T] {
  def apply(volume: Volume[Int], data: Array[A]): T
}

class TensorLikeOps[A,T<:TensorLike[A]](tensor: T) {
  def zip[R](rhs: T)(func: (A, A) => R): TensorView[R]
  = new TensorView(tensor.vol, tensor.iterator.zip(rhs.iterator).map{case (a, b) => func(a, b) })
  def map[R](func: A => R): TensorView[R] = new TensorView[R](tensor.vol, tensor.iterator.map(func))
  def mapIndices[R](func: Idx => R): TensorView[R] = new TensorView[R](tensor.vol, tensor.indices.map(func))

  def +(rhs: T)(implicit num: Numeric[A]): TensorView[A] = zip(rhs)(num.plus)
  def -(rhs: T)(implicit num: Numeric[A]): TensorView[A] = zip(rhs)(num.minus)
  def *(rhs: T)(implicit num: Numeric[A]): TensorView[A] = zip(rhs)(num.times)
}

trait StaticTensorOps[T[X]<:Tensor[X]] {
  def apply[A](volume: Volume[Int], data: Array[A]): T[A]
  def apply[A](shape: Idx, data: Array[A]): T[A]
    = apply(Volume(Pos.zero[Int](shape.rank), shape - 1), data) // min and max are inclusive in volume

  def apply[A:ClassTag](iter: Iterator[Iterable[A]]): T[A] = {
    val data = iter.toArray
    val rows = data.length
    val cols = data.headOption.map(_.size).getOrElse(0)
    apply(Volume(Pos.zero[Int](2), Idx(rows, cols)), data.flatten)
  }

  def apply(file: scala.io.BufferedSource): T[Char]
    = apply(file.getLines().map(_.toArray) : Iterator[Iterable[Char]])
}
object Tensor extends StaticTensorOps[Tensor] {
  def apply[A](volume: Volume[Int], data: Array[A]): Tensor[A] = new Tensor(volume, data)
  implicit def tensorIsConstructible[A]: Constructible[A, Tensor[A]] = Tensor.apply
}
