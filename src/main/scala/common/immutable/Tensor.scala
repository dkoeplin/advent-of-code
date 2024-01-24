package common.immutable

import common.immutable.Pos.Idx

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait Constructable[A,T] {
  def apply(volume: Box[Int], data: Array[A]): T
}

class TensorView[A](vol: Box[Int], data: IterableOnce[A]) {
  def shape: Pos[Int] = vol.shape
  def strides: Pos[Int] = vol.shape.strides

  def volume: Box[Int] = vol
  def iterator: Iterator[A] = data.iterator

  def rank: Int = vol.rank
  def H: Int = vol.shape.getOrElse(rank - 2, 1)
  def W: Int = vol.shape.getOrElse(rank - 1, 1)

  def has(pos: Idx): Boolean = pos isIn vol

  def sum(implicit num: Numeric[A]): A = iterator.sum

  def indices: Iterator[Idx] = vol.iterator
  def reverseIndices: Iterator[Idx] = vol.reverseIterator

  def indexWhere(cond: A => Boolean): Option[Idx] = iterator.zip(indices).find{case (d,_) => cond(d) }.map(_._2)

  def zip[B,R,C<:TensorView[B]](rhs: C)(func: (A, B) => R): TensorView[R]
  = new TensorView(vol, iterator.zip(rhs.iterator).map{case (a, b) => func(a, b) })
  def map[R](func: A => R): TensorView[R] = new TensorView[R](vol, iterator.map(func))
  def mapIndices[R](func: Idx => R): TensorView[R] = new TensorView[R](vol, indices.map(func))

  def +(rhs: TensorView[A])(implicit num: Numeric[A]): TensorView[A] = zip[A,A,TensorView[A]](rhs)(num.plus)
  def -(rhs: TensorView[A])(implicit num: Numeric[A]): TensorView[A] = zip[A,A,TensorView[A]](rhs)(num.minus)
  def *(rhs: TensorView[A])(implicit num: Numeric[A]): TensorView[A] = zip[A,A,TensorView[A]](rhs)(num.times)

  def to[T[X]<:TensorView[X]](implicit a: ClassTag[A], c: Constructable[A,T[A]]): T[A] = c(vol, iterator.toArray(a))
  def to[T](implicit a: ClassTag[A], c: Constructable[A,T]): T = c(vol, iterator.toArray(a))
}
object TensorView {
  implicit def make[A:ClassTag,T](view: TensorView[A])(implicit c: Constructable[A,T]): T = view.to[T]
}

class Tensor[A](vol: Box[Int], data: Array[A]) extends TensorView[A](vol, data) {
  protected def flatten(i: Idx): Int = ((i - vol.min) * strides).sum

  def raw: Array[A] = data

  def apply(i: Idx): A = data(flatten(i))
  def get(pos: Idx): Option[A] = if (has(pos)) Some(apply(pos)) else None
  def getOrElse(indices: Int*)(default: => A): A = get(Pos(indices)).getOrElse(default)
  def getOrElse(pos: Idx, default: => A): A = get(pos).getOrElse(default)
  //  def apply(indices: Int*): A = apply(Pos(indices))

  def reverseIterator: Iterator[A] = data.reverseIterator
  def lastIndexWhere(cond: A => Boolean): Option[Idx]
  = reverseIterator.zip(reverseIndices).find{case (x,_) => cond(x) }.map(_._2)

  def t: TensorView[A] = new TensorView[A](vol.t, vol.t.iterator.map(_.t).map(apply))
}

trait StaticTensorMethods[T[X]<:Tensor[X]] {
  def apply[A](volume: Box[Int], data: Array[A]): T[A]
  def apply[A](shape: Idx, data: Array[A]): T[A] = apply(Box(Pos.zero[Int](shape.rank), shape - 1), data)
  def apply[A:ClassTag](iter: Iterator[Iterable[A]]): T[A] = {
    val data = iter.toArray
    val rows = data.length
    val cols = data.headOption.map(_.size).getOrElse(0)
    apply(shape = Idx(rows, cols), data.flatten)
  }

  def fill[A:ClassTag](shape: Idx, value: A): T[A] = apply(shape, Array.fill(shape.product)(value))

  private class StaticConstructor[A](x: StaticTensorMethods[T]) extends Constructable[A,T[A]] {
    def apply(vol: Box[Int], data: Array[A]): T[A] = x(vol, data)
  }
  implicit def constructable[A]: Constructable[A,T[A]] = new StaticConstructor[A](this)
}

object Tensor extends StaticTensorMethods[Tensor] {
  def apply[A](volume: Box[Int], data: Array[A]): Tensor[A] = new Tensor(volume, data)

  // move me
  implicit class Tuple3Ops(x: (scala.Range, scala.Range, scala.Range)) {
    def apply[A:ClassTag](func: (Int, Int, Int) => A): TensorView[A] = {
      val volume = Box(Pos(x._1.min, x._2.min, x._3.min), Pos(x._1.max, x._2.max, x._3.max))
      new TensorView(volume, volume.iterator.map { case Pos(x, y, z) => func(x, y, z) })
    }
  }
}
