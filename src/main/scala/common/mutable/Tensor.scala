package common.mutable

import scala.collection.mutable
import scala.language.implicitConversions

case class Tensor[T](shape: Seq[Int], zero: T) {
  val numElems: Int = shape.product
  val strides: Seq[Int] = shape.indices.map{i => shape.drop(i + 1).product }
  val data: mutable.Buffer[T] = mutable.Buffer.fill(numElems)(zero)
  val rank: Int = shape.length

  def apply(i: Int*): T = data(flatten(i))
  def get(i: Int*): Option[T] = getFlatten(i).map{index => data(index)}
  def getOrElse(i0: Int, elem: T): T = get(i0).getOrElse(elem)
  def getOrElse(i0: Int, i1: Int, elem: T): T = get(i0, i1).getOrElse(elem)
  def getOrElse(i0: Int, i1: Int, i2: Int, elem: T): T = get(i0, i1, i2).getOrElse(elem)
  def getOrElse(i0: Int, i1: Int, i2: Int, i3: Int, elem: T): T = get(i0, i1, i2, i3).getOrElse(elem)
  def getOrElse(i0: Int, i1: Int, i2: Int, i3: Int, i4: Int, elem: T): T = get(i0, i1, i2, i3, i4).getOrElse(elem)
  def getOrElse(i: Seq[Int], elem: T): T = get(i:_*).getOrElse(elem)

  def update(i0: Int, elem: T): Unit = update(Seq(i0), elem)
  def update(i0: Int, i1: Int, elem: T): Unit =  update(Seq(i0, i1), elem)
  def update(i0: Int, i1: Int, i2: Int, elem: T): Unit = update(Seq(i0, i1, i2), elem)
  def update(i0: Int, i1: Int, i2: Int, i3: Int, elem: T): Unit = update(Seq(i0, i1, i2, i3), elem)
  def update(i0: Int, i1: Int, i2: Int, i3: Int, i4: Int, elem: T): Unit =  update(Seq(i0, i1, i2, i3, i4), elem)
  def update(i: Seq[Int], elem: T): Unit = data(flatten(i)) = elem

  def updateIf(i0: Int, elem: T): Unit = updateIf(Seq(i0), elem)
  def updateIf(i0: Int, i1: Int, elem: T): Unit =  updateIf(Seq(i0, i1), elem)
  def updateIf(i0: Int, i1: Int, i2: Int, elem: T): Unit = updateIf(Seq(i0, i1, i2), elem)
  def updateIf(i0: Int, i1: Int, i2: Int, i3: Int, elem: T): Unit = updateIf(Seq(i0, i1, i2, i3), elem)
  def updateIf(i0: Int, i1: Int, i2: Int, i3: Int, i4: Int, elem: T): Unit =  updateIf(Seq(i0, i1, i2, i3, i4), elem)
  def updateIf(i: Seq[Int], elem: T): Unit = getFlatten(i).foreach{index => data(index) = elem }

  def sum(implicit num: Numeric[T]): T = data.sum(num)

  private def getFlatten(i: Seq[Int]): Option[Int] = {
    assert(shape.length == i.length)
    if (i.zip(shape).forall{case (i,dim) => i >= 0 && i < dim }) Some(flatten(i)) else None
  }
  private def flatten(i: Seq[Int]): Int = {
    assert(shape.length == i.length)
    i.zip(strides).map{case (i,stride) => i * stride}.sum
  }
}

object Tensor {
  def apply[T:Numeric](shape: Int*): Tensor[T] = Tensor(shape, implicitly[Numeric[T]].zero)
}
