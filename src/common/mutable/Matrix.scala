package common.mutable

import common.Pos
import scala.collection.mutable
import scala.reflect.ClassTag

class Matrix[T](vs: Iterator[Iterable[T]]) {
  val data: Array[mutable.Seq[T]] = vs.map{vs => mutable.Seq.empty[T] ++ vs }.toArray
  val rows: Int = data.length
  val cols: Int = if (data.nonEmpty) data(0).length else 0
  def get(i: Int, j: Int): Option[T] = if (i >= 0 && i < rows && j >= 0 && j < cols) { Some(data(i)(j)) } else None
  def get(pos: Pos): Option[T] = get(pos.row, pos.col)
  def getOrElse(i: Int, j: Int, default: => T): T = get(i, j).getOrElse(default)
  def getOrElse(pos: Pos, default: => T): T = get(pos).getOrElse(default)
  def apply(i: Int, j: Int): T = data(i)(j)
  def apply(pos: Pos): T = data(pos.row)(pos.col)
  def update(i: Int, j: Int, value: T): Unit = if (i >= 0 && i < rows && j >= 0 && j < cols) { data(i)(j) = value }
  def update(i: Int, j: Int, value: Option[T]): Unit = value.foreach{v => update(i, j, v) }

  def update(pos: Pos, value: T): Unit = update(pos.row, pos.col, value)

  def indices(): Seq[(Int,Int)] = (0 until rows).flatMap{i => (0 until cols).map{j => (i,j) }}

  def posIterator(): Iterator[Pos] = (0 until rows).iterator.flatMap{i => (0 until cols).map{j => Pos(i,j) }}

  def mapIndices[R](func: (Int,Int) => R): Matrix[R] = Matrix(
    (0 until rows).iterator.map{i =>
      (0 until cols).map{j => func(i,j) }
    }
  )

  def sum(implicit num: Numeric[T], ct: ClassTag[T]): T = data.map(_.reduce(num.plus)).reduce(num.plus)

  override def toString: String = data.map{vs => vs.map{x => Matrix.digit(x)}.mkString("")}.mkString("\n")
}
object Matrix {
  def apply[T](vs: Iterator[Iterable[T]]): Matrix[T] = new Matrix(vs)
  def empty[T](rows: Int, cols: Int, default: T): Matrix[T] = new Matrix((0 until rows).iterator.map{i =>
    Iterable.fill(cols)(default)
  })
  def digit[T](x: T): String = x match {
    case x: Int => val str = Integer.toHexString(x); if (str.length > 1) "N" else str
    case _ => x.toString
  }
}
