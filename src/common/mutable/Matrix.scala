package common.mutable

import scala.collection.mutable

class Matrix[T](vs: Iterator[Iterable[T]]) {
  val data: Array[mutable.Seq[T]] = vs.map{vs => mutable.Seq.empty[T] ++ vs }.toArray
  val rows: Int = data.length
  val cols: Int = if (data.nonEmpty) data(0).length else 0
  def get(i: Int, j: Int): Option[T] = if (i >= 0 && i < rows && j >= 0 && j < cols) { Some(data(i)(j)) } else None
  def getOrElse(i: Int, j: Int, default: => T): T = get(i, j).getOrElse(default)
  def apply(i: Int, j: Int): T = data(i)(j)
  def update(i: Int, j: Int, value: T): Unit = if (i >= 0 && i < rows && j >= 0 && j < cols) { data(i)(j) = value }
  def update(i: Int, j: Int, value: Option[T]): Unit = value.foreach{v => update(i, j, v) }

  def indices(): Seq[(Int,Int)] = (0 until rows).flatMap{i => (0 until cols).map{j => (i,j) }}

  def mapIndices[R](func: (Int,Int) => R): Matrix[R] = Matrix(
    (0 until rows).iterator.map{i =>
      (0 until cols).map{j => func(i,j) }
    }
  )

  def sum(implicit num: Numeric[T]): T = data.map(_.reduce(num.plus)).reduce(num.plus)

  override def toString: String = data.map{vs => vs.map{x => Matrix.digit(x)}.mkString("")}.mkString("\n")
}
object Matrix {
  def apply[T](vs: Iterator[Iterable[T]]): Matrix[T] = new Matrix(vs)
  def digit[T](x: T): String = x match {
    case x: Int => val str = Integer.toHexString(x); if (str.length > 1) "N" else str
    case _ => x.toString
  }
}
