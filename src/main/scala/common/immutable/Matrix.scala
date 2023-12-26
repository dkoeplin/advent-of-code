package common.immutable

import common.Pos
import scala.collection.mutable
import scala.reflect.ClassTag

class Matrix[T](vs: Iterator[Iterable[T]]) {
  val data: Array[Seq[T]] = vs.map{vs => Seq.empty[T] ++ vs }.toArray
  val rows: Int = data.length
  val cols: Int = if (data.nonEmpty) data(0).length else 0
  def has(pos: Pos): Boolean = get(pos).nonEmpty
  def get(i: Int, j: Int): Option[T] = if (i >= 0 && i < rows && j >= 0 && j < cols) { Some(data(i)(j)) } else None
  def get(pos: Pos): Option[T] = get(pos.row, pos.col)
  def getOrElse(i: Int, j: Int, default: => T): T = get(i, j).getOrElse(default)
  def getOrElse(pos: Pos, default: => T): T = get(pos).getOrElse(default)
  def apply(i: Int, j: Int): T = data(i)(j)
  def apply(pos: Pos): T = data(pos.row)(pos.col)
  def contains(pos: Pos): Boolean = get(pos).nonEmpty

  def colIndices: Iterator[Int] = (0 until cols).iterator
  def rowIndices: Iterator[Int] = (0 until rows).iterator


  def iterateOverRows(reverse: Boolean = false): Iterator[Iterator[T]]
    = (0 until rows).iterator.map{i => row(i, reverse) }
  def iterateOverCols(reverse: Boolean = false): Iterator[Iterator[T]]
    = (0 until cols).iterator.map{j => col(j, reverse) }

  def row(i: Int, reverse: Boolean = false): Iterator[T]
    = (if (reverse) (0 until cols).reverseIterator else (0 until cols).iterator).map{j => apply(i, j) }
  def col(j: Int, reverse: Boolean = false): Iterator[T]
    = (if (reverse) (0 until rows).reverseIterator else (0 until rows).iterator).map{i => apply(i, j) }

  def indices(): Seq[(Int,Int)] = (0 until rows).flatMap{i => (0 until cols).map{j => (i,j) }}

  def posIterator(): Iterator[Pos] = (0 until rows).iterator.flatMap{i => (0 until cols).map{j => Pos(i,j) }}
  def reverseIterator: Iterator[Pos] = (rows - 1 to 0 by -1).iterator.flatMap{i => (cols - 1 to 0 by -1).map{j => Pos(i,j) }}

  def mapIndices[R](func: (Int,Int) => R): Matrix[R] = Matrix(
    (0 until rows).iterator.map{i =>
      (0 until cols).map{j => func(i,j) }
    }
  )
  def mapPos[R](func: Pos => R): Matrix[R] = Matrix(
    (0 until rows).iterator.map{i =>
      (0 until cols).map{j => func(Pos(i, j)) }
    }
  )

  def follow(start: Pos, delta: Pos): Iterator[Pos] = {
    val dR = if (delta.row == 0) 1 else delta.row
    val dC = if (delta.col == 0) 1 else delta.col
    (start.row to start.row + delta.row by dR).iterator.flatMap{i =>
      (start.col to start.col + delta.col by dC).iterator.map{j => Pos(i, j) }
    }
  }

  def find(cond: T => Boolean): Option[Pos] = posIterator().find{p => cond(apply(p)) }

  def sum(implicit num: Numeric[T], ct: ClassTag[T]): T = data.map(_.reduce(num.plus)).reduce(num.plus)

  def t: Iterator[Iterable[T]] = (0 until cols).iterator.map{j =>
    (0 until rows).map{i => apply(i, j) }
  }

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
