package common.immutable

import common.immutable.Pos.Idx

class Matrix[A](vol: Volume[Int], data: Array[A]) extends Tensor[A](vol, data) {
  def wIterator: Iterator[Int] = vol.cols.iterator.map(_.x)
  def hIterator: Iterator[Int] = vol.rows.iterator.map(_.x)

  def rows: Iterator[Iterator[A]] = hIterator.map{i => row(i) }
  def cols: Iterator[Iterator[A]] = wIterator.map{j => col(j) }

  def row(i: Int): Iterator[A] = vol.alter(rank - 2, i, i).iterator.map(apply)
  def col(j: Int): Iterator[A] = vol.alter(rank - 1, j, j).iterator.map(apply)

  /// Create a new (printable) Matrix using the function func on every position in this matrix.
  def printable(func: A => Char): Matrix[Char] = new Matrix[Char](vol, iterator.map(func).toArray)

  /// Create a new annotated, printable Matrix using the function func on every position in this matrix.
  /// Adds row and column label numbers for help with debugging.
  def annotated(func: Idx => Char): Matrix[Char] = {
    val log10W = Math.ceil(Math.log10(W)).toInt
    val log10H = Math.ceil(Math.log10(H)).toInt
    val paddedH = H + log10W
    val paddedW = W + log10H
    val volume = Volume[Int](Pos(0, 0), Pos(paddedH, paddedW))
    new Matrix[Char](volume, volume.iterator.map{case Pos(h, w) =>
      lazy val hStr = (h - 2).toString
      lazy val hPad = " " * (log10H - hStr.length) + hStr
      lazy val wStr = (w - 2).toString
      lazy val wPad = " " * (log10W - wStr.length) + wStr
      if (h < log10W && w < log10H) ' '
      else if (h < log10W) wPad.charAt(h)
      else if (w < log10H) hPad.charAt(w)
      else func(Idx(h - log10W, w - log10H))
    }.toArray)
  }

  override def toString: String = data.grouped(W).map{_.map(Matrix.digit).mkString("")}.mkString("\n")
}
object Matrix extends StaticTensorMethods[Matrix] {
  def apply[A](volume: Volume[Int], data: Array[A]): Matrix[A] = new Matrix(volume, data)

  def digit[T](x: T): String = x match {
    case x: Int => val str = Integer.toHexString(x); if (str.length > 1) "N" else str
    case _ => x.toString
  }
}
