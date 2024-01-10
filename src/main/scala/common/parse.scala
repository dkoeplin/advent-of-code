package common

import common.immutable.Pos.Idx
import common.immutable.{Pos, TensorView, Volume}

object parse {
  def apply[A](file: scala.io.BufferedSource)(func: Char => A): TensorView[A] = apply(file.getLines())(func)
  def apply[A](lines: IterableOnce[String])(func: Char => A): TensorView[A] = {
    val (x, y) = lines.iterator.duplicate
    val cols = x.nextOption().map(_.length).getOrElse(1)
    val rows = x.size + 1
    new TensorView(Volume(Pos.zero[Int](2), Idx(rows - 1, cols - 1)), y.flatten.map(func))
  }

  def chars(file: scala.io.BufferedSource): TensorView[Char] = parse(file)(identity)
  def chars(lines: IterableOnce[String]): TensorView[Char] = parse(lines)(identity)

  def digits(file: scala.io.BufferedSource): TensorView[Int] = parse(file)(_.asDigit)
  def digits(lines: IterableOnce[String]): TensorView[Int] = parse(lines)(_.asDigit)
}
