package common.immutable

import common.implicits.IteratorOps.zipped

import scala.collection.immutable.NumericRange
import scala.math.Ordered.orderingToOrdered

/**
 * Defines a N-dimensional rectangular volume from min to max, inclusive.
 */
class Cube[T](val l: Pos[T], val r: Pos[T])(implicit num: Numeric[T]) extends Volume[T] {
  import num._
  val min: Pos[T] = l min r
  val max: Pos[T] = l max r
  lazy val shape: Pos[T] = Pos(min.iterator.zip(max.iterator).map{case (a, b) => b - a + num.one })

  def size: T = shape.product
  def rank: Int = min.rank
  def H: T = shape.getOrElse(rank - 2, num.one)
  def W: T = shape.getOrElse(rank - 1, num.one)

  override def equals(obj: Any): Boolean = obj match {
    case v: Cube[_] => min == v.min && max == v.max
    case _ => false
  }
  def ==(rhs: Cube[T]): Boolean = min == rhs.min && max == rhs.max
  def !=(rhs: Cube[T]): Boolean = !(this == rhs)
  override def hashCode(): Int = (min, max).hashCode()

  def +(rhs: Pos[T]): Cube[T] = Cube(min + rhs, max + rhs)
  def -(rhs: Pos[T]): Cube[T] = Cube(min - rhs, max - rhs)

  def *(rhs: T): Cube[T] = Cube(min, max * rhs) // TODO: Both?
  def *(rhs: Pos[T]): Cube[T] = Cube(min * rhs, max * rhs)

  def overlaps(rhs: Cube[T]): Boolean
    = zipped(min.iterator, max.iterator, rhs.min.iterator, rhs.max.iterator)
    .forall{case Seq(amin, amax, bmin, bmax) => bmin <= amax && bmax >= amin }

  def iterator(implicit int: Integral[T]): Iterator[Pos[T]] = Cube.anyIterator(this, reverse = false)
  def reverseIterator(implicit int: Integral[T]): Iterator[Pos[T]] = Cube.anyIterator(this, reverse = true)

  def union(rhs: Cube[T]): Cube[T] = Cube(min.min(rhs.min), max.max(rhs.max))
  def moveTo(dim: Int, pos: T): Cube[T] = {
    val diff = Pos.unit(rank, dim) * (pos - min(dim))
    Cube(min + diff, max + diff)
  }

  def contains(pos: Pos[T]): Boolean = pos isIn this

  /// Returns a slice of this volume over just the given dimension(s).
  def keepDims(dims: Set[Int]): Cube[T] = Cube(min.keepDims(dims), max.keepDims(dims))
  def keepDims(dim: Int*): Cube[T] = keepDims(dim.toSet)

  def rows: Cube[T] = keepDims(rank - 2)
  def cols: Cube[T] = keepDims(rank - 1)

  /// Returns a "slice" of this volume with the given dimension(s) dropped.
  def dropDims(dims: Set[Int]): Cube[T] = Cube(min.dropDims(dims), max.dropDims(dims))
  def dropDims(dim: Int*): Cube[T] = dropDims(dim.toSet)


  /// Returns a new Volume with an innermost (last) dimension with the range [a,b]
  def expand(a: T, b: T): Cube[T] = Cube(min.append(a), max.append(b))

  /// Returns a copy with the given dimension changed to the [a, b]
  def alter(dim: Int, a: T, b: T): Cube[T] = Cube(min.alter(dim, a), max.alter(dim, b))

  /// Returns a new Volume with a dimension inserted at the given index
  def insert(dim: Int, a: T, b: T): Cube[T] = Cube(min.insert(dim, a), max.insert(dim, b))

  def t: Cube[T] = Cube(min.t, max.t)

  def edges: Iterator[Cube[T]] = (0 until rank).flatMap{ dim =>
    Seq(alter(dim, min(dim), min(dim)), alter(dim, max(dim), max(dim)))
  }.toSet.iterator

  def borders(implicit int: Integral[T]): Iterator[Cube[T]] = borders(int.one)
  def borders(width: T)(implicit num: Numeric[T]): Iterator[Cube[T]] = dirsAndBorders(width).map(_._2)

  def dirsAndBorders(width: T)(implicit num: Numeric[T]): Iterator[(Pos[T], Cube[T])]
    = (0 until rank).iterator.flatMap { dim =>
      val dir = Pos.unit[T](rank, dim)
      val delta = dir * width
      Iterator((-dir, Cube(min - delta, max.alter(dim, min(dim)))), (dir, Cube(min.alter(dim, max(dim)), max + delta)))
    }

  def above(width: T = implicitly[Numeric[T]].one, delta: T = implicitly[Numeric[T]].one): Cube[T]
    = Cube(min - Pos.unit[T](rank, rank - 1) * width, max.alter(rank - 1, min(rank - 1) - delta))

  def intersect(rhs: Cube[T]): Option[Cube[T]]
    = if (!overlaps(rhs)) None else Some(Cube(min max rhs.min, max min rhs.max))

  def diff(rhs: Cube[T]): Set[Cube[T]] = intersect(rhs) match {
    case Some(union) =>
      val one = implicitly[Numeric[T]].one
      Cube.deltas(rank).flatMap{ delta =>
        val (x: Iterator[(T,T)], y: Iterator[(T,T)]) = delta.iterator.zipWithIndex.map{
          case (-1, d) => (min(d), union.min(d) - one)
          case (0, d)  => (union.min(d), union.max(d))
          case (1, d)  => (union.max(d) + one, max(d))
        }.duplicate
        Cube.get(Pos(x.map(_._1)), Pos(y.map(_._2)))
      }.toSet
    case None => Set(this)
  }

  def diff(rhs: IterableOnce[Cube[T]]): Set[Cube[T]]
    = rhs.iterator.foldLeft(Set(this)){(set, v) => set.flatMap(_ diff v) }

  def toDoubles: Cube[Double] = Cube(min.toDoubles, max.toDoubles)
  def toInts: Cube[Int] = Cube(min.toInts, max.toInts)

  override def toString: String = s"($min to $max)"
}

object Cube {
  private def anyIterator[T](volume: Cube[T], reverse: Boolean)(implicit int: Integral[T]): Iterator[Pos[T]] = {
    volume.min.iterator.zip(volume.max.iterator).foldLeft(Iterator.empty[Seq[T]]){case (iters, (min, max)) =>
      val start = if (reverse) max else min
      val end = if (reverse) min else max
      val step = if (end >= start) 1 else -1
      val range = NumericRange.inclusive(start, end, int.fromInt(step))
      if (!iters.hasNext) range.iterator.map(Seq.apply(_)) else iters.flatMap{idx => range.iterator.map{x => idx :+ x }}
    }.map(_.iterator).map{seq => Pos[T](seq) }
  }

  def unit[T:Numeric](at: Pos[T]): Cube[T] = new Cube(at, at)
  def apply[T:Numeric](p0: Pos[T], p1: Pos[T]): Cube[T] = new Cube(p0, p1)

  def deltas(rank: Int): Iterator[Pos[Int]]
    = Cube(Pos.fill(rank, -1), Pos.fill(rank, 1)).iterator.filterNot(_.iterator.forall(_ == 0))

  def get[T](min: Pos[T], max: Pos[T])(implicit num: Numeric[T]): Option[Cube[T]] = {
    if (min.iterator.zip(max.iterator).forall{case (a, b) => a <= b }) Some(Cube(min, max)) else None
  }

  implicit class IntegralVolume[T](a: Cube[T])(implicit int: Integral[T]) {
    def /(b: T): Cube[T] = Cube(a.min / b, a.max / b)
    def /(b: Pos[T]): Cube[T] = Cube(a.min / b, a.max / b)
  }
  implicit class FractionalVolume[T](a: Cube[T])(implicit f: Fractional[T]) {
    def /(b: T): Cube[T] = Cube(a.min / b, a.max / b)
    def /(b: Pos[T]): Cube[T] = Cube(a.min / b, a.max / b)
    def roundInt: Cube[Int] = Cube(a.min.roundInt, a.max.roundInt)
  }
}