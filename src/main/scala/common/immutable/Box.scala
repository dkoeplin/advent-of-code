package common.immutable

import common.implicits.IteratorOps.zipped

import scala.collection.immutable.NumericRange
import scala.math.Ordered.orderingToOrdered

/**
 * Defines a N-dimensional rectangular volume from min to max, inclusive.
 */
class Box[T](val l: Pos[T], val r: Pos[T])(implicit num: Numeric[T]) extends Volume[T] {
  import num._
  val min: Pos[T] = l min r
  val max: Pos[T] = l max r
  lazy val shape: Pos[T] = Pos(min.iterator.zip(max.iterator).map{case (a, b) => b - a + num.one })

  def size: T = shape.product
  def rank: Int = min.rank
  def H: T = shape.getOrElse(rank - 2, num.one)
  def W: T = shape.getOrElse(rank - 1, num.one)

  override def equals(obj: Any): Boolean = obj match {
    case v: Box[_] => min == v.min && max == v.max
    case _ => false
  }
  def ==(rhs: Box[T]): Boolean = min == rhs.min && max == rhs.max
  def !=(rhs: Box[T]): Boolean = !(this == rhs)
  override def hashCode(): Int = (min, max).hashCode()

  def +(rhs: Pos[T]): Box[T] = Box(min + rhs, max + rhs)
  def -(rhs: Pos[T]): Box[T] = Box(min - rhs, max - rhs)

  def *(rhs: T): Box[T] = Box(min, max * rhs) // TODO: Both?
  def *(rhs: Pos[T]): Box[T] = Box(min * rhs, max * rhs)

  def overlaps(rhs: Box[T]): Boolean
    = zipped(min.iterator, max.iterator, rhs.min.iterator, rhs.max.iterator)
    .forall{case Seq(aMin, aMax, bMin, bMax) => bMin <= aMax && bMax >= aMin }

  def iterator(implicit int: Integral[T]): Iterator[Pos[T]] = Box.anyIterator(this, reverse = false, Pos.fill(rank, int.one))
  def reverseIterator(implicit int: Integral[T]): Iterator[Pos[T]] = Box.anyIterator(this, reverse = true, Pos.fill(rank, int.one))

  def iteratorBy(step: T)(implicit int: Integral[T]): Iterator[Pos[T]] = Box.anyIterator(this, reverse = false, by = Pos.fill(rank, step))
  def iteratorBy(step: Pos[T])(implicit int: Integral[T]): Iterator[Pos[T]] = Box.anyIterator(this, reverse = false, step)

  def union(rhs: Box[T]): Box[T] = Box(min.min(rhs.min), max.max(rhs.max))
  def moveTo(dim: Int, pos: T): Box[T] = {
    val diff = Pos.unit(rank, dim) * (pos - min(dim))
    Box(min + diff, max + diff)
  }

  def contains(pos: Pos[T]): Boolean = pos isIn this

  /// Returns a slice of this volume over just the given dimension(s).
  def keepDims(dims: Set[Int]): Box[T] = Box(min.keepDims(dims), max.keepDims(dims))
  def keepDims(dim: Int*): Box[T] = keepDims(dim.toSet)

  def rows: Box[T] = keepDims(rank - 2)
  def cols: Box[T] = keepDims(rank - 1)

  /// Returns a "slice" of this volume with the given dimension(s) dropped.
  def dropDims(dims: Set[Int]): Box[T] = Box(min.dropDims(dims), max.dropDims(dims))
  def dropDims(dim: Int*): Box[T] = dropDims(dim.toSet)

  /// Returns a new Volume with an innermost (last) dimension with the range [a,b]
  def expand(a: T, b: T): Box[T] = Box(min.append(a), max.append(b))

  /// Returns a copy with the given dimension changed to the [a, b]
  def alter(dim: Int, a: T, b: T): Box[T] = Box(min.alter(dim, num.min(a, b)), max.alter(dim, num.max(a, b)))

  /// Returns a new Volume with a dimension inserted at the given index
  def insert(dim: Int, a: T, b: T): Box[T] = Box(min.insert(dim, a), max.insert(dim, b))

  def t: Box[T] = Box(min.t, max.t)

  /// Returns an iterator over the edges of this volume.
  /// Edges are inside the volume itself, up to a depth of `width` from each side.
  def edges(width: T): Iterator[Border[T]] = borders(-width, num.zero)

  def borders(): Iterator[Border[T]] = borders(num.one, num.one)
  def borders(width: T, dist: T): Iterator[Border[T]]
    = (0 until rank).iterator.flatMap { dim =>
      val dir = Pos.unit[T](rank, dim)
      val inner = dir * dist
      val outer = dir * (width + dist)
      Iterator(Border(dim, -dir, Box(min - outer, max.alter(dim, min(dim))) - inner),
               Border(dim, dir, Box(min.alter(dim, max(dim)) + inner, max + outer)))
    }

  def above(width: T = implicitly[Numeric[T]].one, delta: T = implicitly[Numeric[T]].one): Box[T]
    = Box(min - Pos.unit[T](rank, rank - 1) * width, max.alter(rank - 1, min(rank - 1) - delta))

  def intersect(rhs: Box[T]): Option[Box[T]]
    = if (!overlaps(rhs)) None else Some(Box(min max rhs.min, max min rhs.max))

  def diff(rhs: Box[T]): Iterator[Box[T]] = intersect(rhs) match {
    case Some(union) =>
      val one = implicitly[Numeric[T]].one
      Box.deltas(rank).flatMap{ delta =>
        val (x: Iterator[(T,T)], y: Iterator[(T,T)]) = delta.iterator.zipWithIndex.map{
          case (-1, d) => (min(d), union.min(d) - one)
          case (0, d)  => (union.min(d), union.max(d))
          case (1, d)  => (union.max(d) + one, max(d))
        }.duplicate
        Box.get(Pos(x.map(_._1)), Pos(y.map(_._2)))
      }
    case None => Iterator(this)
  }

  def diff(rhs: IterableOnce[Box[T]]): Iterator[Box[T]]
    = rhs.iterator.foldLeft(Iterator(this)){(set, v) => set.flatMap(_ diff v) }

  def toDoubles: Box[Double] = Box(min.toDoubles, max.toDoubles)
  def toInts: Box[Int] = Box(min.toInts, max.toInts)
  def toLongs: Box[Long] = Box(min.toLongs, max.toLongs)

  override def toString: String = s"($min to $max)"
}

object Box {
  private def anyIterator[T](volume: Box[T], reverse: Boolean, by: Pos[T])(implicit int: Integral[T]): Iterator[Pos[T]] = {
    zipped(volume.min.iterator, volume.max.iterator, by.iterator).foldLeft(Iterator.empty[Seq[T]]){case (iters, Seq(min, max, by)) =>
      val start: T = if (reverse) max else min
      val end: T = if (reverse) min else max
      val step = if (end >= start) by else int.negate(by)
      val range = NumericRange.inclusive(start, end, step)
      if (!iters.hasNext) range.iterator.map(Seq.apply(_)) else iters.flatMap{idx => range.iterator.map{x => idx :+ x }}
    }.map(_.iterator).map{seq => Pos[T](seq) }
  }

  private def deltas(rank: Int): Iterator[Pos[Int]]
    = Box(Pos.fill(rank, -1), Pos.fill(rank, 1)).iterator.filterNot(_.iterator.forall(_ == 0))

  def unit[T:Numeric](at: Pos[T]): Box[T] = new Box(at, at)
  def apply[T:Numeric](p0: Pos[T], p1: Pos[T]): Box[T] = new Box(p0, p1)

  def get[T](min: Pos[T], max: Pos[T])(implicit num: Numeric[T]): Option[Box[T]] = {
    if (min.iterator.zip(max.iterator).forall{case (a, b) => a <= b }) Some(Box(min, max)) else None
  }

  implicit class IntegralVolume[T](a: Box[T])(implicit int: Integral[T]) {
    def /(b: T): Box[T] = Box(a.min / b, a.max / b)
    def /(b: Pos[T]): Box[T] = Box(a.min / b, a.max / b)
  }
  implicit class FractionalVolume[T](a: Box[T])(implicit f: Fractional[T]) {
    def /(b: T): Box[T] = Box(a.min / b, a.max / b)
    def /(b: Pos[T]): Box[T] = Box(a.min / b, a.max / b)
    def roundInt: Box[Int] = Box(a.min.roundInt, a.max.roundInt)
  }
}