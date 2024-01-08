package common.immutable

import common.implicits.IteratorOps.zipped

import scala.collection.immutable.NumericRange
import scala.math.Ordered.orderingToOrdered

/**
 * Defines an N-dimensional surface from min to max, inclusive.
 */
class Volume[T](val l: Pos[T], val r: Pos[T])(implicit num: Numeric[T]) {
  import num._
  val min: Pos[T] = l min r
  val max: Pos[T] = l max r
  lazy val shape: Pos[T] = Pos(min.iterator.zip(max.iterator).map{case (a, b) => b - a + num.one })

  def size: T = shape.product
  def rank: Int = min.rank
  def H: T = shape.getOrElse(rank - 2, num.one)
  def W: T = shape.getOrElse(rank - 1, num.one)

  override def equals(obj: Any): Boolean = obj match {
    case v: Volume[_] => min == v.min && max == v.max
    case _ => false
  }
  def ==(rhs: Volume[T]): Boolean = min == rhs.min && max == rhs.max
  def !=(rhs: Volume[T]): Boolean = !(this == rhs)
  override def hashCode(): Int = (min, max).hashCode()

  def *(rhs: T): Volume[T] = Volume(min, max * rhs)

  def overlaps(rhs: Volume[T]): Boolean
    = zipped(min.iterator, max.iterator, rhs.min.iterator, rhs.max.iterator)
    .forall{case Seq(amin, amax, bmin, bmax) => bmin <= amax && bmax >= amin }

  def iterator(implicit int: Integral[T]): Iterator[Pos[T]] = Volume.anyIterator(this, reverse = false)
  def reverseIterator(implicit int: Integral[T]): Iterator[Pos[T]] = Volume.anyIterator(this, reverse = true)

  def union(rhs: Volume[T]): Volume[T] = Volume(min.min(rhs.min), max.max(rhs.max))
  def moveTo(dim: Int, pos: T): Volume[T] = {
    val diff = Pos.unit(rank, dim) * (pos - min(dim))
    Volume(min + diff, max + diff)
  }

  def contains(pos: Pos[T]): Boolean = pos isIn this

  /// Returns a slice of this volume over just the given dimension(s).
  def keepDims(dims: Set[Int]): Volume[T] = Volume(min.keepDims(dims), max.keepDims(dims))
  def keepDims(dim: Int*): Volume[T] = keepDims(dim.toSet)

  def rows: Volume[T] = keepDims(rank - 2)
  def cols: Volume[T] = keepDims(rank - 1)

  /// Returns a "slice" of this volume with the given dimension(s) dropped.
  def dropDims(dims: Set[Int]): Volume[T] = Volume(min.dropDims(dims), max.dropDims(dims))
  def dropDims(dim: Int*): Volume[T] = dropDims(dim.toSet)


  /// Returns a new Volume with an innermost (last) dimension with the range [a,b]
  def expand(a: T, b: T): Volume[T] = Volume(min.append(a), max.append(b))

  /// Returns a copy with the given dimension changed to the [a, b]
  def alter(dim: Int, a: T, b: T): Volume[T] = Volume(min.alter(dim, a), max.alter(dim, b))

  /// Returns a new Volume with a dimension inserted at the given index
  def insert(dim: Int, a: T, b: T): Volume[T] = Volume(min.insert(dim, a), max.insert(dim, b))

  def t: Volume[T] = Volume(min.t, max.t)

  def edges: Iterator[Volume[T]] = (0 until rank).flatMap{dim =>
    Seq(alter(dim, min(dim), min(dim)), alter(dim, max(dim), max(dim)))
  }.toSet.iterator

  def borders(implicit int: Integral[T]): Iterator[Volume[T]] = (0 until rank).iterator.flatMap{dim =>
    val up = min(dim) - int.one
    val dw = max(dim) + int.one
    val m0 = Pos(min.iterator.zipWithIndex.map{case (i, d) => if (d == dim) up else if (d > dim) i - int.one else i })
    val m1 = Pos(max.iterator.zipWithIndex.map{case (i, d) => if (d == dim) up else if (d > dim) i + int.one else i })
    val m2 = Pos(min.iterator.zipWithIndex.map{case (i, d) => if (d == dim) dw else if (d > dim) i - int.one else i })
    val m3 = Pos(max.iterator.zipWithIndex.map{case (i, d) => if (d == dim) dw else if (d > dim) i + int.one else i })
    Seq(Volume(m0, m1), Volume(m2, m3))
  }

  def intersect(rhs: Volume[T]): Option[Volume[T]]
    = if (!overlaps(rhs)) None else Some(Volume(min max rhs.min, max min rhs.max))

  def diff(rhs: Volume[T]): Set[Volume[T]] = intersect(rhs) match {
    case Some(union) =>
      val one = implicitly[Numeric[T]].one
      Volume.deltas(rank).flatMap{delta =>
        val (x: Iterator[(T,T)], y: Iterator[(T,T)]) = delta.iterator.zipWithIndex.map{
          case (-1, d) => (min(d), union.min(d) - one)
          case (0, d)  => (union.min(d), union.max(d))
          case (1, d)  => (union.max(d) + one, max(d))
        }.duplicate
        Volume.get(Pos(x.map(_._1)), Pos(y.map(_._2)))
      }.toSet
    case None => Set(this)
  }

  def diff(rhs: Iterable[Volume[T]]): Set[Volume[T]] = rhs.foldLeft(Set(this)){(set,v) => set.flatMap(_ diff v) }

  override def toString: String = s"($min to $max)"
}

object Volume {
  private def anyIterator[T](volume: Volume[T], reverse: Boolean)(implicit int: Integral[T]): Iterator[Pos[T]] = {
    volume.min.iterator.zip(volume.max.iterator).foldLeft(Iterator.empty[Seq[T]]){case (iters, (min, max)) =>
      val start = if (reverse) max else min
      val end = if (reverse) min else max
      val step = if (end >= start) 1 else -1
      val range = NumericRange.inclusive(start, end, int.fromInt(step))
      if (!iters.hasNext) range.iterator.map(Seq.apply(_)) else iters.flatMap{idx => range.iterator.map{x => idx :+ x }}
    }.map(_.iterator).map{seq => Pos[T](seq) }
  }

  def unit[T:Numeric](at: Pos[T]): Volume[T] = new Volume(at, at)
  def apply[T:Numeric](p0: Pos[T], p1: Pos[T]): Volume[T] = new Volume(p0, p1)

  def deltas(rank: Int): Iterator[Pos[Int]]
    = Volume(Pos.fill(rank, -1), Pos.fill(rank, 1)).iterator.filterNot(_.iterator.forall(_ == 0))

  def get[T](min: Pos[T], max: Pos[T])(implicit num: Numeric[T]): Option[Volume[T]] = {
    if (min.iterator.zip(max.iterator).forall{case (a, b) => a <= b }) Some(Volume(min, max)) else None
  }
}