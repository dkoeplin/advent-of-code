package common.immutable

import common.implicits.BooleanOps.BooleanOps
import common.implicits.IteratorOps.zipped

class Pos[T](private val inds: List[T])(implicit num: Numeric[T]) {
  import num._

  def rank: Int = inds.size
  def iterator: Iterator[T] = inds.iterator
  def reverseIterator: Iterator[T] = inds.reverseIterator
  def get(dim: Int): Option[T] = if (dim >= 0 && dim < rank) Some(apply(dim)) else None
  def getOrElse(dim: Int, els: => T): T = get(dim).getOrElse(els)
  def apply(dim: Int): T = inds(dim)

  def reverse: Pos[T] = Pos(inds.reverse)

  def product: T = inds.product
  def sum: T = inds.sum

  // Special accessors
  def x: T = getOrElse(0, num.fromInt(1))
  def y: T = getOrElse(1, num.fromInt(1))
  def z: T = getOrElse(2, num.fromInt(1))

  def w: T = getOrElse(rank - 1, num.fromInt(1))
  def h: T = getOrElse(rank - 2, num.fromInt(1))

  def +(rhs: T): Pos[T] = Pos(iterator.map{x => x + rhs})
  def -(rhs: T): Pos[T] = Pos(iterator.map{x => x - rhs})
  def *(rhs: T): Pos[T] = Pos(iterator.map{x => x * rhs})
  def /(rhs: T)(implicit f: Integral[T]): Pos[T] = Pos(iterator.map{x => f.quot(x, rhs) })

  def +(rhs: Pos[T]): Pos[T] = Pos(iterator.zip(rhs.iterator).map{case (a,b) => a + b })
  def -(rhs: Pos[T]): Pos[T] = Pos(iterator.zip(rhs.iterator).map{case (a,b) => a - b })
  def *(rhs: Pos[T]): Pos[T] = Pos(iterator.zip(rhs.iterator).map{case (a,b) => a * b })
  def unary_- : Pos[T] = Pos(iterator.map(num.negate))

  def min(rhs: Pos[T]): Pos[T] = Pos(iterator.zip(rhs.iterator).map{case (a,b) => num.min(a, b) })
  def max(rhs: Pos[T]): Pos[T] = Pos(iterator.zip(rhs.iterator).map{case (a,b) => num.max(a, b) })

  override def equals(obj: Any): Boolean = obj match {
    case rhs: Pos[_] => inds == rhs.inds
    case _ => false
  }
  def ==(rhs: Pos[T]): Boolean = inds == rhs.inds
  def !=(rhs: Pos[T]): Boolean = inds != rhs.inds
  override def hashCode(): Int = inds.hashCode()

  def isIn(volume: Volume[T]): Boolean
    = zipped(iterator, volume.min.iterator, volume.max.iterator).forall{case Seq(i, min, max) => i >= min && i <= max }

  def manhattanDist(rhs: Pos[T]): T = iterator.zip(rhs.iterator).map{case (a,b) => num.abs(a - b) }.sum
  def dist(rhs: Pos[T]): Double = {
    val total = iterator.zip(rhs.iterator).map{case (a,b) => a - b }.map{x => x*x }.sum
    Math.sqrt(num.toDouble(total))
  }

  def magnitude: Double = dist(Pos.zero[T](rank))

  def to(rhs: Pos[T]): Volume[T] = new Volume[T](this, rhs)

  /// Returns a new Pos with only the given dimension(s).
  def keepDims(dims: Set[Int]): Pos[T] = Pos(iterator.zipWithIndex.filter{case (_,i) => dims.contains(i) }.map(_._1))
  def keepDims(dim: Int*): Pos[T] = keepDims(dim.toSet)

  /// Returns a new Pos with the given dimension(s) dropped
  def dropDims(dims: Set[Int]): Pos[T] = Pos(iterator.zipWithIndex.filterNot{case (_,i) => dims.contains(i) }.map(_._1))
  def dropDims(dim: Int*): Pos[T] = dropDims(dim.toSet)

  // Drops the first N dimensions.
  // def drop(n: Int): Pos[T] = Pos(inds.drop(n))

  /// Returns a new Pos with an appended innermost (last) index
  def append(i: T): Pos[T] = Pos(iterator ++ Iterator(i))

  def alter(dim: Int, i: T): Pos[T] = {
    val (before, after) = iterator.splitAt(dim)
    Pos(before ++ Iterator(i) ++ after.drop(1))
  }

  /// Returns a new Pos with a dimension at the given index
  def insert(dim: Int, i: T): Pos[T] = {
    val (before, after) = iterator.splitAt(dim)
    Pos(before ++ Iterator(i) ++ after)
  }

  def t: Pos[T] = Pos(iterator.take(rank - 2) ++ Iterator(w, h))

  override def toString: String = inds.mkString("(", ", ", ")")

  lazy val strides: Pos[T] = Pos(reverseIterator.drop(1).scanLeft(num.fromInt(1)){(prev,n) => prev * n}.toSeq.reverse)
}

object Pos {
  def fill[T:Numeric](rank: Int, value: T): Pos[T] = new Pos[T](List.fill(rank)(value))
  def zero[T:Numeric](rank: Int): Pos[T] = fill(rank, implicitly[Numeric[T]].zero)

  def unit[T:Numeric](rank: Int, dim: Int): Pos[T]
    = new Pos[T](List.tabulate(rank){i => implicitly[Numeric[T]].fromInt((i == dim).toInt)})

  def apply[T:Numeric](indices: IterableOnce[T]): Pos[T] = new Pos[T](List.from(indices))
  def apply[T:Numeric](x: T, y: T): Pos[T] = new Pos[T](List(x, y))
  def apply[T:Numeric](x: T, y: T, z: T): Pos[T] = new Pos[T](List(x, y, z))

  def parse[T:Numeric](line: String): Pos[T] = parse(line, ",")
  def parse[T](line: String, delim: String)(implicit num: Numeric[T]): Pos[T] = {
    apply(line.split(delim).iterator.map(_.trim).flatMap(num.parseString))
  }

  /**
   * Returns an iterator over all Pos of the given rank where exactly one index is non-zero and either -1 or 1.
   */
  def nondiag[T](rank: Int)(implicit int: Integral[T]): Iterator[Pos[T]] = {
    Iterator.tabulate(rank) { i => apply(Iterator.tabulate(rank) { j => if (j == i) int.fromInt(-1) else int.zero }) } ++
      Iterator.tabulate(rank) { i => apply(Iterator.tabulate(rank) { j => if (j == i) int.fromInt(1) else int.zero }) }
  }

  /**
   * Returns an iterator over all Pos in (-1,-1,...,-1) to (1,1,...,1) except (0,0,...,0).
   */
  def adjacent[T](rank: Int)(implicit int: Integral[T]): Iterator[Pos[T]] = {
    val zero = int.zero
    val min: Pos[T] = apply(Seq.fill(rank)(int.fromInt(-1)))
    val max: Pos[T] = apply(Seq.fill(rank)(int.fromInt(1)))
    (min to max).iterator.filterNot(_.iterator.forall { x => int.equiv(x, zero) })
  }

  def unapplySeq[T](x: Pos[T]): UnapplySeqWrapper[T] = new UnapplySeqWrapper[T](x)
  final class UnapplySeqWrapper[T](private val a: Pos[T]) extends AnyVal {
    def isEmpty: false = false
    def get: UnapplySeqWrapper[T] = this
    def lengthCompare(len: Int): Int = a.inds.lengthCompare(len)
    def apply(i: Int): T = a(i)
    def drop(n: Int): scala.Seq[T] = a.inds.drop(n)
    def toSeq: scala.Seq[T] = a.inds
  }

  type Idx = Pos[Int]

  object Idx {
    def zero(rank: Int): Idx = Pos.zero[Int](rank)
    def apply(inds: Int*): Idx = Pos[Int](inds)
    def unapplySeq(x: Idx): Option[Seq[Int]] = Some(x.inds)
    def parse(str: String): Idx = Pos.parse[Int](str)

    case class ND(rank: Int) {
      def nondiag: Iterator[Idx] = Pos.nondiag[Int](rank)
      def adjacent: Iterator[Idx] = Pos.adjacent[Int](rank)

      val U: Idx = Pos.unit[Int](rank, 0) * -1
      val D: Idx = Pos.unit[Int](rank, 0)
      val L: Idx = Pos.unit[Int](rank, 1) * -1
      val R: Idx = Pos.unit[Int](rank, 1)
    }
    val D2: ND = ND(2)

    // def unapply(str: String): Option[Idx] = Some(parse[Long](str)).filter(_.rank > 0)
  }

  // Pos isn't quite numeric - it doesn't have a strict ordering
  // implicit class NumericIdx(x: Idx) extends Numeric[Idx]
}

