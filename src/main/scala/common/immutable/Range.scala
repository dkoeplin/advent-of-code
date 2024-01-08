package common.immutable

import scala.collection.immutable.NumericRange

/**
 * An inclusive range from [min, max]
 */
class Range(val min: Long, val max: Long) {
  lazy val length: Long = max - min + 1
  def iterator: Iterator[Long] = NumericRange.inclusive(min, max, 1).iterator

  def contains(i: Long): Boolean = i >= min && i <= max

  def overlaps(rhs: Range): Boolean = rhs.min <= max && rhs.max >= min

  /**
   * Returns a tuple of ranges, the first with values [start,cutoff), the second with [cutoff,end]
   * Either range may be empty if cutoff is not contained within this range.
   */
  def <(cutoff: Long): (Range, Range) = {
    if (contains(cutoff)) (Range.inclusive(min, cutoff - 1), Range.inclusive(cutoff,max))
    else if (max < cutoff) (this, Range.empty)
    else (Range.empty, this)
  }
  def >(cutoff: Int): (Range, Range) = {
    if (contains(cutoff)) (Range.inclusive(cutoff + 1, max), Range.inclusive(min, cutoff))
    else if (min > cutoff) (this, Range.empty)
    else (Range.empty, this)
  }

  def -(rhs: Range): List[Range] = diff(rhs)
  def diff(rhs: Range): List[Range] = if (!overlaps(rhs)) List(this) else {
    val l = Range.inclusive(min, rhs.min - 1)
    val r = Range.inclusive(rhs.max + 1, max)
    List(l, r).filter(_.nonEmpty)
  }

  def intersect(rhs: Range): Range = {
    Range.inclusive(Math.max(min, rhs.min), Math.min(max, rhs.max))
  }

  def nonEmpty: Boolean = length > 0

  override def toString: String = if (length > 0) s"[$min, $max]" else "Empty"
}

object Range {
  def empty: Range = Range.inclusive(0, -1)
  def unit(i: Long): Range = Range.inclusive(i, i)
  def inclusive(min: Long, max: Long): Range = new Range(min, max)
  def at(min: Long, length: Long): Range = new Range(min, min + length -1)
}
