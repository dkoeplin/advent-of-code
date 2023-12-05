package common

class Range(val start: Long, val length: Long) {
  def end: Long = start + length - 1

  def contains(i: Long): Boolean = i >= start && i <= end

  def overlaps(rhs: Range): Boolean = rhs.start <= end && rhs.end >= start

  def intersect(rhs: Range): Range = {
    val beg = math.max(start, rhs.start)
    val end = math.min(this.end, rhs.end)
    val len = end - beg + 1
    Range(beg, len)
  }

  def -(rhs: Range): Seq[Range] = if (!overlaps(rhs)) Seq(this) else {
    val i = intersect(rhs)
    val left = Range(start, i.start - start)
    val right = Range(i.end, end - i.end)
    Seq(left, right).filter(_.nonEmpty)
  }

  def nonEmpty: Boolean = length > 0

  override def toString: String = if (length > 0) s"$start until $end" else "Empty"
}

object Range {
  def empty: Range = Range(0, 0)
  def apply(start: Long, length: Long): Range = if (length >= 0) new Range(start, length) else empty
  def unapply(x: Any): Option[(Long, Long)] = x match {
    case range: Range => Some((range.start, range.length))
    case _ => None
  }
}
