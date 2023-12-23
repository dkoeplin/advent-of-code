package common

// 2D surface from (x0,y0) to (x1,y1) inclusive
case class Surface(min: Pos, max: Pos) {
  lazy val xRange: Range = Range.inclusive(min.col, max.col)
  lazy val yRange: Range = Range.inclusive(min.row, max.row)
  def overlaps(rhs: Surface): Boolean = (xRange overlaps rhs.xRange) && (yRange overlaps rhs.yRange)
  def union(rhs: Surface): Surface = Surface(min.min(rhs.min), max.max(rhs.max))
  override def toString: String = s"($min to $max)"
}
