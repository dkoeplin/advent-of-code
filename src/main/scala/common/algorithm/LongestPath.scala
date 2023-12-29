package common.algorithm

abstract class LongestPath[T](start: T, end: T) extends GraphAlgorithm[T] {
  override def isEnd(v: T): Boolean = { v == end }
  /// Determines the longest path between start and end, keeping intermediate path sets
  /// Returns the set of all nodes on this path between start and end, inclusive.
  def solve(): Set[T] = {
    val queue = scala.collection.mutable.Queue.empty[T]
    val dist = scala.collection.mutable.Map.empty[T, Int]
    val path = scala.collection.mutable.Map.empty[T, Set[T]]
    def distance(v: T): Int = dist.getOrElse(v, 0)

    queue += start
    dist(start) = 0
    path(start) = Set.empty
    while (queue.nonEmpty) {
      val u = queue.dequeue()
      val t = dist(u) + weight(u)
      val p = path(u) + u
      next(u).foreach{v =>
        //println(s"$u => $v (dist ${distance(v)} < $t)")
        if (distance(v) < t && !p.contains(v)) {
          dist(v) = t
          path(v) = p
          queue += v
        }
      }
    }
    path(end) + end
  }
}
