package common.algorithm

abstract class Dijkstra[T](start: T) extends GraphAlgorithm[T] {
  def solve(): Paths[T] = {
    val queue = scala.collection.mutable.HashSet.empty[T]
    val visited = scala.collection.mutable.Set.empty[T]
    val dist = scala.collection.mutable.Map.empty[T, Int]
    val prev = scala.collection.mutable.Map.empty[T, T]

    def distance(x: T): Int = dist.getOrElse(x, Int.MaxValue)

    queue += start
    dist(start) = 0
    while (queue.nonEmpty) {
      val u = queue.minBy(distance)
      queue -= u
      visited += u
      val total = distance(u) + weight(u)
      next(u).filterNot(visited.contains).foreach{v =>
        if (total < distance(v)) {
          dist(v) = total
          prev(v) = u
          queue += v
        }
      }
      //continue = !isEnd(v.pos)
    }
    Paths(start, dist.toMap, prev.toMap)
  }

}
