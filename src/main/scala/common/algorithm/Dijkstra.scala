package common.algorithm

case class PathSummary[T](start: T, dist: Map[T, Int], prev: Map[T, T]) {
  def pathTo(v: T): List[T] = {
    var path: List[T] = List.empty[T]
    var visited: Set[T] = Set.empty[T]
    var current = v
    while (current != start && !visited.contains(current)) {
      path = current +: path
      visited = visited + current
      current = prev(current)
    }
    path
  }
  def distanceTo(v: T): Int = dist(v)
}

abstract class Dijkstra[T](start: T) {
  case class Vertex(pos: T, dist: Int) extends Ordered[Vertex] {
    override def compare(rhs: Vertex): Int = implicitly[Ordering[Int]].compare(rhs.dist, dist)
  }

  def isEnd(v: T): Boolean
  def next(v: T): Iterator[T]
  def weight(v: T): Int

  def solve(): PathSummary[T] = {
    val queue = scala.collection.mutable.PriorityQueue.empty[Vertex]
    val visited = scala.collection.mutable.Set.empty[T]
    val dist = scala.collection.mutable.Map.empty[T, Int]
    val prev = scala.collection.mutable.Map.empty[T, T]

    def distance(x: T): Int = dist.getOrElse(x, Int.MaxValue)

    queue.enqueue(Vertex(start, 0))
    dist(start) = 0
    while (queue.nonEmpty) {
      val v = queue.dequeue()
      if (!visited.contains(v.pos)) {
        visited += v.pos
        next(v.pos).foreach{next =>
          val total = distance(v.pos) + weight(v.pos)
          println(s"$v => $next [$total, prev: ${dist.get(next).map(_.toString).getOrElse("N/A")}]")
          if (total < distance(next)) {
            dist(next) = total
            prev(next) = v.pos
            queue.enqueue(Vertex(next, total))
          }
        }
      }
      //continue = !isEnd(v.pos)
    }
    PathSummary(start, dist.toMap, prev.toMap)
  }
}
