package common.algorithm

case class PathSummary[T](start: T, dist: Map[T, Int], prev: Map[T, T]) {
  def pathTo(v: T): Iterator[T] = Iterator.iterate(v){v => prev(v) }.takeWhile{v => v != start }
  def distanceTo(v: T): Int = dist(v)
}

abstract class Dijkstra[T](start: T) {
  case class Vertex(pos: T, dist: Int) extends Ordered[Vertex] {
    override def compare(rhs: Vertex): Int = implicitly[Ordering[Int]].compare(-dist, -rhs.dist)
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

    var continue = true
    queue.enqueue(Vertex(start, 0))
    dist(start) = 0
    while (queue.nonEmpty) {
      val v = queue.dequeue()
      continue = !isEnd(v.pos)
      if (continue && !visited.contains(v.pos)) {
        visited += v.pos
        next(v.pos).foreach{next =>
          val total = distance(v.pos) + weight(v.pos)
          if (total < distance(next)) {
            dist(next) = total
            prev(next) = v.pos
            queue.enqueue(Vertex(next, total))
          }
        }
      }
    }
    PathSummary(start, dist.toMap, prev.toMap)
  }
}
