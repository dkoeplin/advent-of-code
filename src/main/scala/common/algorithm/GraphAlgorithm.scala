package common.algorithm

case class Paths[T](start: T, dist: Map[T, Int], prev: Map[T, T]) {
  def pathTo(v: T): List[T] = {
    var path: List[T] = List.empty[T]
    var visited: Set[T] = Set.empty[T]
    var current = v
    while (!visited.contains(current)) {
      path = current +: path
      visited = visited + current
      current = prev.getOrElse(current, current)
    }
    path
  }
  def distanceTo(v: T): Int = dist(v)
}

abstract class GraphAlgorithm[T] {
  def isEnd(v: T): Boolean
  def next(v: T): Iterator[T]
  def weight(v: T): Int = 1
}
