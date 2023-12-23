package Year2022

import common.Pos
import common.mutable.Matrix

object Day12 extends App {
  val file = scala.io.Source.fromFile("data/2022/12")
  val matrix: Matrix[Char] = Matrix(file.getLines().map(_.toArray))
  val start = matrix.posIterator().find{pos => matrix(pos) == 'S' }.get
  val end = matrix.posIterator().find{pos => matrix(pos) == 'E' }.get
  matrix(start) = 'a'
  matrix(end) = 'z'

  def neighbors(pos: Pos, matrix: Matrix[Char]): Seq[Pos] = {
    val src = matrix(pos).toInt
    val left = Pos(pos.row, pos.col - 1)
    val right = Pos(pos.row, pos.col + 1)
    val up = Pos(pos.row - 1, pos.col)
    val down = Pos(pos.row + 1, pos.col)
    Seq(left, right, up, down).filter{pos => matrix.get(pos).exists{dest => dest.toInt - src <= 1 }}
  }
  def dijkstra(matrix: Matrix[Char], start: Pos, end: Pos): Int = {
    case class Vertex(pos: Pos, dist: Int)
    implicit def ordering: Ordering[Vertex] = (a: Vertex, b: Vertex) => implicitly[Ordering[Int]].compare(-a.dist, -b.dist)
    val queue = scala.collection.mutable.PriorityQueue.empty[Vertex]
    val dist = scala.collection.mutable.Map.empty[Pos, Int]
    def distance(x: Pos) = dist.getOrElse(x, Int.MaxValue)
    val visited = scala.collection.mutable.Set.empty[Pos]
    var continue = true
    queue.enqueue(Vertex(start, 0))
    dist(start) = 0
    while (queue.nonEmpty) {
      val v = queue.dequeue()
      continue = v.pos != end
      if (continue && !visited.contains(v.pos)) {
        visited += v.pos
        neighbors(v.pos, matrix).foreach{next =>
          val total = distance(v.pos) + 1
          if (total < distance(next)) {
            dist(next) = total
            queue.enqueue(Vertex(next, total))
          }
        }
      }
    }
    distance(end)
  }

  val part1 = dijkstra(matrix, start, end)
  println(s"Part1: $part1")

  val part2 = matrix.posIterator().flatMap{pos =>
    if (matrix(pos) == 'a') Some(dijkstra(matrix, pos, end)) else None
  }.min
  println(s"Part2: $part2")
}
