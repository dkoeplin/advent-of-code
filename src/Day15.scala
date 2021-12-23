import scala.collection.mutable

object Day15 extends App {
  val Rule = "([A-Z])([A-Z]) -> ([A-Z])".r
  val file = scala.io.Source.fromFile("./data/15")
  val grid: Array[Array[Int]] = file.getLines().map(_.map(_ - '0').toArray).toArray
  val tileRows = grid.length
  val tileCols = grid(0).length

  val neighbors = Seq(Point(-1, 0), Point(1, 0), Point(0, -1), Point(0, 1))

  def risk(point: Point): Int = {
    val i = point.i % tileRows
    val j = point.j % tileCols
    val tile_i = point.i / tileRows
    val tile_j = point.j / tileCols
    val base = grid(i)(j) + tile_i + tile_j
    if (base > 9) base % 9 else base
  }

  case class Point(i: Int, j: Int) {
    def +(rhs: Point): Point = Point(i + rhs.i, j + rhs.j)
  }
  case class Pair(point: Point, cost: Int)

  def search(rows: Int, cols: Int): Int = {
    val start = Point(0, 0)
    val end = Point(rows - 1, cols - 1)

    def isValid(point: Point): Boolean = point.i >= 0 && point.i < rows && point.j >= 0 && point.j < cols

    val costs = mutable.Map.empty[Point,Int]
    val visited = mutable.Set.empty[Point]
    def cost(x: Point): Int = costs.getOrElse(x, Int.MaxValue)
    implicit def pointOrdering: Ordering[Pair] = (a: Pair, b: Pair) => implicitly[Ordering[Int]].compare(-a.cost, -b.cost)

    val frontier = mutable.PriorityQueue.empty[Pair]
    costs(start) = 0
    frontier.enqueue(Pair(start, 0))

    var continue = true
    while (continue && frontier.nonEmpty) {
      val current = frontier.dequeue()
      continue = current.point != end

      if (continue && !visited.contains(current.point)) {
        visited += current.point
        neighbors.iterator.map(_ + current.point).filter(isValid).filterNot(visited.contains).foreach{n =>
          val newCost = cost(current.point) + risk(n)
          if (newCost < cost(n)) {
            costs(n) = newCost
            frontier.enqueue(Pair(n, newCost))
          }
        }
      }
    }
    costs(end)
  }

  println(s"Part 1: ${search(tileRows, tileCols)}")
  println(s"Part 2: ${search(tileRows*5, tileCols*5)}")
}
