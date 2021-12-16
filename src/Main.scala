import scala.collection.mutable

object Main extends App {
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

  def search(rows: Int, cols: Int): Map[Point,Int] = {
    val start = Point(0, 0)
    val end = Point(rows - 1, cols - 1)

    def isValid(point: Point): Boolean = point.i >= 0 && point.i < rows && point.j >= 0 && point.j < cols

    val unvisited = mutable.Set.empty[Point]
    unvisited ++= (0 until rows).flatMap{i => (0 until cols).map{j => Point(i,j) }}
    val costs = mutable.Map.empty[Point, Int]
    def cost(x: Point): Int = costs.getOrElse(x, Int.MaxValue)
    def next(): Point = {
      val next = unvisited.minBy(cost)
      unvisited.remove(next)
      next
    }
    var continue = true
    costs(start) = 0
    while (continue && unvisited.nonEmpty) {
      val current = next()
      continue = current != end
      neighbors.iterator.map(_ + current).filter(isValid).filter(unvisited.contains).foreach{n =>
        costs(n) = Math.min(cost(n), costs(current) + risk(n))
      }
    }
    costs.toMap
  }

  def show(rows: Int, cols: Int): Unit = {
    (0 until rows).foreach{i =>
      println((0 until cols).map{j => risk(Point(i,j)) }.mkString(""))
    }
  }

  def show(rows: Int, cols: Int, costs: Map[Point,Int]): Unit = {
    val head = (0 until cols).map{i => val l = i.toString; s"${" "*(3 - l.length)}$l" }.mkString("|")
    println(s"   ||$head")
    println("-" * (head.length+6))
    (0 until rows).foreach { i =>
      val line = (0 until cols).iterator.map { j => costs.getOrElse(Point(i, j), 999).toString }.map { x => s"${" " * Math.max(3 - x.length, 0)}$x" }.mkString("|")
      val n = i.toString
      println(s"${" "*(3 - n.length)}$n||$line")
      println("-" * (line.length+6))
    }
  }

  val part1Costs = search(tileRows, tileCols)
  val part1 = part1Costs(Point(tileRows - 1, tileCols - 1))
  println(s"Part 1: $part1")

  val part2Costs = search(tileRows*5, tileCols*5)
  val part2 = part2Costs(Point(tileRows*5 - 1, tileCols*5 - 1))
  println(s"Part 2: $part2")
}
