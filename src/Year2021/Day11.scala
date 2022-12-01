package Year2021

import common.mutable.Matrix

object Day11 extends App {
  def neighbors() = (-1 to 1).iterator.flatMap { i => (-1 to 1).map { j => (i, j) } }.filterNot { case (i, j) => i == 0 && j == 0 }

  val file = scala.io.Source.fromFile("./data/11")
  val grid = Matrix(file.getLines().map(_.map(_ - '0')))
  var flashes: Int = 0

  def increment(i: Int, j: Int): Boolean = {
    grid(i, j) = grid.get(i, j).map(_ + 1)
    grid.get(i, j).contains(10)
  }

  def flash(i: Int, j: Int): Iterable[(Int, Int)] = {
    flashes += 1
    neighbors().map { case (di, dj) => (i + di, j + dj) }.filter { case (i, j) => increment(i, j) }.toSeq
  }

  def step(): Unit = {
    var flashers: Seq[(Int, Int)] = grid.indices().filter { case (i, j) => increment(i, j) }
    while (flashers.nonEmpty) {
      flashers = flashers.flatMap { case (i, j) => flash(i, j) }
    }
    grid.indices().foreach { case (i, j) => grid(i, j) = grid.get(i, j).filter(_ >= 10).map(_ => 0) }
  }

  (1 to 100).foreach { _ => step() }
  println(s"Part 1: Flashes: $flashes")

  val part2 = (101 to 1000).find { _ => step(); grid.indices().forall { case (i, j) => grid(i, j) == 0 } }
  println(s"Part 2: Step: ${part2.map(_.toString).getOrElse("> 1000")}")
}
