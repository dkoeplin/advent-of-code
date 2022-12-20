package Year2022

import common.Pos
import common.mutable.SparseGrid
object Day14 extends App {
  val file = scala.io.Source.fromFile("data/2022/14")
  val paths = file.getLines().map{line => line.split(" -> ").map{pt =>
    val pair = pt.split(",")
    Pos(pair(1).toInt, pair(0).toInt) // pair is x, y, matrix uses y, x
  }}.toArray
  val grid = SparseGrid[Char]('.')
  paths.foreach{path => path.sliding(2).foreach{pair =>
    // defines an area, but each path is actually a line in practice
    val stepy = if (pair(1).row >= pair(0).row) 1 else -1
    val stepx = if (pair(1).col >= pair(0).col) 1 else -1
    (pair(0).row to pair(1).row by stepy).foreach{i =>
      (pair(0).col to pair(1).col by stepx).foreach{j =>
        grid(i, j) = '#'
      }
    }
  }}

  def flood(floor: Option[Int]): Int = {
    def inBounds(pos: Pos): Boolean = floor match {
      case Some(maxY) => pos.row < maxY
      case None =>
        val min = grid.start()
        val max = grid.end()
        pos.col >= min.col && pos.col <= max.col && pos.row >= min.row && pos.row <= max.row
    }
    def isAir(pos: Pos): Boolean = floor match {
      case Some(maxY) if pos.row == maxY => false
      case _ => grid(pos) == '.'
    }
    var continue: Boolean = true
    val moves = Seq(Pos.DOWN, Pos.DOWN_LEFT, Pos.DOWN_RIGHT)
    var count: Int = 0
    while (continue) {
      var sand: Pos = Pos(0, 500)
      var next: Option[Pos] = Some(sand)
      while (next.exists(inBounds)) {
        next = moves.iterator.map(_ + sand).find(isAir)
        sand = next.getOrElse(sand)
      }
      continue = inBounds(sand)
      if (continue) {
        grid(sand) = 'o'
        count += 1
        continue = continue && (sand != Pos(0, 500))
      }
    }
    count
  }
  println("== Part 1 ==")
  val maxY = grid.end().row
  val part1 = flood(floor=None)
  //println(grid.toString)
  println(s"Part1: $part1")

  println("== Part 2 ==")
  val part2 = flood(floor=Some(maxY + 2))
  //val min = grid.start()
  //val max = grid.end()
  //grid.minX = Some(min.col - 2)
  //grid.maxX = Some(max.col + 2)
  //println(grid.toString)
  //println("#" * (max.col - min.col + 5))
  println(s"Part2: ${part1 + part2}")
}
