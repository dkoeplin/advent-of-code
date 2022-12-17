package Year2022

object Day08 extends App {
  object Dir extends Enumeration {
    type Dir = Value
    val L = Value("L")
    val R = Value("R")
    val U = Value("U")
    val D = Value("D")
  }

  case class Pos(row: Int, col: Int) {
    def +(dir: Dir.Dir): Pos = dir match {
      case Dir.L => Pos(row, col - 1)
      case Dir.R => Pos(row, col + 1)
      case Dir.U => Pos(row - 1, col)
      case Dir.D => Pos(row + 1, col)
    }
  }

  val file = scala.io.Source.fromFile("data/full/08")
  val data = common.mutable.Matrix[Int](file.getLines().map(_.map(_ - '0')))
  def scan(i: Int, dir: Dir.Dir): Set[Pos] = {
    var max: Int = -1
    var set: Set[Pos] = Set.empty
    var pos: Pos = dir match {
      case Dir.L => Pos(i, data.cols - 1)
      case Dir.R => Pos(i, 0)
      case Dir.U => Pos(data.rows - 1, i)
      case Dir.D => Pos(0, i)
    }
    while (pos.row >= 0 && pos.row < data.rows && pos.col >= 0 && pos.col < data.cols) {
      if (data(pos.row, pos.col) > max) {
        max = data(pos.row, pos.col)
        set += pos
      }
      pos += dir
    }
    set
  }
  def score(i: Int, j: Int, dir: Dir.Dir): Int = {
    val find = data(i, j)
    var count: Int = 0
    var pos: Pos = Pos(i, j) + dir
    while (pos.row >= 0 && pos.row < data.rows && pos.col >= 0 && pos.col < data.cols) {
      count += 1
      pos = if (data(pos.row, pos.col) >= find) Pos(-1, -1) else pos + dir
    }
    count
  }

  val h = (0 until data.rows).iterator.flatMap{i => scan(i, Dir.L) ++ scan(i, Dir.R) }.toSet
  val v = (0 until data.cols).iterator.flatMap{i => scan(i, Dir.U) ++ scan(i, Dir.D) }.toSet
  val x = (h ++ v)
  val part1 = x.size
  println(s"Part1: $part1")

  val part2 = data.indices().iterator.map{case (i,j) =>
    score(i, j, Dir.L) * score(i, j, Dir.R) * score(i, j, Dir.U) * score(i, j, Dir.D)
  }.max
  println(s"Part2: $part2")
}
