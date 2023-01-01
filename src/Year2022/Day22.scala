package Year2022

import Year2022.Day22.PosFacing.facings
import common.Pos
import common.Directions
import common.mutable.Matrix

object Day22 extends App {
  case class Bounds(min: Int, max: Int) {
    def union(i: Int): Bounds = Bounds(min = Math.min(min, i), max = Math.max(max, i))
  }

  case class Instruction(x: Either[Int, Directions.Dir])
  object Instruction {
    private val dir = "([0-9]+|L|R)".r
    def apply(x: Directions.Dir): Instruction = Instruction(Right(x))
    def apply(n: Int): Instruction = Instruction(Left(n))
    def parse(x: String): Array[Instruction] = dir.findAllMatchIn(x).map{m => m.group(1) match {
      case "L" => Instruction(Directions.L)
      case "R" => Instruction(Directions.R)
      case n   => Instruction(n.toInt)
    }}.toArray
  }

  case class PosFacing(x: Pos, facingIdx: Int) {
    val facing: Pos = facings(facingIdx)
    def password: Int = 1000 * (x.row + 1) + 4 * (x.col + 1) + facingIdx
    def char: Char = PosFacing.chars(facingIdx)
    override def toString: String = s"$char(${x.row},${x.col})"
  }
  object PosFacing {
    def apply(x: Pos, facing: Pos): PosFacing = PosFacing(x, facings.indexOf(facing))
    val facings: Seq[Pos] = Seq(Pos.RIGHT, Pos.DOWN, Pos.LEFT, Pos.UP)
    val chars: Seq[Char] = Seq('>','v','<','^')
  }

  val file = scala.io.Source.fromFile("data/2022/22")
  val lines = file.getLines().toArray
  val map = lines.dropRight(2)
  val rows = map.length
  val cols = map.map(_.length).max
  val board = Matrix.empty[Char](rows, cols, ' ')
  val rowBounds = Array.fill[Bounds](board.rows)(Bounds(min = board.cols, max = 0))
  val colBounds = Array.fill[Bounds](board.cols)(Bounds(min = board.rows, max = 0))
  map.zipWithIndex.foreach{case (line, i) => line.zipWithIndex.foreach{case (c, j) =>
    board(i, j) = c
    if (c != ' ') {
      rowBounds(i) = rowBounds(i) union j
      colBounds(j) = colBounds(j) union i
    }
  }}
  def wrapped(pos: Pos, facing: Pos): Pos = facing match {
    case Pos.LEFT if pos.col < rowBounds(pos.row).min => Pos(pos.row, rowBounds(pos.row).max)
    case Pos.RIGHT if pos.col > rowBounds(pos.row).max => Pos(pos.row, rowBounds(pos.row).min)
    case Pos.UP if pos.row < colBounds(pos.col).min => Pos(colBounds(pos.col).max, pos.col)
    case Pos.DOWN if pos.row > colBounds(pos.col).max => Pos(colBounds(pos.col).min, pos.col)
    case _ => pos
  }
  def getNext(current: Pos, facing: Pos): Option[Pos] = {
    Some(current + facing).map{n => wrapped(n, facing) }.filter{p => board(p) != '#' }
  }

  val dirs = Instruction.parse(lines.last)
  val start = PosFacing(Pos(row = 0, col = rowBounds(0).min), Pos.RIGHT)
  val part1 = dirs.foldLeft(start){(current, i) => i.x match {
    case Left(n) =>
      var curr: Pos = current.x
      var next: Option[Pos] = Some(curr)
      var ii = 0
      while (ii < n && next.nonEmpty) {
        curr = next.get
        next = getNext(curr, current.facing)
        // println(s"  Moved to $curr (${board(curr)}), next: $next")
        board(curr) = current.char
        ii += 1
      }
      val end = PosFacing(next.getOrElse(curr), current.facing)
      board(end.x) = end.char
      println(s"$current + ${current.char}$n: $end")
      end
    case Right(Directions.L) =>
      val end = PosFacing(current.x, Math.floorMod(current.facingIdx - 1, facings.size))
      board(end.x) = end.char
      println(s"$current + L: $end")
      end
    case Right(Directions.R) =>
      val end = PosFacing(current.x, Math.floorMod(current.facingIdx + 1, facings.size))
      board(end.x) = end.char
      println(s"$current + R: $end")
      end
  }}
  println(board.toString)
  println(s"Part1: $part1: ${part1.password}")
}
