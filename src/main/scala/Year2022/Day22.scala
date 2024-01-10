package Year2022

import common.immutable.Pos.Idx
import common.mutable.Matrix

object Day22 extends common.AoC(22, 2022) {
  case class Bounds(min: Int, max: Int) {
    def union(i: Int): Bounds = Bounds(min = Math.min(min, i), max = Math.max(max, i))
  }

  case class Instruction(x: Either[Int, Char])
  object Instruction {
    private val dir = "([0-9]+|L|R)".r
    def apply(x: Char): Instruction = Instruction(Right(x))
    def apply(n: Int): Instruction = Instruction(Left(n))
    def parse(x: String): Array[Instruction] = dir.findAllMatchIn(x).map{m => m.group(1) match {
      case "L" => Instruction('L')
      case "R" => Instruction('R')
      case n   => Instruction(n.toInt)
    }}.toArray
  }

  case class PosFacing(x: Idx, facingIdx: Int) {
    val facing: Idx = PosFacing.facings(facingIdx)
    def password: Long = 1000 * (x.h + 1) + 4 * (x.w + 1) + facingIdx
    def char: Char = PosFacing.chars(facingIdx)
    override def toString: String = s"$char(${x.h},${x.w})"
  }
  object PosFacing {
    def apply(x: Idx, facing: Idx): PosFacing = PosFacing(x, facings.indexOf(facing))
    val facings: Seq[Idx] = Idx.D2.nondiag.toSeq
    val chars: Seq[Char] = Seq('>','v','<','^')
  }

  val lines = data.getLines().toArray
  val map = lines.dropRight(2)
  val rows = map.length
  val cols = map.map(_.length).max
  val board = Matrix.fill[Char](Idx(rows, cols), ' ')
  val rowBounds = Array.fill(board.H)(Bounds(min = board.W, max = 0))
  val colBounds = Array.fill(board.W)(Bounds(min = board.H, max = 0))
  map.zipWithIndex.foreach{case (line, i) => line.zipWithIndex.foreach{case (c, j) =>
    board(Idx(i, j)) = c
    if (c != ' ') {
      rowBounds(i) = rowBounds(i) union j
      colBounds(j) = colBounds(j) union i
    }
  }}
  def wrapped(pos: Idx, facing: Idx): Idx = facing match {
    case Idx.D2.L if pos.w < rowBounds(pos.h).min => Idx(pos.h, rowBounds(pos.h).max)
    case Idx.D2.R if pos.w > rowBounds(pos.h).max => Idx(pos.h, rowBounds(pos.h).min)
    case Idx.D2.U if pos.h < colBounds(pos.w).min => Idx(colBounds(pos.w).max, pos.w)
    case Idx.D2.D if pos.h > colBounds(pos.w).max => Idx(colBounds(pos.w).min, pos.w)
    case _ => pos
  }
  def getNext(current: Idx, facing: Idx): Option[Idx] = {
    Some(current + facing).map{n => wrapped(n, facing) }.filter{p => board(p) != '#' }
  }

  val dirs = Instruction.parse(lines.last)
  val start = PosFacing(Idx(0,  rowBounds(0).min), Idx.D2.R)
  val part1 = dirs.foldLeft(start){(current, i) => i.x match {
    case Left(n) =>
      var curr: Idx = current.x
      var next: Option[Idx] = Some(curr)
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
    case Right('L') =>
      val end = PosFacing(current.x, Math.floorMod(current.facingIdx - 1, PosFacing.facings.size))
      board(end.x) = end.char
      println(s"$current + L: $end")
      end
    case Right('R') =>
      val end = PosFacing(current.x, Math.floorMod(current.facingIdx + 1, PosFacing.facings.size))
      board(end.x) = end.char
      println(s"$current + R: $end")
      end
  }}
  println(board.toString)
  println(s"Part1: $part1: ${part1.password}")
}
