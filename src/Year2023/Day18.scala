package Year2023

import common.Pos
import common.Range
import common.immutable.Matrix
import common.implicits.OptionOps._

object Day18 extends Year2023(18) {
  abstract class Border {
    def diff(r: Rectangle): List[Border]
    def diff(r: Set[Rectangle]): List[Border] = r.foldLeft(List(this)){(borders, r) => borders.flatMap(_ diff r) }
    def toRectangle: Rectangle
  }
  case class BorderH(row: Int, cols: Range) extends Border {
    override def diff(r: Rectangle): List[Border]
      = if (!r.containsRow(row)) List(this) else (cols diff r.colRange).map{range => BorderH(row, range) }

    override def toRectangle: Rectangle = Rectangle(Pos(row, cols.start.toInt), Pos(row, cols.end.toInt))
  }
  case class BorderV(rows: Range, col: Int) extends Border {
    override def diff(r: Rectangle): List[Border]
      = if (!r.containsCol(col)) List(this) else (rows diff r.rowRange).map{range => BorderV(range, col) }

    override def toRectangle: Rectangle = Rectangle(Pos(rows.start.toInt, col), Pos(rows.end.toInt, col))
  }

  case class Rectangle(p0: Pos, p1: Pos) {
    val min: Pos = p0 min p1
    val max: Pos = p0 max p1
    lazy val colRange: Range = Range.inclusive(min.col, max.col)
    lazy val rowRange: Range = Range.inclusive(min.row, max.row)
    def containsCol(col: Int): Boolean = min.col <= col && col <= max.col
    def containsRow(row: Int): Boolean = min.row <= row && row <= max.row
    def contains(pos: Pos): Boolean = containsRow(pos.row) && containsCol(pos.col)
    def union(rhs: Rectangle): Rectangle = Rectangle(min min rhs.min, max max rhs.max)
    def area: Long = (max.row - min.row + 1).toLong * (max.col - min.col + 1).toLong
    def borders: List[Border] = List(
      BorderH(min.row - 1, colRange), BorderH(max.row + 1, colRange),
      BorderV(rowRange, min.col - 1), BorderV(rowRange, max.col + 1)
    )
  }
  object Rectangle {
    def unit(at: Pos): Rectangle = Rectangle(at, at)
    def start: Rectangle = Rectangle.unit(Pos(0, 0))
  }

  case class Instruction(dir: Pos, n: Int)
  object Instruction {
    private val dirs1 = Map("U" -> Pos.UP, "D" -> Pos.DOWN, "L" -> Pos.LEFT, "R" -> Pos.RIGHT)
    private val dirs2 = Map('0' -> Pos.RIGHT, '1' -> Pos.DOWN, '2' -> Pos.LEFT, '3' -> Pos.UP)
    def parse1(line: String): Option[Instruction] = Some(line.split(' ')).filter(_.length == 3).map{
      case Array(dir, n, _) => Instruction(dirs1(dir), n.toInt)
    }
    def parse2(line: String): Option[Instruction] = Some(line.split(' ')).filter(_.length == 3).map{
      case Array(_, _, c) => Instruction(dirs2(c.charAt(c.length - 2)), Integer.parseInt(c.drop(2).dropRight(2), 16))
    }
  }

  def fillOne(b: Border, start: Lagoon): Lagoon = {
    var lagoon: Lagoon = start
    var frontier: List[Border] = List(b)
    while (frontier.nonEmpty) {
      val current = frontier.head
      frontier = frontier.tail
      val diff = current diff lagoon.dug
      if (diff.nonEmpty) {
        val next = lagoon.expanded(diff.head.toRectangle)
        frontier = frontier ++ diff.tail
        lagoon = if (next.nonEmpty) new Lagoon(lagoon.dug ++ next, start.bounds) else start
        frontier = if (next.nonEmpty) frontier ++ next.get.borders else Nil
      }
    }
    lagoon
  }

  case class Lagoon(dug: Set[Rectangle], bounds: Rectangle) {
    def isDug(pos: Pos): Boolean = dug.exists(_.contains(pos))

    def expanded(t: Rectangle): Option[Rectangle] = if ((bounds union t) != bounds) None else {
      minCol(t).zip(maxCol(t)).flatMap{case (c0, c1) =>
        val t2 = Rectangle(Pos(t.min.row, c0), Pos(t.max.row, c1))
        minRow(t2).zip(maxRow(t2)).map{case (r0, r1) => Rectangle(Pos(r0, c0), Pos(r1, c1)) }
      }
    }

    case class Diff(remaining: List[Range], x: Option[Int] = None) {
      def this(r: Range) = this(List(r))
      def add(r: Range)(func: Option[Int] => Option[Int]): Diff = Diff(remaining.flatMap(_ diff r), func(x))
    }

    def left(r: Rectangle): Iterator[Rectangle]
      = dug.iterator.filter(_.rowRange overlaps r.rowRange).filter(_.max.col < r.min.col)
    def right(r: Rectangle): Iterator[Rectangle]
      = dug.iterator.filter(_.rowRange overlaps r.rowRange).filter(_.min.col > r.max.col)
    def above(r: Rectangle): Iterator[Rectangle]
      = dug.iterator.filter(_.colRange overlaps r.colRange).filter(_.max.row < r.min.row)
    def below(r: Rectangle): Iterator[Rectangle]
      = dug.iterator.filter(_.colRange overlaps r.colRange).filter(_.min.row > r.max.row)

    def minCol(r: Rectangle): Option[Int]
      = Some(left(r).foldLeft(new Diff(r.rowRange)){(diff, r) => diff.add(r.rowRange)(_ max r.max.col) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ + 1))

    def maxCol(r: Rectangle): Option[Int]
      = Some(right(r).foldLeft(new Diff(r.rowRange)){(diff, r) => diff.add(r.rowRange)(_ min r.min.col) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ - 1))

    def minRow(r: Rectangle): Option[Int]
      = Some(above(r).foldLeft(new Diff(r.colRange)){(diff, r) => diff.add(r.colRange)(_ max r.max.row) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ + 1))

    def maxRow(r: Rectangle): Option[Int]
      = Some(below(r).foldLeft(new Diff(r.colRange)){(diff, r) => diff.add(r.colRange)(_ min r.min.row) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ - 1))

    def filled(): Lagoon
      = dug.iterator.flatMap(_.borders).foldLeft(this){(current, border) => fillOne(border, current) }

    def matrix(): Matrix[Char] = Matrix(
      (bounds.min to bounds.max).map{p => if (isDug(p)) '#' else '.' }
                                .grouped(bounds.max.col - bounds.min.col + 1)
                                .map(_.toArray))

    def area: Long = dug.iterator.map(_.area).sum
  }
  object Lagoon {
    def start: Lagoon = Lagoon(Set.empty, Rectangle.start)
    def apply(insts: Iterator[Instruction]): Lagoon = insts.foldLeft((Pos(0,0), Lagoon.start)){
      case ((pos,state), inst) =>
        val trench = Rectangle(pos + inst.dir, pos + inst.dir*inst.n)
        (trench.p1, Lagoon(dug = state.dug + trench, state.bounds union trench))
    }._2
  }

  val lines = data.getLines().toArray
  val part1 = Lagoon(lines.iterator.flatMap(Instruction.parse1)).filled()
  println(s"Part 1: ${part1.area}")

  val part2 = Lagoon(lines.iterator.flatMap(Instruction.parse2)).filled()
  println(s"Part 2: ${part2.area}")
}
