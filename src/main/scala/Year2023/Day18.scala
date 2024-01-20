package Year2023

import common.immutable.Pos.Idx
import common.immutable.{Cube, Matrix}
import common.implicits.OptionOps._

object Day18 extends common.AoC(18, 2023) {
  type Rectangle = Cube[Int]

  case class Instruction(dir: Idx, n: Int)
  object Instruction {
    private val dirs1 = Map("U" -> Idx.D2.U, "D" -> Idx.D2.D, "L" -> Idx.D2.L, "R" -> Idx.D2.R)
    private val dirs2 = Map('0' -> Idx.D2.R, '1' -> Idx.D2.D, '2' -> Idx.D2.L, '3' -> Idx.D2.U)
    def parse1(line: String): Option[Instruction] = Some(line.split(' ')).filter(_.length == 3).map{
      case Array(dir, n, _) => Instruction(dirs1(dir), n.toInt)
    }
    def parse2(line: String): Option[Instruction] = Some(line.split(' ')).filter(_.length == 3).map{
      case Array(_, _, c) => Instruction(dirs2(c.charAt(c.length - 2)), Integer.parseInt(c.drop(2).dropRight(2), 16))
    }
  }

  def fillOne(b: Rectangle, start: Lagoon): Lagoon = {
    var lagoon: Lagoon = start
    var frontier: List[Rectangle] = List(b)
    while (frontier.nonEmpty) {
      val current = frontier.head
      frontier = frontier.tail
      val diff = (current diff lagoon.dug).toArray
      if (diff.nonEmpty) {
        val next = lagoon.expanded(diff.head)
        frontier = frontier ++ diff.tail
        lagoon = if (next.nonEmpty) new Lagoon(lagoon.dug ++ next, start.bounds) else start
        frontier = if (next.nonEmpty) frontier ++ next.get.borders().map(_.volume) else Nil
      }
    }
    lagoon
  }

  case class Lagoon(dug: Set[Rectangle], bounds: Rectangle) {
    def isDug(pos: Idx): Boolean = dug.exists(_.contains(pos))

    def expanded(t: Rectangle): Option[Rectangle] = if ((bounds union t) != bounds) None else {
      minCol(t).zip(maxCol(t)).flatMap{case (c0, c1) =>
        val t2 = Cube(Idx(t.min.r, c0), Idx(t.max.r, c1))
        minRow(t2).zip(maxRow(t2)).map{case (r0, r1) => Cube(Idx(r0, c0), Idx(r1, c1)) }
      }
    }

    case class Diff(remaining: List[Rectangle], x: Option[Int] = None) {
      def this(r: Rectangle) = this(List(r))
      def add(r: Rectangle)(func: Option[Int] => Option[Int]): Diff = Diff(remaining.flatMap(_ diff r), func(x))
    }

    def left(r: Rectangle): Iterator[Rectangle] = dug.iterator.filter(_.rows overlaps r.rows).filter(_.max.c < r.min.c)
    def right(r: Rectangle): Iterator[Rectangle] = dug.iterator.filter(_.rows overlaps r.rows).filter(_.min.c > r.max.c)
    def above(r: Rectangle): Iterator[Rectangle] = dug.iterator.filter(_.cols overlaps r.cols).filter(_.max.r < r.min.r)
    def below(r: Rectangle): Iterator[Rectangle] = dug.iterator.filter(_.cols overlaps r.cols).filter(_.min.r > r.max.r)

    def minCol(r: Rectangle): Option[Int]
      = Some(left(r).foldLeft(new Diff(r.rows)){(diff, r) => diff.add(r.rows)(_ max r.max.c) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ + 1))

    def maxCol(r: Rectangle): Option[Int]
      = Some(right(r).foldLeft(new Diff(r.rows)){(diff, r) => diff.add(r.rows)(_ min r.min.c) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ - 1))

    def minRow(r: Rectangle): Option[Int]
      = Some(above(r).foldLeft(new Diff(r.cols)){(diff, r) => diff.add(r.cols)(_ max r.max.r) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ + 1))

    def maxRow(r: Rectangle): Option[Int]
      = Some(below(r).foldLeft(new Diff(r.cols)){(diff, r) => diff.add(r.cols)(_ min r.min.r) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.x.map(_ - 1))

    def filled(): Lagoon = dug.iterator.flatMap(_.borders()).foldLeft(this){(current, border) => fillOne(border.volume, current)}

    def matrix(): Matrix[Char] = Matrix(bounds, bounds.iterator.map{p => if (isDug(p)) '#' else '.' }.toArray)

    def area: Int = dug.iterator.map(_.size).sum
  }
  object Lagoon {
    def start: Lagoon = Lagoon(Set.empty, Cube.unit(Idx(0,0)))
    def apply(insts: Iterator[Instruction]): Lagoon = insts.foldLeft((Idx(0,0), Lagoon.start)){
      case ((pos,state), inst) =>
        val trench = Cube(pos + inst.dir, pos + inst.dir*inst.n)
        (trench.r, Lagoon(dug = state.dug + trench, state.bounds union trench))
    }._2
  }

  val lines = data.getLines().toArray
  val part1 = Lagoon(lines.iterator.flatMap(Instruction.parse1)).filled()
  println(s"Part 1: ${part1.area}")

  val part2 = Lagoon(lines.iterator.flatMap(Instruction.parse2)).filled()
  println(s"Part 2: ${part2.area}")
}
