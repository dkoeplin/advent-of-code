package Year2023

import common.Pos
import common.Range
import common.immutable.Matrix
import common.implicits.OptionOps._

object Day18 extends Year2023(18) {
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
    def nearby: Iterator[Pos] = Iterator(min, max).flatMap(p => Pos.all.iterator.map(_ + p))

    override def toString: String = s"[${min.row},${min.col}] to [${max.row},${max.col}]"
  }
  object Rectangle {
    def unit(at: Pos): Rectangle = Rectangle(at, at)
    def start: Rectangle = Rectangle.unit(Pos(0, 0))
  }

  case class Instruction(dir: Pos, n: Int) {
    def t: Instruction = Instruction(dir.t, n)
  }
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

  case class State(frontier: List[Pos], lagoon: Lagoon)
  object Dug {
    def unapply(state: State): Option[State] = {
      if (state.lagoon.isDug(state.frontier.head)) Some(State(state.frontier.tail, state.lagoon)) else None
    }
  }
  object Bounded {
    def unapply(state: State): Option[State] = {
      val p = state.frontier.head
      if (!state.lagoon.bounds.contains(p) || state.lagoon.isDug(p)) None else {
        val l = state.lagoon.dug.iterator.filter{r => r.containsRow(p.row) && r.max.col < p.col}.maxByOption(_.max.col).map(_.max.col)
        val r = state.lagoon.dug.iterator.filter{r => r.containsRow(p.row) && r.min.col > p.col}.minByOption(_.min.col).map(_.min.col)
        l.zip(r).flatMap{case (l, r) =>
          val init = Rectangle(Pos(p.row, l + 1), Pos(p.row, r - 1))
          val u = state.lagoon.minRow(init)
          val d = state.lagoon.maxRow(init)
          u.zip(d).map{case (u, d) =>
            val rect = Rectangle(Pos(u, l + 1), Pos(d, r - 1))
            // println(s"$p => $init => $rect")
            State(state.frontier.tail ++ rect.nearby, Lagoon(state.lagoon.dug + rect, state.lagoon.bounds))
          }
        }
      }
    }
  }

  case class Lagoon(dug: Set[Rectangle], bounds: Rectangle) {
    def isDug(pos: Pos): Boolean = dug.exists(_.contains(pos))

    case class Diff(remaining: List[Range], row: Option[Int] = None) {
      def add(r: Rectangle)(func: Option[Int] => Option[Int]): Diff = {
        val next = remaining.flatMap(_ diff r.colRange)
        // if (remaining.nonEmpty) println(s"${remaining.mkString("+")} - ${r.colRange} = ${next.mkString("+")}")
        Diff(next, func(row))
      }
    }

    def above(r: Rectangle): Iterator[Rectangle]
      = dug.iterator.filter(_.colRange overlaps r.colRange).filter(_.max.row < r.min.row)
    def below(r: Rectangle): Iterator[Rectangle]
      = dug.iterator.filter(_.colRange overlaps r.colRange).filter(_.min.row > r.min.row)

    def minRow(r: Rectangle): Option[Int]
      = Some(above(r).foldLeft(Diff(List(r.colRange))){(diff, r) => diff.add(r)(_ max r.max.row) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.row.map(_ + 1))

    def maxRow(r: Rectangle): Option[Int]
      = Some(below(r).foldLeft(Diff(List(r.colRange))){(diff, r) => diff.add(r)(_ min r.min.row) })
        .filter(_.remaining.isEmpty)
        .flatMap(_.row.map(_ - 1))

    def fillOne(pos: Pos): Lagoon = LazyList.iterate(State(List(pos), this)){
      case current @ Dug(next) =>
        // println(s"${current.frontier.head}: DUG")
        next
      case current @ Bounded(next) =>
        // println(next.lagoon.matrix())
        next
      case _ => State(Nil, this)
    }.dropWhile(_.frontier.nonEmpty).head.lagoon

    def filled(): Lagoon = {
      dug.iterator.flatMap(_.nearby).filterNot(isDug).foldLeft(this){(current, pos) =>
        current.fillOne(pos)
      }
    }

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
        (pos + inst.dir*inst.n, Lagoon(dug = state.dug + trench, state.bounds union trench))
    }._2
  }

  val lines = data.getLines().toArray
  val part1 = Lagoon(lines.iterator.flatMap(Instruction.parse1)).filled()
  println(s"Part 1: ${part1.area}")
  write("pt2", part1.matrix().toString)
  // println(trench.matrix())
  // println("-------")
  // println(part1.matrix())
  val part2 = Lagoon(lines.iterator.flatMap(Instruction.parse2)).filled()
  println(s"Part 2: ${part2.area}")
  // val part2_t = Lagoon(insts2.iterator.map(_.t)).filled()
  // println(s"Part 2(t): ${part2_t.area}")
}
