package Year2023

import common.Pos

object Day03 extends Year2023(3) {
  private val number = "[0-9]+".r
  trait Feature {
    def start: Pos
    def len: Int
    def posList: Iterator[Pos] = (0 until len).iterator.map{j => Pos(start.row, start.col + j) }
    def surrounding: Iterator[Pos] = Seq(start.row - 1, start.row + 1).iterator.flatMap { i =>
      (start.col - 1 to start.col + len).map { j => Pos(i, j) }
    } ++ Seq(Pos(start.row, start.col - 1), Pos(start.row, start.col + len))
  }
  case class Symbol(value: Char, start: Pos) extends Feature { def len: Int = 1 }
  implicit class CharMethods(x: Char) { def isSymbol: Boolean = !x.isDigit && x != '.' }

  case class Number(value: Long, start: Pos, len: Int) extends Feature

  val (numbers, symbols) = data.getLines().zipWithIndex
                                    .foldLeft((Set.empty[Number], Set.empty[Symbol])){case ((nums, syms), (line, i)) =>
    val symbols = line.iterator.zipWithIndex.filter(_._1.isSymbol).map{case (c,j) => Symbol(c, Pos(i, j)) }
    val numbers = number.findAllMatchIn(line).map{m =>
      Number(m.group(0).toLong, Pos(i, m.start), m.end - m.start)
    }
    (nums ++ numbers, syms ++ symbols)
  }

  val part1 = numbers.iterator.filter(_.surrounding.exists(x => symbols.exists(_.start == x))).map(_.value).sum
  println(s"Part 1: $part1")

  // ~O(1) lookup from Pos to Number (basically a sparse matrix)
  val numMap: Map[Pos, Number] = numbers.flatMap{n => n.posList.map{p => p -> n}}.toMap
  val part2 = symbols.iterator.filter(_.value == '*').flatMap{sym =>
    val adjacent = sym.surrounding.flatMap(numMap.get).toSet
    if (adjacent.size == 2) Some(adjacent.map(_.value).product) else None
  }.sum
  println(s"Part 2: $part2")
}
