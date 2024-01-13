package Year2023

import common.immutable.Pos

object Day03 extends common.AoC(3, 2023) {
  private val number = "[0-9]+".r
  trait Feature {
    def start: Pos[Long]
    def len: Int
    def posList: Iterator[Pos[Long]] = (0 until len).iterator.map{j => Pos(start.r, start.c + j) }
    def surrounding: Iterator[Pos[Long]] = Seq(start.r - 1, start.r + 1).iterator.flatMap { i =>
      (start.c - 1 to start.c + len).map{ j => Pos(i, j) }
    } ++ Seq(Pos(start.r, start.c - 1), Pos(start.r, start.c + len))
  }
  case class Symbol(value: Char, start: Pos[Long]) extends Feature { def len: Int = 1 }
  // A symbol is any non-digit that isn't a period
  implicit class CharMethods(x: Char) { def isSymbol: Boolean = !x.isDigit && x != '.' }

  case class Number(value: Long, start: Pos[Long], len: Int) extends Feature

  // Memoize the position of numbers and symbols in two sets
  val (numbers, symbols) = data.getLines().zipWithIndex
                                    .foldLeft((Set.empty[Number], Set.empty[Symbol])){case ((nums, syms), (line, i)) =>
    val symbols = line.iterator.zipWithIndex.filter(_._1.isSymbol).map{case (c,j) => Symbol(c, Pos(i, j)) }
    val numbers = number.findAllMatchIn(line).map{m =>
      Number(m.group(0).toLong, Pos(i, m.start), m.end - m.start)
    }
    (nums ++ numbers, syms ++ symbols)
  }

  // All numbers with at least one adjacent symbol
  val part1 = numbers.iterator.filter(_.surrounding.exists(x => symbols.exists(_.start == x)))
                              .map(_.value).sum
  println(s"Part 1: $part1")

  // All * symbols with exactly two adjacent numbers
  val numMap: Map[Pos[Long], Number] = numbers.flatMap{ n => n.posList.map{ p => p -> n}}.toMap
  val part2 = symbols.iterator.filter(_.value == '*').flatMap{sym =>
    val adjacent = sym.surrounding.flatMap(numMap.get).toSet
    if (adjacent.size == 2) Some(adjacent.map(_.value).product) else None
  }.sum
  println(s"Part 2: $part2")
}
