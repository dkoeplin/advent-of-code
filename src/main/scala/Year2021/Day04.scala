package Year2021

import common.immutable.Pos

import scala.collection.mutable
import scala.util.Try

case class Board(id: Int, board: Seq[Seq[Int]]) {
  val numRows: Int = board.size
  val numCols: Int = board.head.size
  var score: Int = board.map(_.sum).sum
  val rows: mutable.Seq[Int] = mutable.Seq.fill(numCols)(0)
  val cols: mutable.Seq[Int] = mutable.Seq.fill(numRows)(0)

  def posOf(x: Int): Option[Pos[Int]] = board.zipWithIndex.flatMap{case (row,idx) =>
    Some(Pos(idx, row.indexOf(x))).filter(_.h != -1)
  }.headOption

  def register(x: Int): Boolean = posOf(x).exists{pos =>
    rows(pos.h) += 1
    cols(pos.w) += 1
    score = score - x
    println(s"Year2021.Board $id: $x at (${pos.h}, ${pos.w}) (row: ${rows(pos.h)} / $numRows, col: ${cols(pos.w)} / $numCols)")
    rows(pos.h) == numRows || cols(pos.w) == numCols
  }
}

object Day04 extends common.AoC(4, 2023) {
  val lines = data.getLines()

  val numbers = lines.next().split(",").flatMap(str => if (str.isEmpty) None else Some(str.toInt))
  var boards: Set[Board] = Set.empty
  while (lines.hasNext) {
    val entries: Seq[Seq[Int]] = lines.takeWhile(_.nonEmpty).map(_.split("\\s+").flatMap{ x => Try {x.toInt }.toOption }.toSeq).toSeq
    if (entries.nonEmpty)
      boards += Board(boards.size, entries)
  }

  var iter = numbers.iterator
  var called: Int = 0
  var winner: Option[(Board, Int)] = None
  var last: Option[Board] = None
  while (iter.hasNext && boards.nonEmpty) {
    called = iter.next()
    println(s"Called $called")
    val (won, ongoing) = boards.partition(_.register(called))
    if (winner.isEmpty && won.nonEmpty)
      winner = Some((won.head, called))
    if (last.isEmpty && boards.size == 1)
      last = won.lastOption
    boards = ongoing
  }
  val part2 = last.get.score
  println(s"Part 1: called: ${winner.get._2}, score: ${winner.get._1.score}, final: ${winner.get._1.score * winner.get._2}")
  println(s"Part 2: called: $called, score: $part2, final: ${part2 * called}")
}
