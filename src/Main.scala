import scala.collection.mutable
import scala.util.Try

case class Pos(row: Int, col: Int)

case class Board(board: Seq[Seq[Int]]) {
  val numRows: Int = board.size
  val numCols: Int = board.head.size
  var score: Int = board.map(_.sum).sum
  val rows: mutable.Seq[Int] = mutable.Seq.fill(numCols)(0)
  val cols: mutable.Seq[Int] = mutable.Seq.fill(numRows)(0)

  def posOf(x: Int): Option[Pos] = board.zipWithIndex.flatMap{case (row,idx) =>
    Some(Pos(idx, row.indexOf(x))).filter(_.col != -1)
  }.headOption

  def register(x: Int): Boolean = posOf(x).exists{pos =>
    rows(pos.row) += 1
    cols(pos.col) += 1
    score = score - x
    println(s"Board $x at (${pos.row}, ${pos.col}) (row: ${rows(pos.row)} / $numRows, col: ${cols(pos.col)} / $numCols)")
    rows(pos.row) == numRows || cols(pos.col) == numCols
  }
}

object Main extends App {
  val file = scala.io.Source.fromFile("./data/4")
  val lines = file.getLines()

  val numbers = lines.next().split(",").flatMap(str => if (str.isEmpty) None else Some(str.toInt))
  var boards: Seq[Board] = Seq.empty

  while (lines.hasNext) {
    val entries: Seq[Seq[Int]] = lines.takeWhile(_.nonEmpty).map(_.split("\\s+").flatMap{ x => Try {x.toInt }.toOption }.toSeq).toSeq
    if (entries.nonEmpty)
      boards = boards :+ Board(entries)
  }

  var iter = numbers.iterator
  var called: Int = 0
  var winner: Option[Board] = None
  while (iter.hasNext && winner.isEmpty) {
    called = iter.next()
    println(s"Called $called")
    winner = boards.find(_.register(called))
  }
  assert(winner.nonEmpty)
  val score = winner.get.score
  println(s"called: $called, score: $score, final: ${score * called}")
}
