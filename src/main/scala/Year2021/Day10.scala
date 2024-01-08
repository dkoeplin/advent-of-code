package Year2021

import scala.collection.mutable

case class Brackets() {
  val stack: mutable.Buffer[(Int,Int)] = mutable.Buffer.empty[(Int,Int)]
  def open(c: Char, col: Int): Boolean = { stack += ((Brackets.openType(c), col)); false }
  def close(c: Char, col: Int): Boolean = {
    val valid = stack.lastOption.forall{case (o,_) => o == Brackets.closeType(c) }
    if (valid && stack.nonEmpty) stack.remove(stack.size - 1)
    !valid
  }
}
object Brackets {
  val open = "([{<"
  val close = ")]}>"
  val closeType: Map[Char,Int] = close.zipWithIndex.toMap
  val openType: Map[Char,Int] = open.zipWithIndex.toMap
  val closeChar: Map[Int,Char] = close.zipWithIndex.map{case (c,i) => i -> c}.toMap
  val openChar: Map[Int,Char] = open.zipWithIndex.map{case (c,i) => i -> c}.toMap
  val part1Scores: Array[Int] = Array[Int](3, 57, 1197, 25137)
  val part2Scores: Array[Int] = Array[Int](1, 2, 3, 4)
}

object Day10 extends common.AoC(10, 2021) {
  val Open = "([(\\[{<])".r
  val Close = "([)\\]}>])".r

  def error(code: String, line: Int, col: Int, message: String): Unit = {
    Console.err.println(s"${line+1}:${col+1}: $message")
    Console.err.println(code)
    Console.err.println(" " * col + "^")
  }

  val (part1, part2) = data.getLines().zipWithIndex.foldLeft((0, List.empty[Long])){case (accum, (code, line)) =>
    val brackets = Brackets()
    val unmatched = code.zipWithIndex.find{
      case (Open(c), col) => brackets.open(c, col)
      case (Close(c), col) => brackets.close(c, col)
      case (c, col) => error(code, line, col, s"Invalid character: $c"); false
    }
    val part1 = unmatched.map{case (c,_) => Brackets.part1Scores(Brackets.closeType(c)) }.getOrElse(0)
    unmatched.foreach{case (c,col) =>
      val expected = Brackets.closeChar(brackets.stack.last._1)
      error(code, line, col, s"Expected $expected, but found $c instead.")
    }
    val part2 = if (unmatched.isEmpty) {
      Some(brackets.stack.reverseIterator.foldLeft(0L){case (accum, (tp,_)) => 5*accum + Brackets.part2Scores(tp) })
    } else None
    (accum._1 + part1, accum._2 ++ part2)
  }
  val part2Score = part2.sorted.apply(part2.size / 2)

  println(s"Part 1: Score: $part1")
  println(s"Part 2: Score: $part2Score")
}
