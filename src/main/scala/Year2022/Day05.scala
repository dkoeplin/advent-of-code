package Year2022

import scala.util.matching.Regex

object Move {
  val pattern: Regex = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
  def unapply(x: String): Option[(Int, Int, Int)] = pattern.findFirstMatchIn(x).map{m =>
    (m.group(1).toInt, m.group(2).toInt - 1, m.group(3).toInt - 1)
  }
}
case class Stacks(stacks: Seq[Seq[Char]]) {
  def tops: String = stacks.flatMap{stack => stack.lastOption }.mkString("")
  def move(n: Int, src: Int, dst: Int, reverse: Boolean): Stacks = {
    val moved = if (reverse) stacks(src).takeRight(n).reverse else stacks(src).takeRight(n)
    Stacks(stacks.zipWithIndex.map{
      case (stack, `src`) => stack.dropRight(n)
      case (stack, `dst`) => stack ++ moved
      case (stack, _) => stack
    })
  }
}
object Stacks {
  def parse(x: Array[String]): Stacks = {
    val lines = x.takeWhile{line => !line.startsWith("move") }.dropRight(2)
    val size = (lines.map(_.length).max + 1) / 4
    Stacks(Seq.tabulate(size){i =>
      val pos = i * 4 + 1
      lines.reverseIterator.map{line => if (line.length >= pos) line(pos) else ' '}.filter(_ != ' ').toSeq
    })
  }
}
object Day05 extends App {
  val file = scala.io.Source.fromFile("data/2022/05")
  val lines = file.getLines().toArray
  val stacks = Stacks.parse(lines)
  val part1 = lines.foldLeft(stacks){
    case (stacks, Move(n, src, dst)) => stacks.move(n, src, dst, reverse=true)
    case (stacks, _) => stacks
  }
  println(s"Part1: ${part1.tops}")
  val part2 = lines.foldLeft(stacks){
    case (stacks, Move(n, src, dst)) => stacks.move(n, src, dst, reverse=false)
    case (stacks, _) => stacks
  }
  println(s"Part2: ${part2.tops}")
}
