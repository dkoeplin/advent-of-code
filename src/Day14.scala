import scala.collection.mutable

case class Poly(a: Char, b: Char)
object Poly {
  def apply(x: String): Poly = Poly(x.charAt(0),x.charAt(1))
}

object Day14 extends App {
  val Rule = "([A-Z])([A-Z]) -> ([A-Z])".r
  val file = scala.io.Source.fromFile("./data/14")
  val lines = file.getLines()
  val init: String = lines.next()
  val rules: Map[Poly,Char] = lines.flatMap{case Rule(a,b,c) => Some(Poly(a.head,b.head) -> c.head); case _ => None }.toMap

  val pairs = mutable.Map.empty[Poly,Long]
  init.sliding(2,1).foreach{x =>
    val pair = Poly(x)
    pairs(pair) = pairs.getOrElse(pair, 0L) + 1L
  }

  def applyRules(current: mutable.Map[Poly,Long]): mutable.Map[Poly,Long] = {
    val next = mutable.Map.empty[Poly,Long]
    current.foreach{
      case (pair,n) if rules.contains(pair) =>
        val c = rules(pair)
        val left = Poly(pair.a, c)
        val right = Poly(c, pair.b)
        next(left) = next.getOrElse(left, 0L) + n
        next(right) = next.getOrElse(right, 0L) + n
      case (pair,n) => next(pair) = n
    }
    next
  }
  case class Part(pairs: mutable.Map[Poly,Long], mce: (Char,Long), lce: (Char,Long), diff: Long)
  def part(offset: Int, steps: Int, initial: mutable.Map[Poly,Long]): Part = {
    val result = (offset until offset + steps).foldLeft(initial){case (current, step) => applyRules(current) }
    val counts = mutable.Map.empty[Char,Long]
    result.foreach{case (Poly(a,_), n) => counts(a) = counts.getOrElse(a, 0L) + n }
    counts(init.last) = counts.getOrElse(init.last, 0L) + 1L
    val mce = counts.maxBy(_._2)
    val lce = counts.minBy(_._2)
    Part(result, mce, lce, mce._2 - lce._2)
  }

  val part1 = part(1, 10, pairs)
  val part2 = part(10, 30, part1.pairs)

  println(s"Part 1: ${part1.mce}, ${part1.lce} ${part1.diff}")
  println(s"Part 2: ${part2.mce}, ${part2.lce} ${part2.diff}")
}
