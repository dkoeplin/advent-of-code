package Year2022

import scala.collection.mutable

class Monkey(_id: Long, initial: Seq[Long], op: Long => Long, div: Long, next: Long => Int) {
  val id: Long = _id
  val items: mutable.Queue[Long] = mutable.Queue.empty[Long] ++ initial
  val inspect: Long => Long = op
  val divider: Long = div
  val test: Long => Int = next
  var inspected: Long = 0
  def takeTurn(monkeys: Array[Monkey], divide: Long => Long): Unit = {
    while (items.nonEmpty) {
      val item = items.dequeue()
      val worry = divide(inspect(item))
      val next = test(worry)
      //println(s"$id: $item -> $worry to monkey $next")
      monkeys(next).items.enqueue(worry)
      inspected += 1
    }
  }
}
object Monkey {
  val MONKEY = "Monkey "
  val STARTING_ITEMS = "  Starting items: "
  val OPERATION = "  Operation: new = "
  val TEST_COND = "  Test: divisible by "
  val TRUE_COND = "    If true: throw to monkey "
  val FALSE_COND = "    If false: throw to monkey "
  def parse(lines: Iterator[String]): Monkey = {
    val id = lines.next().drop(MONKEY.length).dropRight(1).toLong
    val items = lines.next().drop(STARTING_ITEMS.length).split(",").map(_.trim.toLong)
    val op = lines.next().drop(OPERATION.length).split(" ")
    val evaluate: Long => Long = {x: Long =>
      val lhs = if (op(0) == "old") x else op(0).toLong
      val rhs = if (op(2) == "old") x else op(2).toLong
      op(1) match {
        case "*" => lhs * rhs
        case "+" => lhs + rhs
        case o => throw new Exception("Unknown operation: " + o)
      }
    }
    val div = lines.next().drop(TEST_COND.length).toLong
    val true_cond = lines.next().drop(TRUE_COND.length).toInt
    val false_cond = lines.next().drop(FALSE_COND.length).toInt
    val next: Long => Int = {x: Long =>
      if (x % div == 0) true_cond else false_cond
    }
    lines.nextOption() // Skip empty line
    new Monkey(id, items, evaluate, div, next)
  }
  def parseAll(lines: Iterator[String]): Array[Monkey] = {
    var monkeys: Array[Monkey] = Array.empty
    while (lines.hasNext) {
      monkeys = monkeys :+ Monkey.parse(lines)
    }
    monkeys
  }
}

object Day11 extends App {
  def rounds(n: Int)(divide: Array[Monkey] => (Long => Long)): Unit = {
    val file = scala.io.Source.fromFile("data/2022/11")
    val lines = file.getLines()
    val monkeys: Array[Monkey] = Monkey.parseAll(lines)
    val divider = divide(monkeys)
    (1 to n).foreach { round =>
      monkeys.foreach(_.takeTurn(monkeys, divider))
    }
    val result = monkeys.map(_.inspected).sortBy { x => -x }.take(2).product
    println(s"Result: $result")
  }
  rounds(20){_ => {x: Long => x / 3}}
  rounds(10000){ monkeys =>
    val divider = monkeys.map(_.divider).product
    val func = {x: Long => x % divider}
    func
  }
}
