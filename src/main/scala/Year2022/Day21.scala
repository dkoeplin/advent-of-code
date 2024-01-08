package Year2022


object Day21 extends common.AoC(21, 2022) {
  class Rational(x: BigInt, y: BigInt) {
    private val g: BigInt = x.gcd(y).abs
    private val neg: Boolean = (x < 0) != (y < 0)
    val a: BigInt = if (neg) -x.abs / g else x.abs / g
    val b: BigInt = y.abs / g
    def unary_- : Rational = Rational(-x, y)
    def +(rhs: Rational): Rational = Rational(a * rhs.b + rhs.a * b, b * rhs.b)
    def -(rhs: Rational): Rational = Rational(a * rhs.b - rhs.a * b, b * rhs.b)
    def *(rhs: Rational): Rational = Rational(a * rhs.a, b * rhs.b)
    def /(rhs: Rational): Rational = Rational(a * rhs.b, b * rhs.a)

    // Rationals are minimized on construction, so just have to check exact matches
    def ==(rhs: Rational): Boolean = (a == rhs.a) && (b == rhs.b)
    def !=(rhs: Rational): Boolean = (a != rhs.a) || (b != rhs.b)
    def toLong: Long = (a / b).toLong

    override def toString: String = if (b == 1) a.toString else s"($a/$b)"
  }
  object Rational {
    def apply(x: BigInt, y: BigInt) = new Rational(x, y)
    val one = new Rational(1, 1)
    val zero = new Rational(0, 1)
  }

  // (A*X + b)/D
  class Symbol(components: Map[Int, Rational]) {
    val xs: Map[Int, Rational] = components.filter{x => x._2 != Rational.zero || x._1 == 0 }
    def unary_- : Symbol = Symbol(xs.view.mapValues{x => -x}.toMap)
    def zip(rhs: Symbol)(func: (Rational, Rational) => Rational): Symbol = Symbol((xs.keySet ++ rhs.xs.keySet).map { e =>
      e -> func(xs.getOrElse(e, Rational.zero), rhs.xs.getOrElse(e, Rational.zero))
    }.toMap)
    def +(rhs: Symbol): Symbol = zip(rhs)(_+_)
    def -(rhs: Symbol): Symbol = zip(rhs)(_-_)
    def *(rhs: Symbol): Symbol = {
      Symbol(xs.keys.flatMap{e1 => val r1 = xs(e1); rhs.xs.keys.map{e2 => (e1 + e2) -> (r1 * rhs.xs(e2)) }}
                    .groupBy(_._1).view.mapValues(_.foldLeft(Rational.zero){(a, b) => a + b._2 }).toMap)
    }
    def /(rhs: Symbol): Symbol = rhs.xs.size match {
      case 1 =>
        val (e2, r2) = rhs.xs.head
        if (r2 == Rational.zero) throw new Exception("Divide by zero")
        Symbol(xs.map{case (e1, r1) => (e1 - e2) -> (r1 / r2) })
      case n => throw new Exception("Not supported: divide by Symbol with > 1 component")
    }
    def ==(rhs: Symbol): Boolean = (xs.keySet ++ rhs.xs.keySet).forall{e =>
      val r1 = xs.getOrElse(e, Rational.zero)
      val r2 = rhs.xs.getOrElse(e, Rational.zero)
      r1 == r2
    }

    override def toString: String = xs.keys.toList.sorted.map{e =>
      val r = xs(e)
      if (e == 0) r.toString else if (e == 1) s"$r*x" else s"$r*x^$e"
    }.mkString(" + ")
  }
  object Symbol {
    def apply(xs: Map[Int, Rational]): Symbol = new Symbol(xs)
    def apply(v: Long): Symbol = new Symbol(Map(0 -> Rational(v, 1)))
    val x: Symbol = Symbol(Map(1 -> Rational.one))

    def solve(s: Symbol): Seq[Rational] = {
      if (s.xs.keys.max == 1) {
        Seq(-s.xs(0) / s.xs(1))
      } else {
        throw new Exception("Eh.")
      }
    }
  }

  abstract class Monkey {
    def evaluate(map: scala.collection.Map[String, Symbol]): Option[Symbol]
  }

  case class Exp(lhs: String, rhs: String, op: Char) extends Monkey {
    def evaluate(map: scala.collection.Map[String, Symbol]): Option[Symbol] =
      map.get(lhs).zip(map.get(rhs)).map{case (a, b) => op match {
        case '*' => a * b
        case '+' => a + b
        case '/' => a / b
        case '-' => a - b
        case '=' => if (a == b) Symbol(1) else Symbol(0)
      }}
  }

  case class Const(value: Long) extends Monkey {
    def evaluate(map: scala.collection.Map[String, Symbol]): Option[Symbol] = Some(Symbol(value))
  }

  object Monkey {
    private val Constant = "(.+): ([0-9]+)".r
    private val Express = "(.+): (.+) ([+-/*]) (.+)".r

    def parse(line: String): (String, Monkey) = line match {
      case Constant(name, v) => (name, Const(v.toInt))
      case Express(name, a, op, b) => (name, Exp(a, b, op.charAt(0)))
      case _ => throw new Exception(s"Unable to parse line \n$line")
    }
  }

  val lines = data.getLines().map(Monkey.parse).toArray

  def part1(): Unit = {
    val done = scala.collection.mutable.Map.empty[String, Symbol]
    val pend = scala.collection.mutable.Map.empty[String, Monkey]
    pend ++= lines
    while (pend.nonEmpty) {
      val eval = pend.flatMap{case (name, e) => e.evaluate(done).map{v => (name, v) }}
      pend --= eval.keys
      done ++= eval
    }
    println(s"Part1: ${done("root")}")
  }
  part1()

  def part2(): Unit = {
    val done = scala.collection.mutable.Map.empty[String, Symbol]
    val pend = scala.collection.mutable.Map.empty[String, Monkey]
    pend ++= lines
    pend -= "humn"
    done("humn") = Symbol.x
    val root = pend("root").asInstanceOf[Exp]
    while (pend.nonEmpty) {
      val eval = pend.flatMap{case (name, e) => e.evaluate(done).map{v => (name, v) }}
      pend --= eval.keys
      done ++= eval
    }
    val lhs = done(root.lhs)
    val rhs = done(root.rhs)
    val ans = Symbol.solve(lhs - rhs).map(_.toLong).head
    println(s"Part2: ${done(root.lhs)} = ${done(root.rhs)}")
    println(s"       $ans")
    done.clear()
    pend.clear()
    pend ++= lines
    pend("humn") = Const(ans)
    pend("root") = Exp(root.lhs, root.rhs, '=')
    while (pend.nonEmpty) {
      val eval = pend.flatMap { case (name, e) => e.evaluate(done).map { v => (name, v) } }
      pend --= eval.keys
      done ++= eval
    }
    println(s"Part2: Test ${done(root.lhs)} == ${done(root.rhs)}: ${done("root")}")
  }
  part2()
}
