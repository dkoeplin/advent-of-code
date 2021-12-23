import scala.collection.mutable
import scala.language.implicitConversions

object Directions extends Enumeration {
  type Dir = Value
  def flip(dir: Dir): Dir = if (dir == L) R else L
  val L = Value("L")
  val R = Value("R")
}
import Directions._

case class Trace(path: List[Dir], x: Either[Int,Pair]) {
  def pair: Pair = x.right.get

  def left(n: Int = -1): Trace = x match {
    case Left(_) => this
    case Right(pair) => pair.left(path, n)
  }
  def right(n: Int = -1): Trace = x match {
    case Left(_) => this
    case Right(pair) => pair.right(path, n)
  }

  override def toString: String = x match {
    case Left(v) => s"$v ${path.mkString(", ")}"
    case Right(pair) => s"$pair ${path.mkString(", ")}"
  }
}

case class Pair(var a: Either[Int,Pair], var b: Either[Int,Pair]) {
  override def toString: String = {
    def str(x: Either[Int,Pair]): String = x match {
      case Left(i) => i.toString
      case Right(p) => p.toString
    }
    s"[${str(a)},${str(b)}]"
  }
  def isLeaf: Boolean = a.isLeft && b.isLeft

  def map(path: List[Dir], func: PartialFunction[Trace, Either[Int,Pair]]): Pair = {
    val copy: PartialFunction[Trace, Either[Int,Pair]] = {
      case Trace(_, Left(v)) => Left(v)
      case Trace(path, Right(pair)) => Right(pair.map(path, func))
    }
    val visit = func.orElse(copy)
    Pair(visit(Trace(L +: path, a)), visit(Trace(R +: path, b)))
  }
  def map(func: PartialFunction[Trace, Either[Int,Pair]]): Pair = map(Nil, func)

  def value(dir: Dir): Int = dir match {
    case L if a.isLeft => a.left.get
    case R if b.isLeft => b.left.get
  }
  def update(dir: Dir, v: Either[Int,Pair]): Unit = dir match {
    case L => a = v
    case R => b = v
  }
  def update(dir: Dir, v: Int): Unit = update(dir, Left(v))

  def trace(path: List[Dir]): Option[Trace] = path.lastOption match {
    case None => Some(Trace(Nil,Right(this)))
    case Some(L) => a.toOption.flatMap(_.trace(path.dropRight(1))).map{t => Trace(path, t.x) }
    case Some(R) => b.toOption.flatMap(_.trace(path.dropRight(1))).map{t => Trace(path, t.x) }
  }
  def left(path: List[Dir], n: Int = -1): Trace = if (n == 0) Trace(path, Right(this)) else a match {
    case Left(v) => Trace(L +: path, Left(v))
    case Right(pair) => pair.left(L +: path, n - 1)
  }
  def right(path: List[Dir], n: Int = -1): Trace = if (n == 0) Trace(path, Right(this)) else b match {
    case Left(v) => Trace(L +: path, Left(v))
    case Right(pair) => pair.right(R +: path, n - 1)
  }

  def findSplit(path: List[Dir] = Nil): Option[Trace] = {
    def find(x: Either[Int,Pair], path: List[Dir]): Option[Trace] = x match {
      case Left(v) if v > 9 => Some(Trace(path, Left(v)))
      case Left(_) => None
      case Right(pair) => pair.findSplit(path)
    }
    find(a, L +: path).orElse(find(b, R +: path))
  }
  def split(): Option[Pair] = findSplit().map{case Trace(path, Left(v)) =>
    val left = Math.floor(v / 2.0).toInt
    val right = Math.ceil(v / 2.0).toInt
    this.map{case Trace(p, _) if p == path => Right(Pair(Left(left), Left(right))) }
  }

  def findExplode(path: List[Dir] = Nil): Option[Trace] = {
    def find(x: Either[Int,Pair], path: List[Dir]): Option[Trace] = x match {
      case Right(pair) if path.size >= 4 && pair.isLeaf => Some(Trace(path, Right(pair)))
      case Right(pair) => pair.findExplode(path)
      case Left(_) => None
    }
    find(a, L +: path).orElse(find(b, R +: path))
  }
  def explode(verbose: Boolean = false): Option[Pair] = findExplode().map{case t @ Trace(path, Right(Pair(Left(a), Left(b)))) =>

    def dirOrElse(path2: List[Dir], default: Dir): Dir = {
      val common = path.reverseIterator.zip(path2.reverseIterator).takeWhile{case (a,b) => a == b}.size
      val flip = common == path2.size && path.size > common
      if (flip) Directions.flip(path(path.size - common - 1)) else default
    }

    val firstL = path.indexOf(L)
    val firstR = path.indexOf(R)
    val leftTop: Option[Trace] = if (firstR >= 0) trace(path.drop(firstR + 1)) else None
    val rightTop: Option[Trace] = if (firstL >= 0) trace(path.drop(firstL + 1)) else None
    val zero: Option[List[Dir]] = Some(path)
    val left: Option[List[Dir]] = leftTop.map(_.left(1).right()).map{case Trace(path, _) =>
      dirOrElse(path.drop(1), R) +: path.drop(1)
    }
    val right: Option[List[Dir]] = rightTop.map(_.right(1).left()).map{case Trace(path, _) =>
      dirOrElse(path.drop(1), L) +: path.drop(1)
    }

    this.map{
      case Trace(p, _) if zero.contains(p) => Left(0)
      case Trace(p, Left(v)) if left.contains(p) => Left(v + a)
      case Trace(p, Left(v)) if right.contains(p) => Left(v + b)
    }
  }

  def reduce(): Pair = {
    var current: Option[Pair] = None
    var next: Option[Pair] = Some(this)
    while (next.nonEmpty) {
      current = next
      next = current.flatMap(_.explode())
      if (next.isEmpty) next = current.flatMap(_.split())
    }
    current.get
  }

  def +(rhs: Pair): Pair = Pair(Right(this),Right(rhs)).reduce()

  def magnitude(): Long = {
    def partial(x: Either[Int,Pair]): Long = x match {
      case Left(x) => x.toLong
      case Right(pair) => pair.magnitude()
    }
    3L * partial(a) + 2 * partial(b)
  }
}

object Pair {
  def unapply(x: String): Option[Pair] = {
    val stack = mutable.Stack[(Pair,Dir)]()
    var i = 0
    while (i < x.length) {
      x.charAt(i) match {
        case '[' =>
          stack.push((new Pair(Left(0), Left(0)), L))
          i += 1
        case ',' =>
          val current = stack.pop()
          stack.push((current._1, R))
          i += 1
        case ']' =>
          val (pair, _) = stack.pop()
          i += 1
          if (stack.nonEmpty)
            stack.top._1.update(stack.top._2, Right(pair))
          else
            return Some(pair)
        case c =>
          val num = new StringBuilder()
          while (x.charAt(i).isDigit) {
            num += x.charAt(i)
            i += 1
          }
          stack.top._1.update(stack.top._2, Left(num.toString().toInt))
      }
    }
    None
  }
}

object Day18 extends App {
  var explodePassed = true
  var splitPassed = true
  var magnitudePassed = true
  def testExplode(in: String, out: String, verbose: Boolean = false): Unit = in match {case Pair(input) =>
    val output = input.explode(verbose)
    val outStr = output.map(_.toString).getOrElse("???")
    val pass = outStr == out
    println(s"$input => $outStr [${if (pass) "PASS" else s"FAIL: expected $out"}]")
    explodePassed &= pass
  }
  def testSplit(in: String, out: String): Unit = in match {case Pair(input) =>
    val output = input.split()
    val outStr = output.map(_.toString).getOrElse("???")
    val pass = outStr == out
    println(s"$input => $outStr [${if (pass) "PASS" else s"FAIL: expected $out"}]")
    splitPassed &= pass
  }
  def testMagnitude(in: String, expect: Long): Unit = in match {case Pair(input) =>
    val magnitude = input.magnitude()
    val pass = magnitude == expect
    println(s"$input => $magnitude [${if (pass) "PASS" else s"FAIL: expected $expect"}]")
    magnitudePassed &= pass
  }

  testExplode("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
  testExplode("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
  testExplode("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
  testExplode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
  testExplode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  testExplode("[[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]", "[[[[5,0],[[9,7],[9,6]]],[[4,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]")
  testExplode("[[[[5,9],[16,0]],[[10,[1,2]],[[1,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]", "[[[[5,9],[16,0]],[[11,0],[[3,4],2]]],[[[5,[2,8]],4],[5,[[9,9],0]]]]")
  assert(explodePassed, "Failed explode tests")

  testSplit("[10,0]", "[[5,5],0]")
  testSplit("[0,11]", "[0,[5,6]]")
  assert(splitPassed, "Failed split tests")

  testMagnitude("[[1,2],[[3,4],5]]", 143L)
  testMagnitude("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384L)
  testMagnitude("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445L)
  testMagnitude("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791L)
  testMagnitude("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137L)
  testMagnitude("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488L)
  assert(magnitudePassed, "Failed magnitude tests")

  val file = scala.io.Source.fromFile("./data/18")
  val nums = file.getLines().map{case Pair(pair) => pair}.toArray
  val part1 = nums.reduceLeft{_+_}
  println(s"  Part 1 (Sum): $part1")
  println(s"  Part 1 (Magnitude): ${part1.magnitude()}")

  val part2 = nums.zipWithIndex.flatMap{case (a,i) =>
    nums.zipWithIndex.flatMap{case (b,j) if i != j => Some(a + b) case _ => None }.map{x => (x, x.magnitude()) }
  }.maxBy(_._2)

  println(s"  Part 2 (Max): ${part2._1}")
  println(s"  Part 2 (Magnitude): ${part2._2}")
}
