package Year2023

import common.algorithm.LongestPath
import common.immutable.Pos.Idx
import common.immutable.{Constructable, Matrix, Volume}
import common.parse

import scala.collection.immutable.VectorMap

object Day23 extends common.AoC(23, 2023) {
  case class Trails(vol: Volume[Int], data: Array[Char]) extends Matrix(vol, data) {
    val start: Idx = indices.find{p => apply(p) != '#' }.get
    val end: Idx = indices.find{p => apply(p) != '#' }.get
    type VectorSet = VectorMap[Idx, Boolean]
    object VectorSet {
      def apply(x: Idx): VectorSet = VectorMap[Idx,Boolean](x -> true)
    }
    /*def longest: Int = LazyList.iterate(List(VectorSet(start))){paths =>
      paths.flatMap{path => next(paths.) }
    }*/
  }
  implicit object Trails extends Constructable[Char,Trails]

  class Part1(trails: Trails) extends LongestPath(trails.start, trails.end) {
    protected def delta(c: Char): Iterator[Idx] = c match {
      case '.' => Idx.D2.nondiag.iterator
      case '>' => Iterator(Idx.D2.R)
      case '<' => Iterator(Idx.D2.L)
      case '^' => Iterator(Idx.D2.U)
      case 'v' => Iterator(Idx.D2.D)
    }
    def next(p: Idx): Iterator[Idx] = delta(trails(p)).map(_ + p).filter(trails.has).filter{ x => trails(x) != '#'}
    override def weight(v: Idx): Int = 1
  }
  class Part2(trails: Trails) extends Part1(trails) {
    override protected def delta(c: Char): Iterator[Idx] = Idx.D2.nondiag.iterator
  }

  val trails = parse.chars(data).to[Trails]
  val part1: Set[Idx] = new Part1(trails).solve()

  //println(trails.annotated{p => if (part1.contains(p)) 'O' else trails(p) })
  println(s"Part 1: ${part1.size - 1} steps")

  val part2: Set[Idx] = new Part2(trails).solve()
  //println(trails.annotated{p => if (part2.contains(p)) 'O' else trails(p) })
  println(s"Part 2: ${part2.size - 1} steps") // 4826 too low
}
