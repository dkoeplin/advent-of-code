package Year2023

import common.Pos
import common.immutable.Matrix
import common.algorithm.LongestPath

import scala.collection.immutable.VectorMap

object Day23 extends Year2023(23) {
  class Trails(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    val start: Pos = posIterator().find{p => apply(p) != '#' }.get
    val end: Pos = reverseIterator.find{p => apply(p) != '#' }.get
    type VectorSet = VectorMap[Pos, Boolean]
    object VectorSet {
      def apply(x: Pos): VectorSet = VectorMap[Pos,Boolean](x -> true)
    }
    /*def longest: Int = LazyList.iterate(List(VectorSet(start))){paths =>
      paths.flatMap{path => next(paths.) }
    }*/
  }

  class Part1(trails: Trails) extends LongestPath(trails.start, trails.end) {
    protected def delta(c: Char): Iterator[Pos] = c match {
      case '.' => Pos.nondiag.iterator
      case '>' => Iterator(Pos.RIGHT)
      case '<' => Iterator(Pos.LEFT)
      case '^' => Iterator(Pos.UP)
      case 'v' => Iterator(Pos.DOWN)
    }
    def next(p: Pos): Iterator[Pos] = delta(trails(p)).map(_ + p).filter(trails.has).filter{x => trails(x) != '#'}
    override def weight(v: Pos): Int = 1
  }
  class Part2(trails: Trails) extends Part1(trails) {
    override protected def delta(c: Char): Iterator[Pos] = Pos.nondiag.iterator
  }

  val trails = new Trails(data.getLines().map(_.toArray))
  val part1: Set[Pos] = new Part1(trails).solve()

  //println(trails.annotated{p => if (part1.contains(p)) 'O' else trails(p) })
  println(s"Part 1: ${part1.size - 1} steps")

  val part2: Set[Pos] = new Part2(trails).solve()
  //println(trails.annotated{p => if (part2.contains(p)) 'O' else trails(p) })
  println(s"Part 2: ${part2.size - 1} steps") // 4826 too low
}
