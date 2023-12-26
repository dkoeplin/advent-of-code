package Year2023

import common.Pos
import common.immutable.Matrix
import common.algorithm.Dijkstra

import scala.collection.immutable.VectorMap

object Day23 extends Year2023(23) {
  class Trails(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    val start: Pos = posIterator().find{p => apply(p) != '#' }.get
    val end: Pos = reverseIterator.find{p => apply(p) != '#' }.get
    def next(p: Pos): Iterator[Pos] = apply(p) match {
      case '.' => Pos.nondiag.iterator.map(_ + p).filter(has).filter{x => apply(x) != '#'}
      case '>' => Iterator(p + Pos.RIGHT)
      case '<' => Iterator(p + Pos.LEFT)
      case '^' => Iterator(p + Pos.UP)
      case 'v' => Iterator(p + Pos.DOWN)
    }
    type VectorSet = VectorMap[Pos, Boolean]
    object VectorSet {
      def apply(x: Pos): VectorSet = VectorMap[Pos,Boolean](x -> true)
    }
    /*def longest: Int = LazyList.iterate(List(VectorSet(start))){paths =>
      paths.flatMap{path => next(paths.) }
    }*/
  }

  class Solver(trails: Trails) extends Dijkstra[Pos](trails.start) {
    override def isEnd(v: Pos): Boolean = { v == trails.end }
    override def next(v: Pos): Iterator[Pos] = trails.next(v)
    override def weight(v: Pos): Int = -1
  }

  val trails = new Trails(example().getLines().map(_.toArray))
  val part1 = new Solver(trails).solve()

  val path = part1.pathTo(trails.end).toSet
  val annotated =
    Matrix(trails.posIterator().map{p => if (path.contains(p)) 'O' else trails(p) }.grouped(trails.cols))

  println(annotated)
  println(s"Part 1: ${part1.distanceTo(trails.end)}")
}
