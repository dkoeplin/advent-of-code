package Year2023

import common.immutable.Pos._
import common.immutable.{Constructable, Cube, Matrix}
import common.implicits.IteratorOps._
import common.parse

object Day14 extends common.AoC(14, 2023) {
  case class Platform(vol: Cube[Int], data: Array[Char]) extends Matrix(vol, data) {
    private def slide(empty: Int, rocks: Int, reverse: Boolean): String
      = if (empty + rocks == 0) "#" else if (!reverse) "." * empty + "O" * rocks else "O" * rocks + "." * empty

    private def slide(vec: Iterator[Char], reverse: Boolean): String = {
      vec.split('#').map(_.total(_ == 'O')).map{case Idx(empty, rocks) => slide(empty, rocks, reverse) }.mkString
    }

    def tilt(delta: Idx): Platform = {
      val reverse = delta.c < 0 || delta.r < 0
      val vectors = if (delta.r != 0) cols else rows
      val tilted = Platform(vol, vectors.flatMap{vec => slide(vec, reverse) }.toArray)
      if (delta.r != 0) tilted.t.to[Platform] else tilted
    }

    override def hashCode(): Int = rocks.hashCode()

    case class Cycle(setup: Int, period: Int, steps: Array[Int]) {
      def loadAt(i: Long): Int = steps(((i - setup - 1) % period).toInt)
    }

    def spin(cycles: Long): Int = {
      val spin = List(Idx.D2.U, Idx.D2.L, Idx.D2.D, Idx.D2.R)
      val n = cycles * spin.length
      val cache = collection.mutable.Map.empty[(Int, Int), Int]
      val path = collection.mutable.ArrayBuffer.empty[Int]
      var i: Int = 0
      var current: Platform = this
      while (i < n && cycle.isEmpty) {
        current = current.tilt(spin(i % 4))
        path += current.load
        cycle = cache.get((current.hashCode(), i % 4)).map{prev =>
          Cycle(prev, i - prev, path.view.drop(prev).dropRight(1).toArray)
        }
        cache((current.hashCode(), i % 4)) = i
        i += 1
      }
      cycle.map{_.loadAt(n)}.getOrElse(current.load)
    }

    lazy val rocks: Set[Idx] = indices.filter{pos => apply(pos) == 'O'}.toSet
    lazy val load: Int = rocks.iterator.map{pos => H - pos.r}.sum
    private var cycle: Option[Cycle] = None
  }
  implicit object Platform extends Constructable[Char,Platform]

  val platform = parse.chars(data).to[Platform]
  println(s"Part 1: ${platform.tilt(Idx.D2.U).load}")
  println(s"Part 2: ${platform.spin(1000000000L)}")
}
