package Year2023

import common.Pos
import common.immutable.Matrix
import common.implicits.IteratorOps._

object Day14 extends Year2023(14) {
  case class Platform(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    def tilt(delta: Pos): Platform = {
      val reverse = delta.col < 0 || delta.row < 0
      val iterator = if (delta.row != 0) iterateOverCols() else iterateOverRows()
      val tilted = Platform(iterator.map{vec =>
        vec.split('#').map(_.total(_ == 'O')).map{case (rocks, empty) =>
          if (!reverse) "."*empty + "O"*rocks else "O"*rocks + "."*empty
        }.mkString("#").toArray
      })
      if (delta.row != 0) Platform(tilted.t) else tilted
    }

    override def hashCode(): Int = rocks.hashCode()

    case class Cycle(setup: Int, period: Int, steps: Array[Int]) {
      def loadAt(i: Long): Int = steps(((i - setup - 1) % period).toInt)
    }

    def spin(cycles: Long): Int = {
      val spin = List(Pos.UP, Pos.LEFT, Pos.DOWN, Pos.RIGHT)
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

    lazy val rocks: Set[Pos] = posIterator().filter{pos => apply(pos) == 'O'}.toSet
    lazy val load: Int = rocks.iterator.map{pos => rows - pos.row}.sum
    private var cycle: Option[Cycle] = None
  }

  val platform = Platform(data.getLines().map(_.toArray))
  println(s"Part 1: ${platform.tilt(Pos.UP).load}")
  println(s"Part 2: ${platform.spin(1000000000L)}")
}
