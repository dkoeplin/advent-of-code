package Year2023

import common.immutable.Pos.Idx
import common.immutable.{Box, Constructable, Matrix}
import common.parse

object Day16 extends common.AoC(16, 2023) {
  def splitOrReflect(c: Char, dir: Idx): Iterator[Idx] = (c, dir) match {
    case ('.', _) => Iterator(dir)
    case ('\\', Idx.D2.R) => Iterator(Idx.D2.D)
    case ('\\', Idx.D2.L) => Iterator(Idx.D2.U)
    case ('\\', Idx.D2.U) => Iterator(Idx.D2.L)
    case ('\\', Idx.D2.D) => Iterator(Idx.D2.R)
    case ('/', Idx.D2.R) => Iterator(Idx.D2.U)
    case ('/', Idx.D2.L) => Iterator(Idx.D2.D)
    case ('/', Idx.D2.U) => Iterator(Idx.D2.R)
    case ('/', Idx.D2.D) => Iterator(Idx.D2.L)
    case ('-', Idx.D2.R | Idx.D2.L) => Iterator(dir)
    case ('-', Idx.D2.U | Idx.D2.D) => Iterator(Idx.D2.L, Idx.D2.R)
    case ('|', Idx.D2.R | Idx.D2.L) => Iterator(Idx.D2.U, Idx.D2.D)
    case ('|', Idx.D2.U | Idx.D2.D) => Iterator(dir)
  }
  case class Beam(pos: Idx, dir: Idx)
  case class State(beams: Set[Beam], visited: Set[Beam]) {
    def this(beam: Beam) = this(Set(beam), Set.empty)
  }
  case class Contraption(vol: Box[Int], data: Array[Char]) extends Matrix(vol, data) {
    def move(beam: Beam): Iterator[Beam]
      = get(beam.pos).iterator.flatMap{c => splitOrReflect(c, beam.dir) }
                              .map{d => Beam(beam.pos + d, d) }
                              .filter{b => get(b.pos).nonEmpty }

    def energized(start: Beam): Set[Idx] = LazyList.iterate(new State(start)) { prev =>
      State(beams = prev.beams.iterator.flatMap(move).filterNot(prev.visited.contains).toSet,
            prev.visited ++ prev.beams)
    }.dropWhile(_.beams.nonEmpty).head.visited.map(_.pos)

    def beams(start: Idx): Iterator[Beam] = {
      (if (start.r == 0) Iterator(Idx.D2.D) else Iterator.empty) ++
        (if (start.r == H - 1) Iterator(Idx.D2.U) else Iterator.empty) ++
        (if (start.c == 0) Iterator(Idx.D2.R) else Iterator.empty) ++
        (if (start.c == W - 1) Iterator(Idx.D2.L) else Iterator.empty)
    }.map{d => Beam(start, d) }

    def edges: Iterator[Beam] =
      hIterator.flatMap{i => Iterator(Idx(i, 0), Idx(i, W - 1)) }.flatMap(beams) ++
      wIterator.flatMap{j => Iterator(Idx(0, j), Idx(H - 1, j)) }.flatMap(beams)

    def best: Int = edges.map{start => energized(start).size }.max
  }
  implicit object Contraption extends Constructable[Char,Contraption]

  val contraption = parse.chars(data).to[Contraption]
  println(s"Part 1: ${contraption.energized(Beam(Idx(0,0), Idx.D2.R)).size}")
  println(s"Part 2: ${contraption.best}")
}
