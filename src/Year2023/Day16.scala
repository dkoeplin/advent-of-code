package Year2023

import common.Pos
import common.immutable.Matrix

object Day16 extends Year2023(16) {
  def splitOrReflect(c: Char, dir: Pos): Iterator[Pos] = (c, dir) match {
    case ('.', _) => Iterator(dir)
    case ('\\', Pos.RIGHT) => Iterator(Pos.DOWN)
    case ('\\', Pos.LEFT) => Iterator(Pos.UP)
    case ('\\', Pos.UP) => Iterator(Pos.LEFT)
    case ('\\', Pos.DOWN) => Iterator(Pos.RIGHT)
    case ('/', Pos.RIGHT) => Iterator(Pos.UP)
    case ('/', Pos.LEFT) => Iterator(Pos.DOWN)
    case ('/', Pos.UP) => Iterator(Pos.RIGHT)
    case ('/', Pos.DOWN) => Iterator(Pos.LEFT)
    case ('-', Pos.RIGHT | Pos.LEFT) => Iterator(dir)
    case ('-', Pos.UP | Pos.DOWN) => Iterator(Pos.LEFT, Pos.RIGHT)
    case ('|', Pos.RIGHT | Pos.LEFT) => Iterator(Pos.UP, Pos.DOWN)
    case ('|', Pos.UP | Pos.DOWN) => Iterator(dir)
  }
  case class Beam(pos: Pos, dir: Pos)
  case class State(beams: Set[Beam], visited: Set[Beam]) {
    def this(beam: Beam) = this(Set(beam), Set.empty)
  }
  case class Contraption(iter: Iterator[Iterable[Char]]) extends Matrix(iter) {
    def move(beam: Beam): Iterator[Beam]
      = get(beam.pos).iterator.flatMap{c => splitOrReflect(c, beam.dir) }
                              .map{d => Beam(beam.pos + d, d) }
                              .filter{b => get(b.pos).nonEmpty }

    def energized(start: Beam): Set[Pos] = LazyList.iterate(new State(start)) { prev =>
      State(beams = prev.beams.iterator.flatMap(move).filterNot(prev.visited.contains).toSet,
            prev.visited ++ prev.beams)
    }.dropWhile(_.beams.nonEmpty).head.visited.map(_.pos)

    def beams(start: Pos): Iterator[Beam] = {
      (if (start.row == 0) Iterator(Pos.DOWN) else Iterator.empty) ++
        (if (start.row == rows - 1) Iterator(Pos.UP) else Iterator.empty) ++
        (if (start.col == 0) Iterator(Pos.RIGHT) else Iterator.empty) ++
        (if (start.col == cols - 1) Iterator(Pos.LEFT) else Iterator.empty)
    }.map{d => Beam(start, d) }

    def edges: Iterator[Beam] =
      rowIndices.flatMap{i => Iterator(Pos(i, 0), Pos(i, cols - 1)) }.flatMap(beams) ++
      colIndices.flatMap{j => Iterator(Pos(0, j), Pos(rows - 1, j)) }.flatMap(beams)

    def best: Int = edges.map{start => energized(start).size }.max
  }

  val contraption = Contraption(data.getLines().map(_.toArray))
  println(s"Part 1: ${contraption.energized(Beam(Pos(0,0), Pos.RIGHT)).size}")
  println(s"Part 2: ${contraption.best}")
}
