package Year2023

import common.Pos
import common.immutable.Matrix

object Day10 extends Year2023(10) {
  private val pipes = Map(
    '|' -> Seq(Pos.UP, Pos.DOWN),
    '-' -> Seq(Pos.LEFT, Pos.RIGHT),
    'L' -> Seq(Pos.UP, Pos.RIGHT),
    'J' -> Seq(Pos.UP, Pos.LEFT),
    '7' -> Seq(Pos.DOWN, Pos.LEFT),
    'F' -> Seq(Pos.DOWN, Pos.RIGHT),
    'S' -> Nil, // This one has to be discovered per diagram
    '.' -> Nil
  )

  case class Nodes(inside: Set[Pos] = Set.empty, outside: Set[Pos] = Set.empty) {
    def ++(rhs: Nodes): Nodes = Nodes(inside ++ rhs.inside, outside ++ rhs.outside)
    def contains(x: Pos): Boolean = inside.contains(x) || outside.contains(x)
    def and(nodes: Set[Pos], isInside: Boolean): Nodes = Nodes(
      inside = inside ++ (if (isInside) nodes else Set.empty),
      outside = outside ++ (if (isInside) Set.empty else nodes))
  }

  case class Diagram(iter: Iterator[Iterable[Char]]) extends Matrix[Char](iter) {
    val start: Pos = find(_ == 'S').get
    private val types: Map[Char, Seq[Pos]] = {
      val next = Pos.nondiag.filter{d => pipeNeighbors(d + start, pipes).contains(start) }
      pipes ++ Seq('S' -> next)
    }

    // Returns an iterator over connected pipe neighbors.
    private def pipeNeighbors(pos: Pos, types: Map[Char, Seq[Pos]] = types): Iterator[Pos] = get(pos).map{c =>
      types(c).iterator.map(_ + pos).filter(this.contains)
    }.getOrElse(Iterator.empty)

    // Returns an iterator over horizontal/vertical neighbors that are ground (.) tiles.
    private def emptyNeighbors(pos: Pos): Iterator[Pos] =
      Pos.nondiag.iterator.map(_ + pos).filter{p => get(p).contains('.') }

    // True if any horizontal or vertical neighbor of this tile is outside the diagram
    private def isOutside(pos: Pos): Boolean = Pos.nondiag.exists{d => !contains(pos + d) }

    // Returns all tiles that are connected to the start tile based on the 'next' function.
    private def connected(start: Pos)(next: Pos => Iterator[Pos]): Set[Pos] = {
      case class State(frontier: List[Pos], nodes: Set[Pos])
      LazyList.iterate(State(List(start), Set(start))){case State(frontier, nodes) =>
        val unvisited = next(frontier.head).filterNot{nodes.contains}.toSeq
        State(frontier.tail ++ unvisited, nodes ++ unvisited)
      }.dropWhile(_.frontier.nonEmpty).head.nodes
    }

    private lazy val cycle: Set[Pos] = connected(start){n => pipeNeighbors(n)}

    lazy val part1: Int = (cycle.size + 1) / 2

    // Returns an expanded diagram twice the current size, making sure to keep pipes connected.
    // Keeps only the pipes that belong to the starting cycle.
    // This exposes "paths" between pipes to more easily detect whether a tile is truly inside the cycle.
    def expand(): Diagram = Diagram((0 until 2 * rows).iterator.map{i =>
      (0 until 2 * cols).map { j =>
        val original = Pos(i / 2, j / 2)
        val inCycle = cycle.contains(original)
        lazy val horz = inCycle && types(apply(original)).contains(Pos.RIGHT)
        lazy val vert = inCycle && types(apply(original)).contains(Pos.DOWN)
        if (i % 2 == 0 && j % 2 == 0 && inCycle) apply(original)
        else if (i % 2 == 1 && j % 2 == 0 && vert) '|'
        else if (i % 2 == 0 && j % 2 == 1 && horz) '-'
        else '.'
      }
    })

    // Classifies tiles as either "inside" or "outside" the main cycle.
    private def classifyTiles(): Nodes = {
      case class State(frontier: List[Pos], nodes: Nodes = Nodes())
      val frontier = cycle.flatMap(emptyNeighbors).toList
      LazyList.iterate(State(frontier)){state =>
        val current = state.frontier.head
        if (!state.nodes.contains(current)) {
          val component = connected(current)(emptyNeighbors)
          val inside = !component.exists(isOutside)
          State(state.frontier.tail, state.nodes.and(component, inside))
        } else {
          State(state.frontier.tail, state.nodes)
        }
      }.dropWhile(_.frontier.nonEmpty).head.nodes
    }

    lazy val part2: Int = {
      def isOriginal(x: Pos): Boolean = x.row % 2 == 0 && x.col % 2 == 0
      expand().classifyTiles().inside.count(isOriginal)
    }
  }

  val diagram = Diagram(data.getLines().map(_.toArray))
  println(s"Part 1: ${diagram.part1}")
  println(s"Part 2: ${diagram.part2}")
}
