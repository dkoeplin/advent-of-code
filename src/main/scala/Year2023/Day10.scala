package Year2023

import common.immutable.Pos.Idx
import common.immutable.{Constructable, Matrix, Volume}
import common.math.{Even, Odd}
import common.parse

object Day10 extends common.AoC(10, 2023) {
  private val pipes = Map(
    '|' -> Seq(Idx.D2.U, Idx.D2.D),
    '-' -> Seq(Idx.D2.L, Idx.D2.R),
    'L' -> Seq(Idx.D2.U, Idx.D2.R),
    'J' -> Seq(Idx.D2.U, Idx.D2.L),
    '7' -> Seq(Idx.D2.D, Idx.D2.L),
    'F' -> Seq(Idx.D2.D, Idx.D2.R),
    'S' -> Nil, // This one has to be discovered per diagram
    '.' -> Nil
  )

  case class Nodes(inside: Set[Idx] = Set.empty, outside: Set[Idx] = Set.empty) {
    def ++(rhs: Nodes): Nodes = Nodes(inside ++ rhs.inside, outside ++ rhs.outside)
    def contains(x: Idx): Boolean = inside.contains(x) || outside.contains(x)
    def and(nodes: Set[Idx], isInside: Boolean): Nodes = Nodes(
      inside = inside ++ (if (isInside) nodes else Set.empty),
      outside = outside ++ (if (isInside) Set.empty else nodes))
  }

  case class Diagram(vol: Volume[Int], data: Array[Char]) extends Matrix[Char](vol, data) {
    val start: Idx = indexWhere(_ == 'S').get
    private val types: Map[Char, Seq[Idx]] = {
      val next = Idx.D2.nondiag.filter{d => pipeNeighbors(d + start, pipes).contains(start) }.toSeq
      pipes ++ Seq('S' -> next)
    }

    // Returns an iterator over connected pipe neighbors.
    private def pipeNeighbors(pos: Idx, types: Map[Char, Seq[Idx]] = types): Iterator[Idx] = get(pos).map{ c =>
      types(c).iterator.map(_ + pos).filter(has)
    }.getOrElse(Iterator.empty)

    // Returns an iterator over horizontal/vertical neighbors that are ground (.) tiles.
    private def emptyNeighbors(pos: Idx): Iterator[Idx] =
      Idx.D2.nondiag.iterator.map(_ + pos).filter{p => get(p).contains('.') }

    // True if any horizontal or vertical neighbor of this tile is outside the diagram
    private def isOutside(pos: Idx): Boolean = Idx.D2.nondiag.exists{d => !has(pos + d) }

    // Returns all tiles that are connected to the start tile based on the 'next' function.
    private def connected(start: Idx)(next: Idx => Iterator[Idx]): Set[Idx] = {
      case class State(frontier: List[Idx], nodes: Set[Idx])
      LazyList.iterate(State(List(start), Set(start))){case State(frontier, nodes) =>
        val unvisited = next(frontier.head).filterNot{nodes.contains}.toSeq
        State(frontier.tail ++ unvisited, nodes ++ unvisited)
      }.dropWhile(_.frontier.nonEmpty).head.nodes
    }

    private lazy val cycle: Set[Idx] = connected(start){ n => pipeNeighbors(n)}

    lazy val part1: Int = (cycle.size + 1) / 2

    // Returns an expanded diagram twice the current size, making sure to keep pipes connected.
    // Keeps only the pipes that belong to the starting cycle.
    // This exposes "paths" between pipes to more easily detect whether a tile is truly inside the cycle.
    def expand(): Diagram = Diagram(vol * 2 , indices.map{idx =>
        val original = idx / 2
        val inCycle = cycle.contains(original)
        lazy val horz = inCycle && types(apply(original)).contains(Idx.D2.R)
        lazy val vert = inCycle && types(apply(original)).contains(Idx.D2.D)
        idx match {
          case Idx(Even(), Even()) if inCycle => apply(original)
          case Idx(Odd(), Even()) if vert => '|'
          case Idx(Even(), Odd()) if horz => '-'
          case _ => '.'
        }
      }.toArray)

    // Classifies tiles as either "inside" or "outside" the main cycle.
    private def classifyTiles(): Nodes = {
      case class State(frontier: List[Idx], nodes: Nodes = Nodes())
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

    lazy val part2: Int = expand().classifyTiles().inside.count{case Idx(Even(),Even()) => true; case _ => false}
  }
  implicit object Diagram extends Constructable[Char,Diagram]

  val diagram = parse.chars(data).to[Diagram]
  println(s"Part 1: ${diagram.part1}")
  println(s"Part 2: ${diagram.part2}")
}
