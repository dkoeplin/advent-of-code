package Year2023

import scala.util.matching.Regex

object Day12 extends Year2023(12) {
  case class Range(start: Int, end: Int) {
    def contains(i: Int): Boolean = i >= start && i < end
    def length: Int = end - start
  }
  case class Position(offset: Int, group: Int) {
    def regex(groups: Array[Int]): Regex = s"^([#?]{${groups(group)}})($$|[.?])".r
  }
  object Position {
    lazy val start: Position = Position(0, 0)
  }

  case class Edges(next: List[Range], depth: Int, total: Long)
  object Edges {
    def end: Edges = Edges(Nil, 1, 1)
    def empty: Edges = Edges(Nil, -1, 0)
    def invalid: Edges = Edges(Nil, -2, 0)
  }
  case class Cache(map: Map[Int, Map[Range, Edges]]) {
    def done(group: Int, range: Range): Boolean = get(group, range).exists(_.depth != -1)
    def waiting(group: Int, range: Range): Boolean = get(group, range).exists(_.depth == -1)
    def get(group: Int, range: Range): Option[Edges] = map.get(group).flatMap(_.get(range))
    def edges(row: Row, group: Int, start: Position): Edges
      = map.get(group + 1).map{nodes =>
          nodes.iterator.filter{case (range, edges) =>
            val depth: Int = row.groups.length - group
            range.start >= start.offset && edges.depth == depth - 1 &&
            !row.springs.iterator.slice(start.offset, range.start).contains('#')
          }.foldLeft(Edges(Nil, 0, 0)){case (list, (range, edge)) =>
            Edges(range +: list.next, math.max(list.depth, edge.depth + 1), list.total + edge.total)
          }}.getOrElse(Edges.empty)
    def and(group: Int, range: Range, edges: Edges): Cache
      = Cache(map + (group -> (map.getOrElse(group, Map.empty) + (range -> edges))))
  }
  object Cache {
    def empty: Cache = Cache(Map.empty)
  }

  trait FindResult
  case class Group(range: Range, pos: Position) extends FindResult
  case object NoMatch extends FindResult
  case object AlreadyVisited extends FindResult

  case class State(
                    frontier: List[Position],
                    cache: Cache,
                    posCache: Map[Position, FindResult],
                    roots: List[Range]
                  ) {
    def total: Long = roots.iterator.map{root => cache.get(0, root).map(_.total).getOrElse(0L) }.sum
    def paths(n: Int): List[List[Range]] = roots.flatMap{root =>
      (0 until n-1).foldLeft(List(List(root))){(paths, group) =>
        paths.flatMap{path => cache.get(group, path.last).get.next.map{next => path :+ next }}
      }
    }

    def find(row: Row, pos: Position): FindResult = posCache.getOrElse(pos, {
      pos.regex(row.groups).findFirstMatchIn(row.springs.drop(pos.offset)).map{m =>
        val index = pos.offset + m.start(1)
        Group(Range(index, index + row.groups(pos.group)), Position(pos.offset + m.end, pos.group + 1))
      }.filter { case Group(_, pos) =>
        pos.group < row.groups.length || !row.springs.iterator.drop(pos.offset).contains('#')
      }.getOrElse(NoMatch)
    })
  }
  object State {
    lazy val start: State = State(List(Position.start), Cache.empty, Map.empty, Nil)
  }

  case class Row(springs: String, groups: Array[Int]) {
    def skip(pos: Position): Option[Position]
      = if (pos.offset >= springs.length-1 || springs(pos.offset) == '#') None
        else Some(Position(pos.offset + 1, pos.group))

    def combinations: Long = LazyList.iterate(State.start){prev =>
      val current: Position = prev.frontier.head
      lazy val skipped = skip(current).toList
      prev.find(this, current) match {
        case AlreadyVisited =>
          State(prev.frontier.tail, prev.cache, prev.posCache, prev.roots)
        case NoMatch => // No contiguous group here
          State(skipped ++ prev.frontier.tail, prev.cache, prev.posCache + (current -> AlreadyVisited), prev.roots)
        case Group(range, _) if prev.cache.done(current.group, range) => // Already complete
          State(prev.frontier.tail, prev.cache, prev.posCache, prev.roots)
        case Group(range, next) if prev.cache.waiting(current.group, range) => // Already visited
          val edges = prev.cache.edges(this, current.group, next)
          val valid = edges.next.nonEmpty
          if (current.group == 0 && valid) {
            State(prev.frontier.tail, prev.cache.and(current.group, range, edges), prev.posCache, range +: prev.roots)
          } else if (valid) {
            State(prev.frontier.tail, prev.cache.and(current.group, range, edges), prev.posCache, prev.roots)
          } else {
            State(prev.frontier.tail, prev.cache.and(current.group, range, Edges.invalid), prev.posCache, prev.roots)
          }
        case group@Group(range, next) if next.group < groups.length => // Not yet visited, but valid
          State(next +: (skipped ++ prev.frontier),
                prev.cache.and(current.group, range, Edges.empty),
                prev.posCache + (current -> group),
                prev.roots)
        case group@Group(range, _) =>
          State(skipped ++ prev.frontier.tail,
                prev.cache.and(current.group, range, Edges.end),
                prev.posCache + (current -> group),
                prev.roots)
      }
    }.dropWhile(_.frontier.nonEmpty).head.total
  }
  def parseRow(line: String): Option[Row] = line.split(' ') match {
    case Array(springs, groups) => Some(Row(springs, groups.split(',').map(_.toInt)))
    case _ => None
  }

  val rows = data.getLines().flatMap(parseRow).toArray
  val part1 = rows.map{row => row.combinations }.sum
  println(s"Part 1: $part1")

  val part2 = rows.map{row =>
    val springs = (0 until 5).map { _ => row.springs }.mkString("?")
    val groups = (0 until 5).flatMap { _ => row.groups }.toArray
    Row(springs, groups).combinations
  }.sum
  println(s"Part 2: $part2")
}
