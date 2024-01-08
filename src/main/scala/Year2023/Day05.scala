package Year2023

import common.immutable.Range

object Day05 extends common.AoC(5, 2023) {
  case class RangeMapping(src: Range, dst: Range) {
    def apply(i: Long): Long = dst.min - src.min + i
    def apply(r: Range): Range = if (!src.overlaps(r)) Range.empty else {
      val i = src.intersect(r)
      Range.at(dst.min + (i.min - src.min), i.length)
    }
    def get(r: Range): Option[Range] = if (src.overlaps(r)) Some(apply(r)) else None
  }
  object RangeMapping {
    def unapply(x: String): Option[RangeMapping] = x.split(' ') match {
      case Array(dst, src, len) => Some(RangeMapping(Range.at(src.toLong, len.toLong), Range.at(dst.toLong, len.toLong)))
      case _ => None
    }
  }
  case class RangeMap(name: String, entries: List[RangeMapping]) {
    def +:(lhs: RangeMapping): RangeMap = RangeMap(name, lhs +: entries)
    def apply(i: Long): Long = entries.find(_.src.contains(i)).map(_.apply(i)).getOrElse(i)
    def apply(r: Range): List[Range] = {
      val mapping = entries.foldLeft((List(r), Nil: List[Range])){case ((unmapped, mapped), entry) =>
        unmapped.foldLeft((Nil: List[Range], mapped)) {case ((unmapped, mapped), range) =>
          (unmapped ++ (range - entry.src), mapped ++ entry.get(range))
        }
      }
      mapping._1 ++ mapping._2
    }
  }
  object RangeMap {
    def empty(name: String): RangeMap = RangeMap(name, Nil)
    def unapply(line: String): Option[RangeMap] = line.split('-') match {
      case Array(x, "to", y) => Some(RangeMap.empty(y.dropRight(" map:".length)))
      case _ => None
    }
  }

  val lines = data.getLines().toArray
  val seedIDs = lines.head.drop("seeds: ".length).split(' ').iterator.filter(_.nonEmpty).map(_.toLong).toArray
  val maps = lines.tail.foldLeft(Nil: List[RangeMap]){
    case (maps, RangeMap(map)) => map +: maps
    case (maps, RangeMapping(range)) => (range +: maps.head) +: maps.tail
    case (maps, _) => maps
  }.reverse

  val part1 = seedIDs.map{seed => maps.foldLeft(seed){(i, map) => map(i) }}.min
  println(s"Part 1: $part1")

  val part2 = seedIDs.grouped(2).map{case Array(start, len) => Range.at(start, len) }.map{seedRange =>
    maps.foldLeft(List(seedRange)){(ranges, map) => ranges.flatMap(map.apply) }.map(_.min).min
  }.min
  println(s"Part 2: $part2")
}
