package Year2023

import common.{Pos, Pos3, Surface}

object Day22 extends Year2023(22) {
  case class Brick(surface: Surface, minZ: Int, maxZ: Int, id: Int) {
    def height: Int = maxZ - minZ + 1
    def atZ(z: Int): Brick = Brick(surface, z, z + height - 1, id)
  }
  object Brick {
    def parse(x: (String, Int)): Brick = x._1.split('~') match {
      case Array(Pos3(min), Pos3(max)) => Brick(Surface(Pos(min.x, min.y), Pos(max.x, max.y)), min.z, max.z, x._2)
    }
  }
  val bricks = data.getLines().zipWithIndex.map(Brick.parse).toArray.sortBy(_.minZ)
  val area = bricks.foldLeft(Surface(Pos(0,0),Pos(0,0))){(surface, brick) => surface union brick.surface}
  val ground = Brick(area, minZ=0,maxZ=0,-1)

  case class State(fallen: List[Brick], under: Map[Int, Set[Int]], above: Map[Int,Set[Int]]) {
    // lol memoization is for chumps
    def drops(id: Int): Set[Int] = LazyList.iterate((Set(id),true)){case (fall, _) =>
      val next = fall.flatMap{id => above.getOrElse(id, Set.empty) }.filter{id => under(id).forall(fall.contains) }
      (fall ++ next, next.exists{id => !fall.contains(id)})
    }.dropWhile(_._2).head._1 - id

    def part1: Int = fallen.iterator.map(_.id).count{id => drops(id).isEmpty }
    def part2: Int = fallen.reverseIterator.drop(1).map(_.id).map(drops).map(_.size).sum
  }
  val dropped = bricks.foldLeft(State(List(ground),Map.empty, Map.empty)){(prev, brick) =>
    val overlapping = prev.fallen.filter(_.surface overlaps brick.surface)
    val support = overlapping.maxBy(_.maxZ).maxZ
    val all = overlapping.filter(_.maxZ == support).map(_.id).toSet
    val map = all.map{name => name -> (prev.above.getOrElse(name,Set.empty) + brick.id)}
    State(brick.atZ(support + 1) +: prev.fallen, prev.under + (brick.id -> all), prev.above ++ map)
  }
  println(s"Part 1: ${dropped.part1}")
  println(s"Part 2: ${dropped.part2}")
}
