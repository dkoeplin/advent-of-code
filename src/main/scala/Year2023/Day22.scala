package Year2023

import common.immutable.{Cube, Pos}
import common.viz.VolumeVisualizer

object Day22 extends common.AoC(22, 2023) {
  case class Brick(volume: Cube[Int], id: Int) {
    def surface: Cube[Int] = volume.dropDims(2)
    def height: Int = volume.max.z - volume.min.z + 1
    def atZ(z: Int): Brick = Brick(volume.moveTo(2, z), id)
  }
  object Brick {
    def parse(x: (String, Int)): Brick = {
      val Array(minStr, maxStr) = x._1.split('~')
      Brick(Cube(Pos.parse(minStr), Pos.parse(maxStr)), x._2)
    }
  }
  val bricks = data.getLines().zipWithIndex.map(Brick.parse).toArray.sortBy(_.volume.min.z)
  val area = bricks.foldLeft(Cube(Pos(0,0), Pos(0,0))){ (surface, brick) => surface union brick.surface}
  val ground = Brick(area.expand(0, 0), -1)

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
    val support = overlapping.map(_.volume.max.z).max
    val all = overlapping.filter(_.volume.max.z == support).map(_.id).toSet
    val map = all.map{name => name -> (prev.above.getOrElse(name,Set.empty) + brick.id)}
    State(brick.atZ(support + 1) +: prev.fallen, prev.under + (brick.id -> all), prev.above ++ map)
  }
  println(s"Part 1: ${dropped.part1}")
  println(s"Part 2: ${dropped.part2}")

  visualizers += VolumeVisualizer(dropped.fallen.map(_.volume))
  show()
}
