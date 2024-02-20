package exp.actor.entity

import common.immutable.{Border, Box, Dir, Pos}
import exp.World
import exp.actor.Actor

class Liquid(id: Actor.ID, world: World, parts: Part.Tree) extends Block(id, world, parts) {
  def this(id: Actor.ID, world: World, vol: Box[Long], mat: exp.material.Liquid)
  = this(id, world, Part.Tree.one(new Part(vol, mat)))
  override val material: exp.material.Liquid = iterator.next().material.asInstanceOf[exp.material.Liquid] // FIXME
  override def falls: Boolean = material.falls

  case class Neighbor(v: Either[Box[Long], Liquid], dim: Int, dir: Dir) {
    def size: Long = v match {case Left(c) => c.size; case Right(l) => l.size }
    def spread(scale: Long): Unit = {
      v match {
        case Left(cube) if World.isVertical(dim) =>
          val scaling = Pos.unit[Long](parts.rank, dim) * (scale - 1) + 1
          val vol = Box(cube.min, cube.max * scaling)
          world.actors += new Liquid(world.actors.nextId, world, vol, material)
        case Left(cube) =>
          val vol = if (dir == Dir.Neg) Box(Pos(cube.min.x, cube.min.y * scale), Pos(cube.max.x, cube.max.y * scale))
                    else Box(cube.min, cube.max * scale)
          world.actors += new Liquid(world.actors.nextId, world, vol, material)
        case Right(liq) =>

      }
    }
  }
  // private var neighbors: List[Neighbor] = Nil
  private def neighbors: List[Neighbor] = parts.borders(World.NotUp).map { case Border(dim, dir, border) =>
    val (x, y) = world.actors.getExcept(border, this).duplicate

    val empty = x.foldLeft(List(border)){(remain,e) => remain.flatMap(_.diff(e.iterator.map(_.box))) }
                 .filter(_.size > material.tension2)

    val liquid = y.collect{case l: Liquid if l.material == material => l }
    empty.map { x => Neighbor(Left(x), dim, dir) } ++ liquid.map { x => Neighbor(Right(x), dim, dir) }
  }.reduce{_ ++ _}

  override def tick(): Unit = {
    super.tick()
    if (velocity.magnitude == 0) spread()
  }

  def spread(): Unit = neighbors match {
    case Nil => sleep()
    case n =>
      val outflow = n.iterator.map(_.size).sum
      val scale = Math.min(1, size / outflow)

  }

  override def break(groups: Iterable[Part.Tree]): Iterable[Entity]
  = groups.map { group => new Liquid(world.actors.nextId, world, group) }
}
