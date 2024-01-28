package exp.actor.entity

import common.immutable.{Border, Box, Pos}
import exp.World
import exp.actor.Actor

class Liquid(id: Actor.ID, world: World, parts: Parts) extends Block(id, world, parts) {
  def this(id: Actor.ID, world: World, vol: Box[Long], mat: exp.material.Liquid) = this(id, world, new Parts(vol, mat))
  override val material: exp.material.Liquid = iterator.next().material.asInstanceOf[exp.material.Liquid] // FIXME
  override def falls: Boolean = material.falls

  case class Neighbor(v: Either[Box[Long], Liquid], dir: Pos[Long]) {
    def size: Long = v match {case Left(c) => c.size; case Right(l) => l.size }
    def spread(scale: Long): Unit = {
      v match {
        case Left(cube) if World.isVertical(dir) =>
          val scaling = Pos(dir.iterator.map{case 1 => scale; case _ => 1 })
          val vol = Box(cube.min, cube.max * scaling)
          world.actors += new Liquid(world.actors.nextId, world, vol, material)
        case Left(cube) =>
          val vol = if (dir.magnitude < 0) Box(Pos(cube.min.x, cube.min.y * scale), Pos(cube.max.x, cube.max.y * scale))
                    else Box(cube.min, cube.max * scale)
          world.actors += new Liquid(world.actors.nextId, world, vol, material)
        case Right(liq) =>

      }
    }
  }
  // private var neighbors: List[Neighbor] = Nil
  private def neighbors: List[Neighbor] = parts.borders.notUp.map{case Border(_, dir, border) =>
    val (x, y) = world.actors.getExcept(border, this).duplicate

    val empty = x.foldLeft(List(border)){(remain,e) => remain.flatMap(_.diff(e.iterator.map(_.box))) }
                 .filter(_.size > material.tension2)

    val liquid = y.collect{case l: Liquid if l.material == material => l }
    empty.map{x => Neighbor(Left(x), dir) } ++ liquid.map{x => Neighbor(Right(x), dir) }
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

  override def break(groups: Iterator[Parts]): Iterator[Entity] = groups.map{group =>
    new Liquid(world.actors.nextId, world, group)
  }
}
