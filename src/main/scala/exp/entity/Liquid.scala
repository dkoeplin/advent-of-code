package exp.entity

import common.immutable.{Cube, Pos}
import exp.World

class Liquid(id: Int, world: World, vol: Cube[Double], mat: exp.material.Liquid) extends Block(id, world, vol, mat) {
  override val material: exp.material.Liquid = mat
  override def falls: Boolean = mat.falls

  case class Neighbor(v: Either[Cube[Double], Liquid], dir: Pos[Double]) {
    def size: Double = v match {case Left(c) => c.size; case Right(l) => l.size }
    def spread(scale: Double): Unit = {
      v match {
        case Left(cube) if World.isVertical(dir) =>
          val scaling = Pos(dir.iterator.map{case 1 => scale; case _ => 1 })
          val vol = Cube(cube.min, cube.max * scaling)
          world.entities += new Liquid(world.entities.nextId, world, vol, mat)
        case Left(cube) =>
          val vol = if (dir.magnitude < 0) Cube(Pos(cube.min.x, cube.min.y * scale), Pos(cube.max.x, cube.max.y * scale))
                    else Cube(cube.min, cube.max * scale)
          world.entities += new Liquid(world.entities.nextId, world, vol, mat)
        case Right(liq) =>

      }
    }
  }
  // private var neighbors: List[Neighbor] = Nil
  private def neighbors: List[Neighbor] = bordersExceptUp(mat.tension).map{case (dir, border) =>
    val (x, y) = world.entities.overlappingExcept(border, Some(this)).duplicate
    val empty = x.foldLeft(List(border)){(remain,e) => remain.flatMap(_.diff(e.iterator.map(_.volume))) }.filter(_.size > mat.tension2)
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

  private def bordersExceptUp(width: Double): Iterator[(Pos[Double],Cube[Double])]
    = iterator.map(_.volume).flatMap(_.dirsAndBorders(width).filterNot(_._1.y < 0))
}
