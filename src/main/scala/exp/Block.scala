package exp

import common.immutable.{Pos, Volume}

import scala.swing.{Color, Graphics2D}

case class Block(var volume: Volume[Double], color: Color, var velocity: Pos[Double]) extends Entity {
  def tick(world: World): Unit = {
    val x = if (velocity.x >= 0) volume.max.x else volume.min.x
    val y = if (velocity.y >= 0) volume.max.y else volume.min.y
    val minX = if (velocity.x == 0) 1 else Math.signum(velocity.x)*1
    val minY = if (velocity.y == 0) 1 else Math.signum(velocity.y)*1
    val boundX = volume.alter(0, x + Math.signum(velocity.x)*0.001, x + minX + velocity.x) // x travel range
    val boundY = volume.alter(1, y + Math.signum(velocity.y)*0.001, y + minY + velocity.y) // y travel range
    var collideX = false
    var collideY = false
    if (velocity.x != 0) world.overlapExcept(boundX, Some(this)).foreach{e =>
      val ex = if (velocity.x >= 0) e.volume.min.x else e.volume.max.x
      velocity = Pos(0, velocity.y)
      volume += Pos(ex - x - minX, 0)
      collideX = true
    }
     world.overlapExcept(boundY, Some(this)) match {
      case Some(e) =>
        // println(s"  $volume on ${e.volume}")
        val ey = if (velocity.y >= 0) e.volume.min.y else e.volume.max.y
        velocity = Pos(velocity.x, 0)
        volume += Pos(0, (ey - y) - minY)
        collideY = true
      case None =>
        velocity = Pos(velocity.x, Math.min(Exp.terminalVelocity, velocity.y + Exp.gravity))
      case Some(ground) =>
        // Do nothing for now
        collideY = true
    }
    volume += velocity
    // velocity = Pos(if (collideX) 0 else velocity.x, if (collideY) 0 else velocity.y)
  }

  override def draw(g: Graphics2D): Unit = {
    val v = volume.roundInt
    g.setColor(color)
    g.fill3DRect(v.min.x, v.min.y, v.shape.x, v.shape.y, true)
  }
}
