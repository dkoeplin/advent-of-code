package exp.message

import common.immutable.Box
import exp.actor.entity.Entity

class Hit(from: Entity, val vol: Box[Long], val strength: Int) extends Message(from) {
  override def toString: String = s"Hit for $strength at $vol by $from"
}
