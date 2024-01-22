package exp.message

import exp.actor.entity.Entity

class Move(from: Entity) extends Message(from) {
  override def toString: String = s"$from moved"
}
