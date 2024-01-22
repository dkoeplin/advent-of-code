package exp.message

import exp.actor.entity.Entity

class Delete(from: Entity) extends Message(from) {
  override def toString: String = s"!DELETE!"
}
