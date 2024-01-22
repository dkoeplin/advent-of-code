package exp.message

import exp.actor.entity.Entity

class Removed(from: Entity) extends Message(from) {
  override def toString: String = s"$from was removed"
}
