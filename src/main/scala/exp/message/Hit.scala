package exp.message

import common.immutable.Cube
import exp.actor.entity.Entity

class Hit(from: Entity, val vol: Cube[Long], val strength: Int) extends Message(from) {

}
