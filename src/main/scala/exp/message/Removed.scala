package exp.message

import exp.{Entity, Message}

class Removed(from: Entity) extends Message(from) {

}
