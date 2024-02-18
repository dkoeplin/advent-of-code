package exp.message

import exp.actor.Actor
import exp.actor.Actor.ID
import exp.actor.entity.Entity

import scala.collection.mutable

class Manager {
  var pending: Int = 0
  var maxPending: Int = 0
  private val queues: mutable.HashMap[Actor.ID, List[Message]] = mutable.HashMap.empty

  def clear(id: Actor.ID): Unit = queues.remove(id)

  def send(message: Message, dest: Entity): Unit = if (dest.isAlive) {
    pending += 1
    maxPending = Math.max(pending, maxPending)
    queues(dest.id) = message +: queues.getOrElse(dest.id, Nil)
    dest.wake()
  }

  def broadcast(message: Message, to: IterableOnce[Entity]): Unit
    = to.iterator.foreach{dest => send(message, dest) }

  /// Returns all pending messages for this entity and empties its mailbox
  def get(entity: Entity): List[Message] = {
    val queue = queues.getOrElse(entity.id, Nil)
    pending -= queue.size
    queues.remove(entity.id)
    queue
  }

  def iterator: Iterator[(ID, List[Message])] = queues.iterator
}
