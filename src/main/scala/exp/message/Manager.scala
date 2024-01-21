package exp.message

import exp.actor.Actor
import exp.actor.entity.Entity

import scala.collection.mutable

class Manager {
  private val queues: mutable.HashMap[Actor.ID, List[Message]] = mutable.HashMap.empty
  def send(message: Message, dest: Entity): Unit = {
    queues(dest.id) = message +: queues.getOrElse(dest.id, Nil)
    dest.wake()
  }
  def broadcast(message: Message, to: IterableOnce[Entity]): Unit = to.iterator.foreach{dest => send(message, dest) }
  /*def get(entity: Entity): Option[Message] = queues.get(entity.id).flatMap(_.dequeueOption).map{
    case (msg, next) if next.nonEmpty => queues(entity.id) = next; msg
    case (msg, _) => queues.remove(entity.id); msg
  }*/
  def get(entity: Entity): Iterator[Message] = {
    val queue = queues.getOrElse(entity.id, Nil)
    queues.remove(entity.id)
    queue.iterator
  }
}
