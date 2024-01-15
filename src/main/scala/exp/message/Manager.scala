package exp.message

import exp.entity.Entity

import scala.collection.immutable.Queue
import scala.collection.mutable

class Manager {
  private val queues: mutable.HashMap[Int,Queue[Message]] = mutable.HashMap.empty
  def broadcast(message: Message, to: IterableOnce[Entity]): Unit = to.iterator.foreach{dest =>
    queues(dest.id) = queues.getOrElse(dest.id, Queue.empty).enqueue(message)
    dest.wake()
  }
  def get(entity: Entity): Option[Message] = queues.get(entity.id).flatMap(_.dequeueOption).map{
    case (msg, next) if next.nonEmpty => queues(entity.id) = next; msg
    case (msg, next) => queues.remove(entity.id); msg
  }
}
