package exp

import common.immutable.{Cube, Pos}
import exp.entity.Block

import java.util.TimerTask
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.language.implicitConversions
import scala.swing._
import scala.swing.event._

class World(val parent: Exp.Main) extends Component {
  focusable = true
  listenTo(mouse.clicks)
  listenTo(mouse.moves)
  listenTo(keys)

  object entities {
    private var id: Int = 0
    private val allEntities: mutable.LinkedHashSet[Entity] = mutable.LinkedHashSet.empty
    private val awakeEntities: mutable.LinkedHashSet[Entity] = mutable.LinkedHashSet.empty

    def nextId: Int = { id += 1; id }

    def all: Iterator[Entity] = allEntities.iterator
    def +=(e: Entity): Unit = {
      allEntities += e
      awakeEntities += e
    }
    def -=(e: Entity): Unit = {
      allEntities -= e
      awakeEntities -= e
    }
    def ++=(e: Iterable[Entity]): Unit = {
      allEntities ++= e
      awakeEntities ++= e
    }

    def awake: Iterator[Entity] = awakeEntities.iterator

    // TODO: Limit this somehow
    def nearby(x: Cube[Double]): Iterator[Entity] = allEntities.iterator

    def find(x: Pos[Int]): Option[Entity] = {
      val p = x.toDoubles
      entities.all.find{e => e.contains(p) && e.alive }
    }
    def overlappingExcept(x: Cube[Double], target: Option[Entity]): Iterator[Entity] = {
      entities.nearby(x).filter{e => !target.contains(e) && e.overlaps(x) && e.alive }
    }

    def notifyAwake(e: Entity): Unit = { awakeEntities += e }
    def notifySleep(e: Entity): Unit = { awakeEntities -= e }
    def notifyDead(e: Entity): Unit = { awakeEntities -= e; allEntities -= e }
  }
  object messages {
    private val queues: mutable.HashMap[Int,Queue[Message]] = mutable.HashMap.empty
    def broadcast(message: Message, to: IterableOnce[Entity]): Unit = to.iterator.foreach{dest =>
      queues(dest.id) = queues.getOrElse(dest.id, Queue.empty).enqueue(message)
      dest.wake()
    }
    def get(entity: Entity): Option[Message] = queues.get(entity.id).flatMap{_.dequeueOption}.map{
      case (msg, next) =>
        queues(entity.id) = next
        msg
    }
  }

  object TargetedEntity {
    def unapply(x: Point): Option[Entity] = entities.find(x)
  }

  val clock: java.util.TimerTask = new TimerTask { override def run(): Unit = tick() }
  def tick(): Unit = {
    // val start = System.nanoTime()
    entities.awake.foreach(_.tick())
    // val end = System.nanoTime()
    // println(s"Tick time: ${end - start}ns")
  }

  def load(): Unit = {
    val wsize = parent.windowSize
    val bottom = Cube(Pos(0, wsize.y - 100), wsize).toDoubles
    entities += new Block(entities.nextId, this, bottom, material.Bedrock)
  }

  override def paint(g: Graphics2D): Unit = {
    entities.all.foreach(_.draw(g))
    pending.foreach(_.draw(g))
    hovered.foreach{e =>
      e.highlight(g, brighter = true)
      e.above.foreach{a => a.highlight(g, brighter = false) }
    }
    children.foreach(_.paint(g))
  }

  var pending: Option[Block] = None
  var hovered: Option[Entity] = None
  var children: Set[Component] = Set.empty

  reactions += {
    case KeyPressed(_, Key.Escape, _, _) => System.exit(0)
    case KeyPressed(_, Key.L, _, _) => entities.all.foreach{e => println(e) }
    case KeyPressed(_, Key.Tab, _, _) =>

    case KeyReleased(_, Key.Tab, _, _) if children.exists(_.isInstanceOf[window.Selection]) =>

    case KeyPressed(_, Key.B, _, _) =>
      val debug = children.find(_.isInstanceOf[window.Debug])
      children = (if (debug.isEmpty) children + new window.Debug(this) else children - debug.get)

    case MouseClicked(src, TargetedEntity(e), keys, n, _) => // do nothing
    case MousePressed(src, TargetedEntity(e), keys, n, _) => // do nothing
    case MouseReleased(src, TargetedEntity(e: Block), keys, n, _) if pending.isEmpty =>
      e.kill()
      hovered = None

    case MousePressed(src, pt, keys, n, _) =>
      val x = (pt: Pos[Int]).toDoubles
      pending = Some(new Block(entities.nextId, this, Cube(x, x), material.Test.random))
    case MouseDragged(src, pt, keys) if pending.nonEmpty =>
      val v = Cube(pending.get.first.volume.l, (pt: Pos[Int]).toDoubles)
      val c = entities.overlappingExcept(v.toDoubles, None)
      if (c.isEmpty)
        pending = Some(new Block(pending.get.id, this, v, pending.get.first.material))
      //else
       // println(s"Hit $c")
    case MouseReleased(src, pt, keys, n, _) if pending.nonEmpty =>
      // pending.get.volume = Volume(pending.get.volume.l, (pt: Pos[Int]).toDoubles)
      entities += pending.get
      pending = None
    case MouseExited(_, _, _) =>
      pending = None
    case MouseMoved(_,TargetedEntity(e: Block),_) =>
      hovered = Some(e)
    case MouseMoved(_,_,_) =>
      hovered = None
  }
}
object World {
  def isVertical(dim: Int): Boolean = (dim == 1)
  def isVertical[T:Numeric](x: Pos[T]): Boolean = isVertical(dim(x))
  def dim[T:Numeric](delta: Pos[T]): Int = delta.iterator.zipWithIndex.find(_._1 != implicitly[Numeric[T]].zero).get._2
}
