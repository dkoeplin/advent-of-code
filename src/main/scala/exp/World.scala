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
  def world: World = this
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
    def awake: Iterator[Entity] = awakeEntities.iterator

    def +=(e: Entity): Unit = {
      allEntities += e
      awakeEntities += e
    }
    def -=(e: Entity): Unit = {
      allEntities -= e
      awakeEntities -= e
    }
    def ++=(e: IterableOnce[Entity]): Unit = {
      allEntities ++= e
      awakeEntities ++= e
    }

    // TODO: Limit this somehow
    def nearby(x: Cube[Double]): Iterator[Entity] = allEntities.iterator

    def find(x: Pos[Int]): Option[Entity] = {
      val p = x.toDoubles
      entities.nearby(Cube(p,p)).find{e => e.contains(p) && e.alive }
    }
    def findVolume(x: Pos[Int]): Option[entity.Part] = {
      val p = x.toDoubles
      entities.nearby(Cube(p,p)).iterator.find(_.contains(p)).flatMap(_.at(p))
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
    def get(entity: Entity): Option[Message] = queues.get(entity.id).flatMap(_.dequeueOption).map{
      case (msg, next) if next.nonEmpty => queues(entity.id) = next; msg
      case (msg, next) => queues.remove(entity.id); msg
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
    /*val wsize = Cube(Pos(0,0), parent.windowSize)
    wsize.iteratorBy(20).iterator.foreach{p =>
      entities.findVolume(p).foreach{part =>
        g.setColor(part.material.color)
        g.fillRect(p.x, p.y, 20, 20)
      }
    }*/

    Tool.current.draw(g)
    children.foreach(_.paint(g))
  }

  var children: Set[Component] = Set.empty

  abstract class Tool {
    def draw(g: Graphics2D): Unit
    def exit(pt: Pos[Int]): Unit = {}
    def down(pt: Pos[Int]): Unit
    def move(pt: Pos[Int]): Unit
    def drag(pt: Pos[Int]): Unit
    def up(pt: Pos[Int]): Unit
  }
  object Tool {
    private val list: Array[Tool] = Array(Remover, Breaker, Creator)
    private var index: Int = 0
    def next(): Unit = { index = (index + 1) % list.length }
    def current: Tool = list(index)
  }
  case object Creator extends Tool {
    var pending: Option[Block] = None
    def draw(g: Graphics2D): Unit = pending.foreach(_.draw(g))
    def down(pt: Pos[Int]): Unit = {
      val x = pt.toDoubles
      pending = Some(new Block(entities.nextId, world, Cube(x, x), material.Test.random))
    }
    def move(pt: Pos[Int]): Unit = {}
    def drag(pt: Pos[Int]): Unit = if (pending.nonEmpty) {
      val p = pending.get.iterator.next()
      val v = Cube(p.volume.l, (pt: Pos[Int]).toDoubles)
      val c = entities.overlappingExcept(v.toDoubles, None)
      if (c.isEmpty)
        pending = Some(new Block(pending.get.id, world, v, p.material))
    }
    def up(pt: Pos[Int]): Unit = if (pending.nonEmpty) {
      entities += pending.get
      pending = None
    }
  }
  case object Remover extends Tool {
    var hovered: Option[Entity] = None
    def draw(g: Graphics2D): Unit = hovered.foreach{e =>
      e.highlight(g, brighter = true)
      e.above.foreach { a => a.highlight(g, brighter = false) }
    }
    def down(pt: Pos[Int]): Unit = { }
    def move(pt: Pos[Int]): Unit = { hovered = world.entities.find(pt) }
    def drag(pt: Pos[Int]): Unit = { }
    def up(pt: Pos[Int]): Unit = if (hovered.nonEmpty) {
      hovered.get.kill()
      hovered = None
    }
    override def exit(pt: Pos[Int]): Unit = { hovered = None }
  }
  case object Breaker extends Tool {
    var vol: Cube[Int] = Cube(Pos(0,0), Pos(0,0))
    def draw(g: Graphics2D): Unit = {
      g.setColor(new Color(255, 0, 0, 32))
      g.fillRect(vol.min.x, vol.min.y, vol.shape.x, vol.shape.y)
    }
    def down(g: Pos[Int]): Unit = { }
    def move(g: Pos[Int]): Unit = { vol = Cube(g - 20, g + 20) }
    def drag(g: Pos[Int]): Unit = { }
    def up(pt: Pos[Int]): Unit = {
      val remove = vol.toDoubles
      world.entities.overlappingExcept(vol.toDoubles, None).foreach(_.remove(remove))
    }
  }

  var prevKey: Option[Key.Value] = None

  reactions += {
    case KeyPressed(_, Key.Escape, _, _) => System.exit(0)
    case KeyPressed(_, Key.L, _, _) => entities.all.foreach{e => println(e) }
    case KeyPressed(_, Key.T, _, _) => Tool.next()

    // case KeyReleased(_, Key.Tab, _, _) if children.exists(_.isInstanceOf[window.Selection]) =>

    case KeyPressed(_, Key.B, _, _) =>
      val debug = children.find(_.isInstanceOf[window.Debug])
      children = if (debug.isEmpty) children + new window.Debug(this) else children - debug.get

    case KeyReleased(_, key, mods, _) =>
      println("Other key pressed ")
      prevKey = Some(key)
    // Remover.active = !Remover.active
    // Breaker.active = !Breaker.active

    case MousePressed(src, pt, keys, n, _)  => Tool.current.down(pt)
    case MouseReleased(src, pt, keys, n, _) => Tool.current.up(pt)
    case MouseDragged(src, pt, keys) => Tool.current.drag(pt)
    case MouseMoved(_, pt, _)  => Tool.current.move(pt)
    case MouseExited(_, pt, _) => Tool.current.exit(pt)
  }
}
object World {
  def isVertical(dim: Int): Boolean = { dim == 1 }
  def isVertical[T:Numeric](x: Pos[T]): Boolean = isVertical(dim(x))
  def dim[T:Numeric](delta: Pos[T]): Int = delta.iterator.zipWithIndex.find(_._1 != implicitly[Numeric[T]].zero).get._2
}
