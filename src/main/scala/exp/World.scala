package exp

import common.immutable.{Pos, Volume}

import java.util.TimerTask
import scala.collection.mutable
import scala.swing._
import scala.swing.event._

class World(val parent: Frame) extends Component {
  focusable = true
  listenTo(mouse.clicks)
  listenTo(mouse.moves)
  listenTo(keys)

  private implicit val entityOrdering: Ordering[Entity]
    = (a: Entity, b: Entity) => implicitly[Ordering[Double]].compare(a.volume.min.y, b.volume.min.y)

  def windowSize: Pos[Int] = {
    val wsize = parent.peer.getBounds
    Pos(wsize.width, wsize.height)
  }

  class Entities(private var entities: mutable.PriorityQueue[Entity] = mutable.PriorityQueue.empty[Entity]) {
    private var id: Int = 0
    def iterator: Iterator[Entity] = entities.iterator
    def +=(x: Entity): Unit = {
      id += 1
      x.id = id
      entities += x
    }
    def -=(e: Entity): Unit = { entities = entities.filterNot(_ == e) }
  }
  private val entities = new Entities

  def find(x: Pos[Int]): Option[Entity] = {
    val p = x.toDoubles
    entities.iterator.find(_.volume.contains(p))
  }
  def overlapExcept(x: Volume[Double], target: Option[Entity]): Option[Entity] = {
    entities.iterator.find{e => !target.contains(e) && e.volume.overlaps(x) }
  }

  object TargetedEntity {
    def unapply(x: Point): Option[Entity] = find(x)
  }

  def tick(): Unit = entities.iterator.foreach(_.tick(this))
  val clock: java.util.TimerTask = new TimerTask { override def run(): Unit = tick() }

  def load(): Unit = {
    entities += Ground(this)
  }

  override def paint(g: Graphics2D): Unit = {
    //val bounds = entities.iterator.map(_.volume).reduce(_ union _)
    // val wsize = windowSize
    // val scaling: Pos[Double] = wsize / bounds.shape
    entities.iterator.foreach(_.draw(g))
    pending.foreach(_.draw(g))
  }

  var pending: Option[Block] = None
  reactions += {
    case KeyPressed(_, key, _, _) =>
      println(s"Key pressed: $key")
      if (key == Key.Escape) System.exit(0)
      if (key == Key.L)
        entities.iterator.foreach{e => println(e.volume) }

    case MouseClicked(src, TargetedEntity(e), keys, n, _) => // do nothing
    case MousePressed(src, TargetedEntity(e), keys, n, _) => // do nothing

    case MouseReleased(src, TargetedEntity(e: Block), keys, n, _) if pending.isEmpty =>
      entities -= e

    case MousePressed(src, pt, keys, n, _) =>
      val x = (pt: Pos[Int]).toDoubles
      val color = new Color(util.Random.nextInt(255), util.Random.nextInt(255), util.Random.nextInt(255))
      pending = Some(Block(Volume(x, x), color, Pos(0, 0)))
    case MouseDragged(src, pt, keys) if pending.nonEmpty =>
      val v = Volume(pending.get.volume.l, (pt: Pos[Int]).toDoubles)
      val c = overlapExcept(v, None)
      if (c.isEmpty)
        pending.get.volume = v
      else
        println(s"Hit $c")
    case MouseReleased(src, pt, keys, n, _) if pending.nonEmpty =>
      // pending.get.volume = Volume(pending.get.volume.l, (pt: Pos[Int]).toDoubles)
      entities += pending.get
      pending = None
    case MouseExited(_, _, _) =>
      pending = None
    case MouseMoved(_,_,_) =>
      requestFocus()
  }
}
