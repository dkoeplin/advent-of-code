package exp

import common.immutable.{Cube, Pos}
import exp.draw.Draw2D
import exp.entity.{Block, Entity}
import exp.screen.Screen

import java.util.TimerTask
import scala.language.implicitConversions
import scala.swing._
import scala.swing.event._

class World(val parent: Exp.Main) extends Component {
  def world: World = this
  focusable = true
  listenTo(mouse.clicks)
  listenTo(mouse.moves)
  listenTo(keys)

  val entities = new entity.Manager
  val messages = new message.Manager
  private var children: Set[Screen] = Set.empty
  var view: Option[draw.View2D] = None
  var prevKey: Option[Key.Value] = None

  def gravity: Long = 3       // px / tick^2: (10 m/s^2) * (1000 px/m) * (1 ms /1000 s)^2 * (50 ms / tick)^2
  def terminal: Long = 1500   // px / tick    (50 m/s) * (1 s / 1000 ms) * (30 ms / tick) * (1000 px/m)

  val clock: java.util.TimerTask = new TimerTask { override def run(): Unit = tick() }
  def tick(): Unit = {
    // val start = System.nanoTime()
    entities.awake.foreach(_.tick())
    // val end = System.nanoTime()
    // println(s"Tick time: ${end - start}ns")
  }

  def load(): Unit = {
    val windowSize = parent.windowSize
    val bottom = Cube(Pos(0, windowSize.y - 100), windowSize).toLongs
    entities += new Block(entities.nextId, this, bottom, material.Bedrock)
    view = Some(new draw.View2D(this))
  }

  override def paint(g: scala.swing.Graphics2D): Unit = {
    val painter = new draw.Draw2D(g, view.get)
    entities.all.foreach(_.draw(painter))
    /*val windowSize = Cube(Pos(0,0), parent.windowSize)
    windowSize.iteratorBy(20).iterator.foreach{p =>
      entities.findVolume(p).foreach{part =>
        g.setColor(part.material.color)
        g.fillRect(p.x, p.y, 20, 20)
      }
    }*/

    Tool.current.draw(painter)
    children.foreach(_.draw(painter))
    view.foreach{v =>
      g.setColor(new Color(25, 25, 25))
      g.drawLine(v.center.x - 10, v.center.y, v.center.x + 10, v.center.y)
      g.drawLine(v.center.x, v.center.y - 10, v.center.x, v.center.y + 10)
    }
  }

  abstract class Tool {
    def draw(g: Draw2D): Unit
    def exit(pt: Pos[Long]): Unit = {}
    def down(pt: Pos[Long]): Unit
    def move(pt: Pos[Long]): Unit
    def drag(pt: Pos[Long]): Unit
    def up(pt: Pos[Long]): Unit
  }
  object Tool {
    private val list: Array[Tool] = Array(Debug, Creator, Remover, Breaker)
    private var index: Int = 0
    def next(): Unit = { index = (index + 1) % list.length }
    def current: Tool = list(index)
  }
  case object Creator extends Tool {
    private var pending: Option[Block] = None
    def draw(g: Draw2D): Unit = pending.foreach(_.draw(g))
    def down(pt: Pos[Long]): Unit = {
      pending = Some(new Block(entities.nextId, world, Cube(pt, pt), material.Test.random))
    }
    def move(pt: Pos[Long]): Unit = {}
    def drag(pt: Pos[Long]): Unit = if (pending.nonEmpty) {
      val p = pending.get.iterator.next()
      val v = Cube(p.volume.l, pt)
      val c = entities.overlappingExcept(v.toLongs, None)
      if (c.isEmpty)
        pending = Some(new Block(pending.get.id, world, v, p.material))
    }
    def up(pt: Pos[Long]): Unit = if (pending.nonEmpty) {
      entities += pending.get
      pending = None
    }
  }
  case object Remover extends Tool {
    private var hovered: Option[Entity] = None
    def draw(g: Draw2D): Unit = hovered.foreach{ e =>
      e.highlight(g, brighter = true)
      // e.above.foreach{a => a.highlight(g, brighter = false) }
    }
    def down(pt: Pos[Long]): Unit = { }
    def move(pt: Pos[Long]): Unit = { hovered = world.entities.find(pt) }
    def drag(pt: Pos[Long]): Unit = { }
    def up(pt: Pos[Long]): Unit = if (hovered.nonEmpty) {
      hovered.get.kill()
      hovered = None
    }
    override def exit(pt: Pos[Long]): Unit = { hovered = None }
  }
  case object Breaker extends Tool {
    private val color = new Color(255, 0, 0, 32)
    def draw(g: Draw2D): Unit = g.window.fillRect(Cube(g.view.center - 20, g.view.center + 20), color)
    def down(pt: Pos[Long]): Unit = {
      val rm = Cube(pt - 20, pt + 20)
      world.entities.overlappingExcept(rm, None).foreach(_.remove(rm))
    }
    def move(pt: Pos[Long]): Unit = { }
    def drag(pt: Pos[Long]): Unit = {
      val rm = Cube(pt - 20, pt + 20)
      world.entities.overlappingExcept(rm, None).foreach(_.remove(rm))
    }
    def up(pt: Pos[Long]): Unit = { }
  }
  case object Debug extends Tool {
    var focus: Option[Entity] = None
    def draw(g: Draw2D): Unit = focus.foreach{e => e.highlight(g, brighter = true) }
    def down(g: Pos[Long]): Unit = { }
    def up(pt: Pos[Long]): Unit = { }
    def move(pt: Pos[Long]): Unit = { focus = world.entities.find(pt) }
    def drag(pt: Pos[Long]): Unit = { }
  }

  reactions += {
    case KeyPressed(_, Key.Escape, _, _) => System.exit(0)
    case KeyPressed(_, Key.L, _, _) => entities.alive.foreach{e => println(e) }
    case KeyPressed(_, Key.T, _, _) => Tool.next()
    case KeyPressed(_, Key.C, _, _) => view.foreach(_.recenter())

    // case KeyReleased(_, Key.Tab, _, _) if children.exists(_.isInstanceOf[window.Selection]) =>

    case KeyPressed(_, Key.B, _, _) =>
      val debug = children.find(_.isInstanceOf[screen.Debug])
      children = if (debug.isEmpty) children + new screen.Debug(this) else children - debug.get

    case KeyReleased(_, key, _, _) => prevKey = Some(key)

    case MousePressed(_, pt, _, _, _)  => view.map(_.move(pt).focus).foreach(Tool.current.down)
    case MouseReleased(_, pt, _, _, _) => view.map(_.move(pt).focus).foreach(Tool.current.up)
    // case MouseExited(_, pt, _) => Tool.current.exit(pt)
    case MouseDragged(_, pt, _) => view.map(_.move(pt).focus).foreach(Tool.current.drag)
    case MouseMoved(_, pt, _) => view.map(_.move(pt).focus).foreach(Tool.current.move)
  }
}
object World {
  def isVertical(dim: Int): Boolean = { dim == 1 }
  def isVertical[T:Numeric](x: Pos[T]): Boolean = isVertical(dim(x))
  def dim[T:Numeric](delta: Pos[T]): Int = delta.iterator.zipWithIndex.find(_._1 != implicitly[Numeric[T]].zero).get._2
}
