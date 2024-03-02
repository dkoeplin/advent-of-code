package exp

import common.immutable.{Border, Box, Dir, Pos}
import exp.actor.entity.{Block, Entity}
import exp.draw.Draw2D
import exp.screen.Screen

import scala.collection.mutable
import scala.language.implicitConversions
import scala.swing._
import scala.swing.event._

class World extends Component {
  def world: World = this
  focusable = true
  listenTo(mouse.clicks)
  listenTo(mouse.moves)
  listenTo(keys)

  val actors = new actor.Manager
  val messages = new message.Manager
  private var children: Set[Screen] = Set.empty
  var view: Option[draw.View2D] = None
  var prevKey: Option[Key.Value] = None
  val log = new java.io.FileWriter(new java.io.File("log/log"))

  def gravity: Long = 3       // px / tick^2: (10 m/s^2) * (1000 px/m) * (1 ms /1000 s)^2 * (50 ms / tick)^2
  def terminal: Long = 1500   // px / tick    (50 m/s) * (1 s / 1000 ms) * (30 ms / tick) * (1000 px/m)

  var tickTime: Long = 0
  var maxTickTime: Long = 0
  var prevActive: Int = 0
  var time: Long = 0
  def tick(): Unit = {
    val start = System.nanoTime()
    actors.awake.foreach(_.tick())
    val end = System.nanoTime()
    Tool.current.tick()
    tickTime = (end - start) / 1000000
    maxTickTime = Math.max(maxTickTime, tickTime)

    // if (prevActive > 0 && actors.awake.isEmpty) {
      // log.write("---------\n")
      // actors.entities.foreach{e => log.write(s"(${e.bbox} + ${e.loc}) named ${e.id}\n") }
      // actors.tree.dump()
      // log.flush()
    // }
    // prevActive = actors.awake.size
    time += 1

    actors.killed.foreach{id => messages.clear(id) }
  }

  def reset(windowSize: Pos[Long]): Unit = {
    val bottom = Box(Pos(0, windowSize.y - 100), windowSize)
    actors += new Block(actors.nextId, this, bottom, material.Bedrock)
  }

  def setView(v: draw.View2D): Unit = { view = Some(v) }

  override def paint(g: scala.swing.Graphics2D): Unit = view.foreach{v =>
    val painter = new draw.Draw2D(g, v)
    actors.visible(v.range).foreach(_.draw(painter))
    /*val windowSize = Cube(Pos(0,0), parent.windowSize)
    windowSize.iteratorBy(20).iterator.foreach{p =>
      entities.findVolume(p).foreach{part =>
        g.setColor(part.material.color)
        g.fillRect(p.x, p.y, 20, 20)
      }
    }*/

    Tool.current.draw(painter)
    children.foreach(_.draw(painter))

    // Crosshair
    g.setColor(new Color(25, 25, 25))
    g.drawLine(v.center.x - 10, v.center.y, v.center.x + 10, v.center.y)
    g.drawLine(v.center.x, v.center.y - 10, v.center.x, v.center.y + 10)
  }

  abstract class Tool {
    protected def kMaxCooldown: Int = 0
    protected var cooldown: Int = 0
    def tick(): Unit = if (cooldown > 0) { cooldown -= 1 } else { cooldown = 0 }
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
    var init: Option[Pos[Long]] = None
    var pending: Option[Block] = None
    def draw(g: Draw2D): Unit = pending.foreach(_.draw(g))
    def down(pt: Pos[Long]): Unit = if (actors.find(pt).isEmpty) {
      init = Some(pt)
      pending = Some(new Block(actors.nextId, world, Box(pt, pt), material.Test.random))
    }
    def move(pt: Pos[Long]): Unit = {}
    def drag(pt: Pos[Long]): Unit = if (pending.nonEmpty) {
      val box = Box(init.get, pt)
      val c = actors.get(box)
      if (c.isEmpty)
        pending = Some(new Block(pending.get.id, world, box, pending.get.material))
    }
    def up(pt: Pos[Long]): Unit = pending.foreach{p =>
      // actors.tree.dump{str => log.write(s"$str\n") }
      // log.write(s"$time: (${p.bbox} + ${p.loc}) named ${p.id}\n")
      // log.flush()

      actors += pending.get
      pending = None
      init = None

      // actors.tree.dump{str => log.write(s"$str\n") }
      // log.write("------------------\n")
      // log.flush()
    }
  }
  case object Remover extends Tool {
    private var hovered: Option[Entity] = None
    def draw(g: Draw2D): Unit = hovered.foreach{e => e.highlight(g, brighter = true) }
    def down(pt: Pos[Long]): Unit = { }
    def move(pt: Pos[Long]): Unit = { hovered = world.actors.find(pt) }
    def drag(pt: Pos[Long]): Unit = { }
    def up(pt: Pos[Long]): Unit = if (hovered.nonEmpty) {
      world.messages.send(new message.Delete(null), hovered.get)
      hovered = None
    }
    override def exit(pt: Pos[Long]): Unit = { hovered = None }
  }
  case object Breaker extends Tool {
    private val color = new Color(255, 0, 0, 32)
    private val pending: mutable.Map[Entity, Set[Box[Long]]] = mutable.Map.empty
    override def tick(): Unit = {
      pending.foreach{case (entity, rms) => rms.foreach{rm =>
        world.messages.send(new message.Hit(null, rm, strength=1), entity)
      }}
      pending.clear()
    }
    def draw(g: Draw2D): Unit = g.window.fillRect(Box(g.view.center - 20, g.view.center + 20), color)
    def down(pt: Pos[Long]): Unit = {
      val rm = Box(pt - 20, pt + 20)
      world.actors.get(rm).foreach{e => pending(e) = pending.getOrElse(e, Set.empty) + rm }
    }
    def move(pt: Pos[Long]): Unit = { }
    def drag(pt: Pos[Long]): Unit = if (cooldown == 0) {
      val rm = Box(pt - 20, pt + 20)
      world.actors.get(rm).foreach{e => pending(e) = pending.getOrElse(e, Set.empty) + rm }
    }
    def up(pt: Pos[Long]): Unit = { }

    override def toString: String = s"Break($cooldown)"
  }
  case object Debug extends Tool {
    var focus: Option[Entity] = None
    def draw(g: Draw2D): Unit = focus.foreach{e => e.highlight(g, brighter = true) }
    def down(g: Pos[Long]): Unit = { }
    def up(pt: Pos[Long]): Unit = { }
    def move(pt: Pos[Long]): Unit = { focus = world.actors.find(pt) }
    def drag(pt: Pos[Long]): Unit = { }
  }

  reactions += {
    case KeyPressed(_, Key.Escape, _, _) => System.exit(0)
    case KeyPressed(_, Key.T, _, _) => Tool.next()
    case KeyPressed(_, Key.C, _, _) => view.foreach(_.recenter())

    case KeyPressed(_, Key.B, _, _) =>
      val debug = children.find(_.isInstanceOf[screen.Debug])
      children = if (debug.isEmpty) children + new screen.Debug(this) else children - debug.get

    case KeyPressed(_, Key.D, _, _) =>
      world.view.flatMap{v => world.actors.find(v.focus) } match {
        case Some(e) =>
          val box = Box(e.bbox.min - 5, e.bbox.max + 5)
          println(s"Bounds: $box (${box.shape})")
          (box.min.y to box.max.y).foreach{y =>
            val line = (box.min.x to box.max.x).map{x =>
              val pos = Pos(x, y)
              if (e.tree.hasValueAt(pos)) 'X' else if (e.tree.hasBorderAt(pos)) 'O' else '.'
            }.mkString
            log.write(line)
            log.write('\n')
          } //.grouped(box.shape.x.toInt).foreach{line => log.write(line.mkString + "\n") }
        case None =>
          actors.entities.foreach{e => log.write(s"${e.id}: ${e.bbox} @ ${e.loc}") }
          actors.tree.dump{line => log.write(line); log.write("\n") }
      }
      log.flush()

    case KeyReleased(_, key, _, _) => prevKey = Some(key)

    case MousePressed(_, pt, _, _, _)  => view.map(_.move(pt).focus).foreach(Tool.current.down)
    case MouseReleased(_, pt, _, _, _) => view.map(_.move(pt).focus).foreach(Tool.current.up)
    case MouseDragged(_, pt, _) => view.map(_.move(pt).focus).foreach(Tool.current.drag)
    case MouseMoved(_, pt, _) => view.map(_.move(pt).focus).foreach(Tool.current.move)
  }
}
object World {
  val Up: Border[Long] => Boolean = { border => isVertical(border.dim) && border.dir == Dir.Neg }
  val NotUp: Border[Long] => Boolean = { border => !isVertical(border.dim) || border.dir == Dir.Pos }

  def isVertical(dim: Int): Boolean = { dim == 1 }
  def isVertical[T:Numeric](x: Pos[T]): Boolean = isVertical(dim(x))
  def dim[T:Numeric](delta: Pos[T]): Int = delta.iterator.zipWithIndex.find(_._1 != implicitly[Numeric[T]].zero).get._2
}
