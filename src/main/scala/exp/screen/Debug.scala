package exp.screen

import common.immutable.Pos
import common.mutable.RTree
import exp.World
import exp.actor.entity.Entity
import exp.draw.Draw2D

class Debug(world: World) extends Screen {
  override def draw(g: Draw2D): Unit = {
    val over: Option[Entity] = world.view.flatMap{v => world.actors.find(v.focus) }
    Iterator(
      s"Alive: ${world.actors.entities.size}",
      s"Awake: ${world.actors.awake.size}",
      s"Parts: ${world.actors.entities.map(_.tree.size).sum}",
      s"Tool:  ${world.Tool.current}",
      s"Key:   ${world.prevKey.map(_.toString).getOrElse("N/A")}",
      s"Focus: ${world.view.map(_.focus.toString).getOrElse("N/A")}",
      s"Hover: ${over.map{e => s"$e: [P: ${e.tree.size}, D: ${e.tree.depth}, N: ${e.tree.nodes}]"}.getOrElse("N/A")}",
      s"Pends: ${world.Creator.pending.map{p => s"${p.tree.iterator.next().toString} @ ${p.loc}" }.getOrElse("N/A")}",
      s"Tick(curr): ${world.tickTime}ms",
      s"Tick(max): ${world.maxTickTime}ms",
      s"Msg(curr): ${world.messages.pending}",
      s"Msg(max): ${world.messages.maxPending}",
    ).zipWithIndex.foreach{case (text, i) => g.window.drawText(text, Pos(10, 20*(i + 1))) }

    world.actors.awake.foreach{
      case e: Entity => e.borders().foreach { border => g.fillRect(border.box, Draw2D.kRed) }
      case _ =>
    }

    def drawTree(tree: RTree[Long, _]): Unit = {
      tree.preorder{case (_, box, _) => box.edges(1).foreach{edge => g.fillRect(edge.box, Draw2D.kRed) }}
                   {case (_, box, _) => box.edges(1).foreach{edge => g.fillRect(edge.box, Draw2D.kRed) }}
    }
    over match {
      case Some(e) => e.borders().foreach{border => g.fillRect(border.box, Draw2D.kRed) }
        // drawTree(e.tree.valueTree)
      case None    => drawTree(world.actors.tree)
    }
    /*over.foreach{e =>
      e.iterator.foreach{part => part.box.edges(1).foreach{edge => g.fillRect(edge.volume, Draw2D.kBlack) }}
    }*/

    var i = 20
    world.messages.iterator.foreach{case (id, msgs) => msgs.foreach{msg =>
      g.window.drawText(s"$id: $msg", Pos(1000, i))
      i += 20
    }}
  }
}
