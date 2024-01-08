package Year2021

import common.immutable.Pos.Idx

object Day02 extends common.AoC(2, 2021) {
  case class Position2(pos: Idx = Idx(0,0), aim: Int = 0) {
    def move(cmd: Idx): Position2 = cmd match {
      case Idx(v, 0) => Position2(pos, aim + v)
      case Idx(0, v) => Position2(Idx(pos.x + aim*v, pos.y + v), aim)
      case _ => this
    }
  }

  object Command {
    def unapply(x: String): Option[Idx] = x.trim.split(' ') match {
      case Array("forward", n) => Some(Idx.D2.R * n.toInt)
      case Array("down", n) => Some(Idx.D2.D * n.toInt)
      case Array("up", n) => Some(Idx.D2.U * n.toInt)
      case _ => None
    }
  }

  val lines = data.getLines().flatMap(Command.unapply).toArray

  val part1 = lines.foldLeft(Idx(0,0)){(pos, cmd) => pos + cmd}
  println(s"Part 1: ${part1.product}")

  val part2 = lines.foldLeft(Position2()){(pos, cmd) => pos.move(cmd) }
  println(s"Part 2: ${part2.pos.product}")
}
