
case class Position(horizontal: Int = 0, vertical: Int = 0) {
  def up(dist: Int): Position = Position(horizontal, vertical - dist)
  def down(dist: Int): Position = Position(horizontal, vertical + dist)
  def forward(dist: Int): Position = Position(horizontal + dist, vertical)
}

case class Position2(horizontal: Int = 0, vertical: Int = 0, aim: Int = 0) {
  def up(dist: Int): Position2 = Position2(horizontal, vertical, aim - dist)
  def down(dist: Int): Position2 = Position2(horizontal, vertical, aim + dist)
  def forward(dist: Int): Position2 = Position2(horizontal + dist, vertical + aim*dist, aim)
}

object Day02 extends App {
  val file = scala.io.Source.fromFile("./data/2")
  val lines = file.getLines().toArray
  file.close()

  val part1 = lines.foldLeft(Position()){
    case (pos, cmd) if cmd.startsWith("forward") => pos.forward(cmd.drop(8).toInt)
    case (pos, cmd) if cmd.startsWith("down") => pos.down(cmd.drop(5).toInt)
    case (pos, cmd) if cmd.startsWith("up") => pos.up(cmd.drop(3).toInt)
  }
  println(s"Part 1: ${part1.horizontal * part1.vertical}")

  val position = lines.foldLeft(Position2()){
    case (pos, cmd) if cmd.startsWith("forward") => pos.forward(cmd.drop(8).toInt)
    case (pos, cmd) if cmd.startsWith("down") => pos.down(cmd.drop(5).toInt)
    case (pos, cmd) if cmd.startsWith("up") => pos.up(cmd.drop(3).toInt)
  }
  println(s"Part 2: ${position.horizontal * position.vertical}")
}
