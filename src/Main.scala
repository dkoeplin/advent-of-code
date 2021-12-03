
case class Position(horizontal: Int = 0, vertical: Int = 0) {
  def up(dist: Int): Position = Position(horizontal, vertical - dist)
  def down(dist: Int): Position = Position(horizontal, vertical + dist)
  def forward(dist: Int): Position = Position(horizontal + dist, vertical)
}

object Main extends App {
  val file = scala.io.Source.fromFile("./data/2")

  val position = file.getLines().foldLeft(Position()){
    case (pos, cmd) if cmd.startsWith("forward") => pos.forward(cmd.drop(8).toInt)
    case (pos, cmd) if cmd.startsWith("down") => pos.down(cmd.drop(5).toInt)
    case (pos, cmd) if cmd.startsWith("up") => pos.up(cmd.drop(3).toInt)
  }
  file.close()
  println(position.horizontal * position.vertical)
}
