
case class Position(horizontal: Int = 0, vertical: Int = 0) {
  def up(dist: Int): Position = Position(horizontal, vertical + dist)
  def move(dist: Int): Position = Position(horizontal + dist, vertical)
}

object Main extends App {
  val file = scala.io.Source.fromFile("./data/2")

  val position = file.getLines().foldLeft(Position()){
    case (pos, cmd) if cmd.startsWith("forward") => pos.move(cmd.drop(8).toInt)
    case (pos, cmd) if cmd.startsWith("down") => pos.up(cmd.drop(5).toInt)
    case (pos, cmd) if cmd.startsWith("up") => pos.up(-cmd.drop(3).toInt)
  }
  file.close()
  println(position.horizontal * position.vertical)
}
