package Year2022

object Day10 extends common.AoC(10, 2022) {
  val WIDTH = 40
  def str(x: Int, cycles: Int): Int = if ((cycles - 20) % 40 == 0) x * cycles else 0
  case class State(during: Int = 1, next: Int = 1, cycles: Int = 0, sum: Int = 0) {
    val pos: Int = (cycles - 1) % WIDTH
    val pixel: Char = if (pos >= (during - 1) && pos <= (during + 1)) '#' else '.'
    def noop(): State = State(next, next, cycles + 1, sum + str(next, cycles + 1))
    def addx(v: Int): State = State(next, next + v, cycles + 1, sum + str(next, cycles + 1))
  }

  val scan = data.getLines().scanLeft(List(State())){(state, line) =>
    if (line.startsWith("addx")) {
      val c0 = state.last.noop()
      val c1 = c0.addx(line.drop(5).toInt)
      List(c0, c1)
    } else if (line == "noop") {
      List(state.last.noop())
    } else throw new Exception("Unknown instruction: " + line)
  }.toArray.flatten
  println(s"Part1: ${scan.last.sum}")
  println(scan.dropWhile(_.cycles < 1).map(_.pixel).grouped(WIDTH).map(_.mkString("")).mkString("\n"))
}
