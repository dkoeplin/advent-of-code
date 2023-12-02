package Year2023

object Day02 extends Year2023(2) {
  object Color extends Enumeration {
    val Red, Green, Blue = Value
    private val map = Map[String, Value](("red", Red), ("green", Green), ("blue", Blue))
    def unapply(x: String): Option[Color.Value] = map.get(x)
  }
  type Color = Color.Value
  val R = Color.Red
  val G = Color.Green
  val B = Color.Blue
  val Colors = List(R, G, B)

  class Round(init: Iterable[(Color, Int)]) {
    def this(init: (Color, Int)*) = this(init)
    def apply(c: Color): Int = cubes.getOrElse(c, 0)
    def <=(rhs: Round): Boolean = Colors.forall{c => this(c) <= rhs(c) }
    def max(rhs: Round): Round = new Round(Colors.map{c => (c, math.max(this(c), rhs(c))) })
    def power(): Int = Colors.map{c => this(c) }.product

    private val cubes: Map[Color, Int] = init.toMap
  }
  object Round {
    def unapply(x: String): Option[Round] = Some(new Round(
      x.split(',').iterator
        .map(_.trim.split(' '))
        .collect { case Array(n, Color(c)) => (c, n.toInt) }
        .toArray.groupBy(_._1).view.mapValues(_.map(_._2).sum)
    ))
  }

  val games = data.getLines().map{game =>
    game.split(':').last.split(';').collect{case Round(r) => r}
  }.toArray

  val part1_target = new Round((R, 12), (G, 13), (B, 14))
  val part1 = games.iterator.zipWithIndex
                   .filter{game => game._1.forall(_ <= part1_target) }
                   .map(_._2 + 1).sum
  println(s"Part 1: $part1")

  val part2 = games.iterator.map{_.reduce(_ max _).power() }.sum
  println(s"Part 2: $part2")
}
