package Year2023

object Day08 extends Year2023(8) {
  private val LR = Map('L' -> 0, 'R' -> 1)
  object Node {
    private val lr = "(...) = \\((...), (...)\\)".r
    def parse(x: String): Option[(String, Seq[String])] =
      lr.findFirstMatchIn(x).map{m => (m.group(1), Seq(m.group(2), m.group(3))) }
  }

  def lcm(x: BigInt, y: BigInt): Long = (x * y / x.gcd(y)).toLong

  // --- setupA --- | -- periodA -- |
  // - setupB - | ----- periodB ----- |
  // M*periodA + setupA = N*periodB + setupB
  // M*periodA - N*periodB = (setupB - setupA)
  // 0 = (setupB - setupA + N*periodB) % periodA
  case class Cycle(delay: Long, period: Long) {
    private def getDelayWith(rhs: Cycle): Option[Long] = {
      if (rhs.delay == delay) Some(0) else {
        // There's probably a closed form solution for this but IDK
        var i: Long = 0
        while ((rhs.delay - delay + i * rhs.period) % period != 0) {
          i += 1
          if (i > 1e10) return None // I dunno give up I guess?
        }
        Some(rhs.period * i + rhs.delay)
      }
    }
    def sync(rhs: Cycle): Option[Cycle] = {
      val delay2 = getDelayWith(rhs)
      val period2 = lcm(period, rhs.period)
      delay2.map{d => Cycle(d, period2)}
    }
    def timeToFirst: Long = if (delay == 0) period else delay
  }
  case class Matches(singles: Set[Long], cycles: List[Cycle]) {
    def sync(rhs: Matches): Matches = Matches(
      singles = singles.intersect(rhs.singles),
      cycles = cycles.iterator.flatMap{c0 => rhs.cycles.flatMap{c1 => c0.sync(c1) }}.toList
    )
    def timeToFirst: Long = (singles.iterator ++ cycles.map(_.timeToFirst)).min
  }

  def findCycle(start: String)(target: String => Boolean): Matches = {
    val visited = collection.mutable.Map.empty[(String, Int), Long]
    var i: Int = 0
    var step: Long = 0
    var x: String = start
    while (!visited.contains((x, i))) {
      visited((x, i)) = step
      x = map(x)(dirs(i))
      i = (i + 1) % dirs.length
      step = step + 1
    }
    val setup = visited((x, i))
    val period = step - setup
    val (before, during) = visited.iterator.collect{case ((node,_),step) if target(node) => (node,step) }
                                  .partition(_._2 < setup)
    Matches (
      singles = before.map(_._2).toSet,
      cycles = during.map(_._2).map{step =>
        if (step == period) Cycle(0, period) else Cycle(step, period)
      }.toList
    )
  }

  val lines = data.getLines()
  val dirs: Array[Int] = lines.next().map(LR.apply).toArray
  val map = lines.flatMap(Node.parse).toMap

  // Part 2 doesn't have an "AAA" :(
  // val part1 = stepsUntil(List("AAA")){_ == "ZZZ"}
  // println(s"Part 1: ${part1}")

  val part2 = map.keysIterator.filter(_.endsWith("A"))
                 .map{k => findCycle(k)(_.endsWith("Z")) }
                 .reduce{_ sync _}.timeToFirst
  println(s"Part 2: $part2")
}
