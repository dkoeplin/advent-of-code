package Year2022

object Day19 extends App {
  case class Resources(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geodes: Int = 0)

  case class Blueprint(
    ore: Resources,
    clay: Resources,
    obsidian: Resources,
    geode: Resources
  )

  object Blueprint {
    private val regex = "Blueprint [0-9]+: Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9])+ obsidian.".r
    def parse(line: String): Blueprint = regex.findFirstMatchIn(line).map { m =>
      Blueprint(
        ore = Resources(ore = m.group(1).toInt),
        clay = Resources(ore = m.group(2).toInt),
        obsidian = Resources(ore = m.group(3).toInt, clay = m.group(4).toInt),
        geode = Resources(ore = m.group(5).toInt, obsidian = m.group(6).toInt)
      )
    }.getOrElse {
      throw new Exception(s"Unable to parse line:\n$line")
    }
  }

  val file = scala.io.Source.fromFile("data/2022/19")
  val blueprints = file.getLines().map(Blueprint.parse).toArray
}
