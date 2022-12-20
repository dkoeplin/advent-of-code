package Year2022

object Day19 extends App {
  class Resources(items: Iterable[(Resource.Type, Double)]) {
    val map: Map[Resource.Type, Double] = Map.empty ++ items
    def apply(t: Resource.Type): Double = map.getOrElse(t, 0)
    def +(rhs: Resources): Resources = new Resources((map.keys ++ rhs.map.keys).map{k => k -> (this(k) + rhs(k)) })
    def -(rhs: Resources): Resources = new Resources((map.keys ++ rhs.map.keys).map{k => k -> (this(k) - rhs(k)) })
    def >=(rhs: Resources): Boolean = rhs.map.forall{case (k,n) => this(k) >= n }

    override def toString: String = items.filter(_._2 != 0).map{case (k, n) => s"$k=${n}"}.mkString("{", ", ", "}")
  }
  object Resource extends Enumeration {
    type Type = Value
    val Ore = Value("Ore")
    val Clay = Value("Clay")
    val Obsidian = Value("Obsidian")
    val Geode = Value("Geode")
  }
  object Resources {
    def apply(items: (Resource.Type, Double)*): Resources = new Resources(items)
    def empty: Resources = new Resources(Nil)
  }

  object Blueprint {
    private val regex = "Blueprint [0-9]+: Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.".r
    def parse(line: String): Map[Resource.Type, Resources] = regex.findFirstMatchIn(line).map{m =>
      Map(Resource.Ore -> Resources(Resource.Ore -> m.group(1).toInt),
          Resource.Clay -> Resources(Resource.Ore -> m.group(2).toInt),
          Resource.Obsidian -> Resources(Resource.Ore -> m.group(3).toInt, Resource.Clay -> m.group(4).toInt),
          Resource.Geode -> Resources(Resource.Ore -> m.group(5).toInt, Resource.Obsidian -> m.group(6).toInt))
    }.getOrElse {
      throw new Exception(s"Unable to parse line:\n$line")
    }
  }

  def accel(v: Resources, blueprint: Map[Resource.Type, Resources], verbose: Boolean = false): Resources
    = new Resources(blueprint.map{case (t, cost) => t -> Math.min(1, cost.map.keys.map{r => v(r) / cost(r) }.min)}.toSeq)

  case class State(x: Resources, v: Resources, t: Int, maxT: Int) {
    def nothing(): State = State(x + v, v, t + 1, maxT)
    def build(m: (Resource.Type,Resources)): State = State(x - m._2 + v, v + Resources(m._1 -> 1), t+1, maxT)
    def expected(blueprint: Map[Resource.Type, Resources], verbose: Boolean = false): Double = (t to maxT).foldLeft(this){(state, t) =>
      val v = state.v
      val a = accel(v, blueprint)
      if (verbose) println(s"t=$t x=${state.x} v=${state.v} a=${a}")
      State(state.x + v, v + a, state.t + 1, state.maxT)
    }.x(Resource.Geode)
  }
  def maxGeodes(blueprint: Map[Resource.Type, Resources], maxT: Int): Double = {
    (1 to maxT).foldLeft(State(x=Resources.empty, v=Resources(Resource.Ore -> 1), t=1, maxT)){(state, t) =>
      println(s"== Minute $t ==")
      val expect = state.expected(blueprint)
      println(s"  x=${state.x} v=${state.v} e=$expect")
      val action = blueprint.filter{b => state.x >= b._2}.map{b =>
        val e = state.build(b).expected(blueprint)
        println(s"    Could build ${b._1} robot: E[g] = $e")
        (b, e)
      }.filter(_._2 > expect).maxByOption(_._2).map(_._1)
      action.map{x =>
        println(s"  Build ${x._1} robot for ${x._2}")
        state.build(x)
      }.getOrElse {
        println(s"  Wait")
        state.nothing()
      }
    }.x(Resource.Geode)
  }

  val file = scala.io.Source.fromFile("example/2022/19")
  val blueprints = file.getLines().map(Blueprint.parse).toArray
  val maxT = 24

  //val initial = State(x = Resources.empty, v = Resources(Resource.Ore -> 1), t=1, maxT=24)
  //initial.expected(blueprints(0), verbose=true)
  //System.exit(0)

  blueprints.zipWithIndex.foreach{case (blueprint, i) =>
    println(s"$i: ${maxGeodes(blueprint, maxT)}")
  }
}
