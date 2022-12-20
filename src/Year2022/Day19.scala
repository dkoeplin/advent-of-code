package Year2022

object Day19 extends App {
  class Resources(items: Iterable[(Resource.Type, Int)]) {
    val map: Map[Resource.Type, Int] = Map.empty ++ items
    def apply(t: Resource.Type): Int = map.getOrElse(t, 0)
    def +(rhs: Resources): Resources = new Resources(Resource.all.map{k => k -> (this(k) + rhs(k)) })
    def -(rhs: Resources): Resources = new Resources(Resource.all.map{k => k -> (this(k) - rhs(k)) })
    def >=(rhs: Resources): Boolean = Resource.all.forall{k => this(k) >= rhs(k) }
    def >(rhs: Resources): Boolean = Resource.all.forall{k => this(k) > rhs(k) }
    def ==(rhs: Resources): Boolean = Resource.all.forall{k => this(k) == rhs(k) }
    def !=(rhs: Resources): Boolean = !(this == rhs)

    override def toString: String = items.filter(_._2 != 0).map{case (k, n) => s"$k=${n}"}.mkString("{", ", ", "}")
  }
  object Resource extends Enumeration {
    type Type = Value
    val Ore = Value("Ore")
    val Clay = Value("Clay")
    val Obsidian = Value("Obsidian")
    val Geode = Value("Geode")
    val all = Seq(Ore, Clay, Obsidian, Geode)
  }
  object Resources {
    def apply(items: (Resource.Type, Int)*): Resources = new Resources(items)
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

  case class Action(a: Option[Resource.Type]) {
    override def toString: String = a.map(_.toString.substring(0, 1)).getOrElse("W")
  }

  def accel(v: Resources, blueprint: Map[Resource.Type, Resources], verbose: Boolean = false): Resources
    = new Resources(blueprint.map{case (t, cost) => t -> Math.min(1, Resource.all.map{r => v(r) / cost(r) }.min)})

  case class State(x: Resources, v: Resources, maxT: Int, actions: Array[Action]) {
    def nothing(): State = State(x + v, v, maxT, actions :+ Action(None))
    def build(tp: Resource.Type, cost: Resources): State = State(x - cost + v, v + Resources(tp -> 1), maxT, actions :+ Action(Some(tp)))
  }
  object State {
    def initial(maxT: Int): State = State(Resources.empty, Resources(Resource.Ore -> 1), maxT, Array.empty)
  }
  def playback(actions: Seq[Action], blueprint: Map[Resource.Type, Resources]): Int = {
    (1 to actions.length).foldLeft(State.initial(actions.length)){(state, t) =>
      println(s"== Minute $t ==")
      println(s"  x=${state.x} v=${state.v}")
      actions(t - 1).a match {
        case Some(t) =>
          println(s"  Build $t robot for ${blueprint(t)}")
          state.build(t, blueprint(t))
        case None =>
          println(s"  Wait")
          state.nothing()
      }
    }
  }.x(Resource.Geode)

  def maxGeodes(blueprint: Map[Resource.Type, Resources], maxT: Int): State = {
    (1 to maxT).foldLeft(Seq(State.initial(maxT))){(states, t) => {
      val next = states.flatMap{state =>
        blueprint.filter{b => state.x >= b._2 }.map{b => state.build(b._1, b._2) }.toSeq :+ state.nothing()
      }
      implicit val ordering: Ordering[Resources] = new Ordering[Resources]{
        def compare(a: Resources, b: Resources): Int = if (a > b) 1 else if (a == b) 0 else -1
      }
      val pruned = next.groupBy(_.v).view.mapValues{states => states.maxBy(_.x) }.values
      println(s"T=$t (next: ${next.length}, pruned: ${pruned.size})")
      pruned.toSeq
    }}.maxBy(_.x(Resource.Geode))
  }

  val file = scala.io.Source.fromFile("example/2022/19")
  val blueprints = file.getLines().map(Blueprint.parse).toArray
  val maxT = 24

  //val initial = State(x = Resources.empty, v = Resources(Resource.Ore -> 1), t=1, maxT=24)
  //initial.expected(blueprints(0), verbose=true)
  //System.exit(0)

  blueprints.zipWithIndex.foreach{case (blueprint, i) =>
    val end = maxGeodes(blueprint, maxT)
    println(s"$i: ${end.actions.mkString("")}: ${end.x(Resource.Geode)}")
  }
}
