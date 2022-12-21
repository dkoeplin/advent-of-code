package Year2022

object Day19 extends App {
  val times = scala.collection.mutable.Map.empty[String, Long]
  def time[T](name: String)(x: => T): T = {
    val start = System.currentTimeMillis()
    val result = x
    val end = System.currentTimeMillis()
    times(name) = times.getOrElse(name, 0L) + (end - start)
    result
  }

  object Resource extends Enumeration {
    type Type = Value
    val Ore: Type = Value("Ore")
    val Clay: Type = Value("Clay")
    val Obsidian: Type = Value("Obsidian")
    val Geode: Type = Value("Geode")
    val all: Seq[Type] = Seq(Ore, Clay, Obsidian, Geode)
  }

  case class Resources(o: Double = 0, c: Double = 0, b: Double = 0, g: Double = 0) {
    //val map: Map[Resource.Type, Double] = Map.empty ++ items
    def apply(t: Resource.Type): Double = t match {
      case Resource.Ore => o
      case Resource.Clay => c
      case Resource.Obsidian => b
      case Resource.Geode => g
    }
    def copyWith(t: Resource.Type, n: Double): Resources = t match {
      case Resource.Ore => Resources(n, c, b, g)
      case Resource.Clay => Resources(o, n, b, g)
      case Resource.Obsidian => Resources(o, c, n, g)
      case Resource.Geode => Resources(o, c, b, n)
    }
    def +(rhs: Resources): Resources = new Resources(o+rhs.o, c+rhs.c, b+rhs.b, g+rhs.g)
    def -(rhs: Resources): Resources = new Resources(o-rhs.o, c-rhs.c, b-rhs.b, g-rhs.g)
    def anyGreater(rhs: Resources): Boolean = o > rhs.o || c > rhs.c || b > rhs.b || g > rhs.g
    def allGreater(rhs: Resources): Boolean =  o >= rhs.o && c >= rhs.c && b >= rhs.b && g >= rhs.g
    override def toString: String = s"{o=$o, c=$c, b=$b, g=$g}"
  }

  object Resources {
    def apply(pair: (Resource.Type, Double)): Resources = Resources.empty.copyWith(pair._1, pair._2)
    def apply(xs: Iterable[(Resource.Type, Double)]): Resources = xs.foldLeft(Resources.empty){(r,e) =>
      r.copyWith(e._1, e._2)
    }
    def empty: Resources = Resources()
  }

  object Blueprint {
    private val regex = "Blueprint [0-9]+: Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.".r
    def parse(line: String): Map[Resource.Type, Resources] = regex.findFirstMatchIn(line).map{m =>
      Map(Resource.Ore -> Resources(o = m.group(1).toInt),
          Resource.Clay -> Resources(o = m.group(2).toInt),
          Resource.Obsidian -> Resources(o = m.group(3).toInt, c = m.group(4).toInt),
          Resource.Geode -> Resources(o = m.group(5).toInt, b = m.group(6).toInt))
    }.getOrElse {
      throw new Exception(s"Unable to parse line:\n$line")
    }
  }

  case class Action(a: Option[Resource.Type]) {
    override def toString: String = a.map(_.toString.substring(0, 1)).getOrElse("W")
  }
  object Action {
    val none: Action = Action(None)
    def apply(tp: Resource.Type): Action = Action(Some(tp))
    val all: Seq[Action] = Resource.all.map(Action.apply) :+ Action.none
  }

  def accel(v: Resources, blueprint: Map[Resource.Type, Resources]): Resources = time("accel") {
    Resources(blueprint.map{case (t, cost) => t -> Math.min(1, Resource.all.map{r => v(r) / cost(r) }.min)})
  }

  case class State(x: Resources, v: Resources, maxT: Int, actions: Array[Action]) {
    def nothing(): State = State(x + v, v, maxT, actions :+ Action(None))
    def build(tp: Resource.Type, cost: Resources): State = {
      State(x - cost + v, v + Resources(tp -> 1.0), maxT, actions :+ Action(Some(tp)))
    }
    def act(a: Action, blueprint: Map[Resource.Type, Resources]): State = time("act") {
      a.a match {
        case Some(t) => build(t, blueprint(t));
        case None => nothing()
      }
    }

    def expected(blueprint: Map[Resource.Type, Resources], verbose: Boolean = false): Double = time("expected"){
      (actions.length to maxT).foldLeft(this) { (state, t) =>
        val a = accel(v, blueprint)
        if (verbose) println(s"t=$t x=${state.x} v=${state.v} a=$a")
        State(x + v, v + a, state.maxT, actions)
      }.x(Resource.Geode)
    }

    def expand(blueprint: Map[Resource.Type, Resources]): Seq[State] = time("expand"){
      val t = actions.length
      if (t >= (maxT-1)) Seq(nothing())
      else if (x allGreater blueprint(Resource.Geode)) Seq(build(Resource.Geode, blueprint(Resource.Geode)))
      else if (t >= (maxT-2)) Seq(nothing())
      else Action.all.filter{case Action(Some(t)) => x allGreater blueprint(t); case _ => true}.map{a => act(a, blueprint) }
    }

    override def toString: String = s"x=$x v=$v ${actions.mkString("")}"
  }
  object State {
    def initial(maxT: Int): State = State(Resources.empty, Resources(o = 1), maxT, Array.empty)
  }
  def playback(actions: Seq[Action], blueprint: Map[Resource.Type, Resources]): Double = {
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

  def filtered(xs: Iterable[State], blueprint: Map[Resource.Type, Resources]): Iterable[State] = time("filtered"){
    // I have no idea why this works but it does
    val t = xs.head.actions.length
    if (xs.size < 100000) xs else {
      val expect = xs.map { state => (state, state.expected(blueprint)) }
      val best: Double = expect.map(_._2).max
      val divide: Double = (t.toDouble / xs.head.maxT) * best
      expect.filter { pair => pair._2 >= divide }.map(_._1)
    }
  }

  def pareto(xs: Iterable[State]): Iterable[State] = time("pareto"){
    xs.foldLeft(Set.empty[State]){(frontier, state) =>
      val (next, pareto) = frontier.foldLeft((Set.empty[State], false)){(prev, f) =>
        val pareto = prev._2 || (state.x anyGreater f.x)
        val dominated = state.x allGreater f.x
        val next = if (dominated) (prev._1 + f) else prev._1
        (next, pareto)
      }
      (frontier diff next) ++ Some(state).filter(_ => frontier.isEmpty || pareto)
    }
  }
  def maxGeodes(blueprint: Map[Resource.Type, Resources], maxT: Int): State = time("maxGeodes"){
    (1 to maxT).foldLeft(Iterable(State.initial(maxT))){(states, t) => {
      val start = System.currentTimeMillis()
      val next = states.flatMap{state => state.expand(blueprint) }
      val prune1 = next.groupBy(_.v).view.values.flatMap{states => pareto(states) }
      val prune2 = filtered(prune1, blueprint) // hell if I know
      val end = System.currentTimeMillis() - start
      println(s"  T=$t (next: ${next.size}, prune1: ${prune1.size}, prune2: ${prune2.size}) $end ms")
      // pruned.take(10).foreach{state => println(s"  ${state.actions.mkString("")}: x=${state.x} v=${state.v} e=${state.expected(blueprint)}")}
      prune2
    }}.maxBy(_.x(Resource.Geode))
  }

  val file = scala.io.Source.fromFile("data/2022/19")
  val blueprints = file.getLines().map(Blueprint.parse).toArray

  def part1 = blueprints.zipWithIndex.map{case (blueprint, i) =>
    val end = maxGeodes(blueprint, maxT = 24)
    //playback(end.actions, blueprint)
    println(s"${i+1}: ${end.actions.mkString("")}: ${end.x(Resource.Geode)}")
    //println("R: WWCWCWCWWWWCWWBWWGWWGWWW")
    //println("#: 123456789111111111122222")
    //println("            012345678901234")
    (i + 1) * end.x(Resource.Geode)
  }.sum
  //println(s"Part1: $part1")
  //times.foreach{case (label,time) => println(s"$label: $time ms") }

  val part2 = blueprints.take(3).zipWithIndex.map{case (blueprint, i) =>
    val end = maxGeodes(blueprint, maxT =32)
    println(s"${i+1}: ${end.actions.grouped(4).map(_.mkString("")).mkString("-")}: ${end.x(Resource.Geode)}")
    if (i == 0)
      println(s"${i+1}: WWWW-OWCC-CCCC-CBWB-BWBG-BGGG-WGGW-GGGW")
    end.x(Resource.Geode)
  }.product
  println(s"Part2: $part2")
}
