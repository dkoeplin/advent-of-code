package Year2023

import common.math

object Day20 extends Year2023(20) {
  // Flip Flop(%): High: nothing, Low(0): (1, high), Low(1): (0, low)
  // Conjunction(&): all prev high: low, else: high
  // Broadcaster: forwards pulse to all destinations
  // Button: sends low pulse to broadcaster
  object Pulse extends Enumeration {
    val High, Low, Reset = Value
  }
  type Pulse = Pulse.Value
  trait Module {
    def state: Array[Boolean]
    def stateStr: String = state.map{b => if (b) '1' else '0'}.mkString("")
    def receive(from: String, p: Pulse): (Module, Option[Pulse])
  }
  object Module {
    def parse(x: String): (String, Module, Array[String]) = {
      val Array(tp, dests) = x.split("->")
      val consumers = dests.split(",").map(_.trim).filter(_.nonEmpty)
      tp.trim match {
        case "broadcaster" => ("broadcaster", Broadcaster, consumers)
        case n if n(0) == '%' => (n.drop(1), FlipFlop(on = false), consumers)
        case n if n(0) == '&' => (n.drop(1), Conjunction(Map.empty), consumers)
      }
    }
  }
  // First high pulse is after one low pulse
  // First low pulse is after two low pulses
  case class FlipFlop(on: Boolean) extends Module {
    def state: Array[Boolean] = Array(on)
    def receive(from: String, p: Pulse): (Module, Option[Pulse]) = p match {
      case Pulse.High => (this, None)
      case Pulse.Low => (FlipFlop(!on), Some(if (on) Pulse.Low else Pulse.High))
      case Pulse.Reset => (this, Some(Pulse.Reset))
    }
  }
  // First low is when all previous were high
  case class Conjunction(prev: Map[String, Pulse]) extends Module {
    def state: Array[Boolean] = prev.valuesIterator.map(_ == Pulse.High).toArray
    override def receive(from: String, p: Pulse): (Module, Option[Pulse]) = p match {
      case Pulse.Reset => (Conjunction(prev + (from -> Pulse.Low)), Some(Pulse.Reset))
      case _ =>
        val next = prev + (from -> p)
        (Conjunction(next), Some(if (next.forall(_._2 == Pulse.High)) Pulse.Low else Pulse.High))
    }
  }
  case object Broadcaster extends Module {
    def state: Array[Boolean] = Array.empty
    override def receive(from: String, p: Pulse): (Module, Option[Pulse]) = (this, Some(p))
  }
  case object Output extends Module {
    def state: Array[Boolean] = Array.empty
    override def receive(from: String, p: Pulse): (Module, Option[Pulse]) = (this, None)
  }
  case class SandMachine(on: Boolean = false) extends Module {
    def state: Array[Boolean] = Array(on)
    override def receive(from: String, p: Pulse): (Module, Option[Pulse]) = p match {
      case Pulse.Low => (SandMachine(true), None)
      case _ => (SandMachine(), None)
    }
  }
  case class Event(producer: String, p: Pulse, t: Int)
  case class Counts(low: Long = 0, high: Long = 0) {
    def add(pulse: Pulse, n: Int): Counts
      = Counts(low + (if (pulse == Pulse.Low) n else 0), high + (if (pulse == Pulse.High) n else 0))
  }
  case class Network(
    modules: Map[String, Module],
    edges: Map[String, Array[String]],
    pulses: Counts = Counts(),
    events: List[Event] = Nil
  ) {
    def inputs(module: String): Array[String] = edges.iterator.filter(_._2.contains(module)).map(_._1).toArray

    def skip: Network = Network(modules, edges, pulses, events.tail)
    def step: Network = {
      val Event(producer, pulse, t) = events.head
      val consumers = edges(producer)
      // println(s"[$time] $producer: $pulse x ${consumers.length}")
      val triggered = consumers.map{name =>
        val (m2, p2) = modules(name).receive(producer, pulse)
        (name -> m2, p2.map{p => Event(name, p, t + 1)})
      }
      val events2 = (events.tail ++ triggered.iterator.flatMap(_._2)).sortBy(_.t)
      Network(modules ++ triggered.iterator.map(_._1), edges, pulses.add(pulse, consumers.length), events2)
    }
    def push(p: Pulse = Pulse.Low): Network = Network(modules, edges, pulses, List(Event("button", p, 0)))
    def product: Long = {
      // println(s"Low(${pulses.low}) * High(${pulses.high}) = ${pulses.low * pulses.high}")
      pulses.low * pulses.high
    }

    private def reset(): Network = {
      LazyList.iterate((Set.empty[String], this.push(Pulse.Reset))){case (visited, network) =>
        val producer = network.events.head.producer
        if (!visited.contains(producer)) (visited + producer, network.step) else (visited, network.skip)
      }.dropWhile(_._2.events.nonEmpty).head._2
    }

    private def run(): Network = LazyList.iterate(this)(_.step).dropWhile(_.events.nonEmpty).head
    def run(n: Int): Network = (1 to n).iterator.foldLeft(this){case (network, _) =>
      network.push().run()
    }

    case class Analysis(
      history: Map[String, Int] = Map.empty,
      prev: Option[String] = None,
      period: Option[Long] = None
    ) {
      def add(state: String, t: Int): Analysis = {
        val foundCycle = prev.nonEmpty && !prev.contains(state) && history.contains(state)
        val cycle = if (foundCycle) Some((t - history(state)).toLong) else None
        val history2 = if (!history.contains(state)) history + (state -> t) else history
        Analysis(history2, Some(state), cycle)
      }
      def sync(cj: Conjunction, map: Map[String, Analysis]): Analysis = {
        val inputs = cj.prev.keysIterator.map{input => map.getOrElse(input, Analysis()).period }.toArray
        if (inputs.forall(_.nonEmpty)) {
          val period = Some(math.lcm(inputs.map(_.get)))
          println(s"${inputs.map(_.get).mkString(", ")} => $period")
          Analysis(history, prev, period)
        } else this
      }
    }

    case class State(network: Network, n: Int = 0, prev: Map[String, Analysis] = Map.empty)
    def runUntilOn(): State = {
      val producersToRx = edges.iterator.filter(_._2.contains("rx")).map(_._1).toSet
      println(s"Finding cycles for rx: ${producersToRx.mkString(", ")}")

      LazyList.iterate(State(this)){case State(network, i, map) =>
        val next = network.push().run()
        val ffs = network.modules.iterator.collect{case (name, ff: FlipFlop) => (name, ff) }
                        .map{case (name, module) => (name, module.stateStr, map.getOrElse(name, Analysis())) }
                        .filter{case (_,_,analysis) => analysis.period.isEmpty}
                        .map{case (name, state, analysis) => name -> analysis.add(state, i+1) }
        val cjs = network.modules.iterator.collect{case (name, cj: Conjunction) => (name, cj) }
                          .map{case (name, module) => (name, module, map.getOrElse(name, Analysis())) }
                          .filter{case (_,_,analysis) => analysis.period.isEmpty}
                          .map{case (name,module,analysis) => name -> analysis.sync(module, map) }
        val updates = (ffs ++ cjs).toList

        // println(s"[${i+1}] ${next.state}")
        if (updates.exists(_._2.period.nonEmpty)) {
          println(s"[${i+1}] Cycle(s) found: ${updates.iterator.filter(_._2.period.nonEmpty).map{case (name,analysis) =>
            s"${if (modules(name).isInstanceOf[FlipFlop]) '%' else '&'}$name: ${analysis.period.get}"}.mkString(", ")}"
          )
        }
        // println(s"[${i+1}] ${network.modules("bt").stateStr}")
        State(next, i + 1, map ++ updates)
      }.dropWhile{s => !s.network.modules("rx").state.head && s.n < 4096 }.head
    }

    def state: String = modules.valuesIterator.flatMap(_.state).map{b => if (b) '1' else '0'}.mkString("")
  }
  object Network {
    def empty: Network = Network(Map("output" -> Output, "rx" -> SandMachine()), Map("button" -> Array("broadcaster")))
    def parse(lines: Iterator[String]): Network = lines.foldLeft(Network.empty){(network,line) =>
      val (name, module, consumers) = Module.parse(line)
      Network(network.modules + (name -> module), network.edges + (name -> consumers))
    }.reset()
  }

  val network = Network.parse(data.getLines())
  println(s"Part 1: ${network.run(1000).product}")
  LazyList.iterate((Array("rx"), Set("button"), List.empty[String])){case (frontier, visited, ops) =>
    val current = frontier.head
    val inputs = network.inputs(current)
    val func = network.modules(current) match {
      case _: Conjunction => "!&"
      case _: FlipFlop => "T"
      case _ => ""
    }
    (inputs.filterNot(visited.contains) ++ frontier.tail, visited + current, s"$current = $func(${inputs.mkString(", ")})" +: ops)
  }.dropWhile(_._1.nonEmpty).head._3.foreach(println)
  // println(s"Part 2: ${network.runUntilOn().n}")
}
