package Year2021

import scala.collection.mutable

object Day23 extends App {
  val verbose = true

  def vprintln(x: => String): Unit = if (verbose) println(x)

  def vvprintln(x: => String): Unit = ()

  case class Pos(y: Int, x: Int) {
    def inRoom: Boolean = { y == 2 || y == 3 }

    def inHall: Boolean = { y == 1 }

    override def toString: String = s"($y,$x)"

    def dist(b: Pos): Int = y + Math.abs(x - b.x) + b.y - 2

    def dest(map: collection.Map[Pos, Amphipod]): Pos = {
      val target = map(this)
      val dest = Pos(Pos.kRoom3, target.room)
      if (map.get(dest).exists(_.tp == target.tp)) Pos(Pos.kRoom2, target.room) else dest
    }

    def pathTo(end: Pos): Seq[Pos] = {
      val min = Math.min(x, end.x)
      val max = Math.max(x, end.x)
      (y - 1 to 2 by -1).map { y => Pos(y, x) } ++
        (min to max).map { hall => Hall(hall) }.filterNot(_ == this) ++
        (2 to end.y).map { y => Pos(y, end.x) }
    }

    def dependencies(amps: scala.collection.Map[Pos, Amphipod]): Seq[Pos] = pathTo(dest(amps)).filter(amps.contains)
  }

  object Pos {
    val kHall = 1
    val kRoom2 = 2
    val kRoom3 = 3

    // X positions of all rooms
    val doors: Seq[Int] = Seq(3, 5, 7, 9)
    // Set of all room positions
    val rooms: Set[Pos] = (2 to 3).flatMap { y => doors.map { x => Pos(y, x) } }.toSet
    // Set of all hall positions
    val halls: Seq[Pos] = Seq.tabulate(11) { i => Pos(kHall, i + 1) }
    // All positions not immediately outside rooms
    val temps: Seq[Pos] = halls.filterNot { case Pos(_, x) => doors.contains(x) }
  }

  object Hall {
    def apply(x: Int): Pos = Pos(Pos.kHall, x)

    def unapply(p: Pos): Option[Int] = if (p.inHall) Some(p.x) else None
  }

  case class Amphipod(tp: Int) {
    val cost: Int = Math.pow(10, tp).toInt
    val char: Char = (tp + 'A').toChar

    def room: Int = Pos.doors(tp)

    override def toString: String = char.toString
  }

  case class Move(a: Amphipod, src: Pos, dst: Pos) {
    def cost: Int = a.cost * src.dist(dst)

    override def toString: String = s"[$a] $src --> $dst"
  }

  case class State(
                    amps: Map[Pos, Amphipod],
                    pending: Seq[Pos],
                    moves: Seq[Move]
                  ) {
    lazy val cost: Int = moves.map(_.cost).sum

    def notDoneSingle(): Boolean = pending.nonEmpty

    def notDoneAll(): Boolean = globalPending().nonEmpty

    def globalPending(): Seq[Pos] = amps.keySet.filter { p => !p.inRoom || p.x != amps(p).room }.toSeq

    def allows(move: Move): Boolean = move.src.pathTo(move.dst).forall { p => !amps.contains(p) }

    def expandAll(): Seq[State] = {
      var steps: Int = 0
      var states: Seq[State] = Seq(State(amps, Nil, moves))
      while (states.nonEmpty && states.exists(_.notDoneAll())) {
        states = states.flatMap(_.expandOneMover())
        val moves = states.map(_.moves.length)
        println(s"[$steps] ${states.length} (Min: ${moves.min}, Max: ${moves.max})")
        steps += 1
      }
      states
    }

    def expandOneMover(): Seq[State] = {
      val pending = globalPending()
      if (pending.isEmpty) Seq(this) else pending.flatMap { mover => expandAllForMover(mover) }
    }

    def expandAllForMover(mover: Pos): Seq[State] = {
      val deps = mover.dependencies(amps)
      if (deps.exists(_.inHall)) return Nil

      val move = Move(amps(mover), mover, mover.dest(amps))
      var states: Seq[State] = Seq(State(amps, deps, moves))
      while (states.nonEmpty && states.exists(_.notDoneSingle())) {
        states = states.flatMap(_.expandOneForMover())
      }
      val path = mover.pathTo(mover.dest(amps))
      states.flatMap { state =>
        val collide = path.filter { p => state.amps.contains(p) }
        if (collide.isEmpty)
          Some(state.add(move))
        else if (collide.forall { p => !p.pathTo(p.dest(state.amps)).contains(mover) })
          Some(state)
        else None
      }
    }

    def isLegal(move: Move): Boolean = {
      if (!this.allows(move)) return false
      val finalPath = move.dst.pathTo(move.src.dest(amps)).filter(_.inHall)
      finalPath.filter(amps.contains).forall { p =>
        !p.pathTo(p.dest(amps)).contains(move.dst)
      }
    }

    def expandOneForMover(): Seq[State] = if (pending.isEmpty) Seq(this) else pending.flatMap { src =>
      Pos.temps.map { tmp => Move(amps(src), src, tmp) }.filter(isLegal).map(this.add)
    }

    def add(move: Move): State = {
      assert(allows(move))
      State(
        amps = (amps - move.src) + (move.dst -> move.a),
        pending = pending.filterNot(_ == move.src),
        moves = moves :+ move
      )
    }
  }

  def show(amps: collection.Map[Pos, Amphipod]): Unit = {
    println("-----------------------")
    println(" 0123456789ABC")
    (0 until 5).foreach { y =>
      val line = (0 until 13).map { x =>
        val pos = Pos(y, x)
        if (amps.contains(pos)) amps(pos).toString
        else if (Pos.halls.contains(pos) || Pos.rooms.contains(pos)) "."
        else "#"
      }
      println(s"$y${line.mkString("")}")
    }
  }

  val file = io.Source.fromFile("./example/23")
  val initial: Map[Pos, Amphipod] = file.getLines().zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.filter { case (c, _) => c >= 'A' && c <= 'D' }.map { case (c, x) => Pos(y, x) -> Amphipod(c - 'A') }
  }.toMap

  val start = State(initial, Nil, Nil)
  val possible = start.expandAll()

  def show(moves: Seq[Move]): Unit = {
    var others = possible
    val amps = mutable.Map.empty[Pos, Amphipod] ++ initial

    def next(idx: Int): Unit = if (idx < moves.length - 1) {
      val next = others.groupBy(_.moves(idx).src).map(_._2.minBy(_.cost).moves(idx))
      println(s"Possible:")
      next.toSeq.sortBy(_.a.tp).foreach { move => println(s"  $move [${move.cost}]") }
      others = others.filter { state => state.moves.take(idx + 1) == moves.take(idx + 1) }
    }

    def move(move: Move): Unit = {
      val path = move.src.pathTo(move.dst)
      assert(path.forall { p => !amps.contains(p) }, s"$move is invalid!")
      amps.remove(move.src)
      amps(move.dst) = move.a
      show(amps)
      println(s"Move ${move.a} from ${move.src} to ${move.dst} [${move.cost}]")
    }

    show(amps)
    moves.zipWithIndex.foreach { case (m, i) => move(m); next(i) }
  }

  println(s"Possible strategies: ${possible.length}")
  val part1 = possible.minBy(_.cost)
  show(part1.moves)
  println(s"Part 1 (Total): ${part1.cost}")

  sys.exit(0)
  val Spot = "([0-4])([0-9ABC])-([0-4])([0-9ABC])".r
  var state = State(initial, Nil, Nil)
  var others: Seq[State] = possible
  while (true) {
    show(state.amps)
    others.filter(_.moves.size > state.moves.size)
      .groupBy(_.moves(state.moves.size).src)
      .map(_._2.minBy(_.cost).moves(state.moves.size))
      .toSeq.sortBy(_.a.tp)
      .foreach { move => println(s"$move [${move.cost}]") }

    print("Move: ")
    val in = scala.io.StdIn.readLine()
    in match {
      case Spot(y1, x1, y2, x2) =>
        val src = Pos(Integer.parseInt(y1, 16), x1.toInt)
        val dst = Pos(Integer.parseInt(y2, 16), x2.toInt)
        val move = Move(state.amps(src), src, dst)
        if (state.allows(move)) {
          state = state.add(move)
          others = others.filter(_.moves.take(state.moves.length) == state.moves)
        } else {
          println("Illegal move")
        }
      case _ => println(s"Erp? $in")
    }
  }
}
