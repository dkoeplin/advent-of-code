package Year2022

case class NestedList(list: Array[Either[Int, NestedList]] = Array.empty) {
  def :+(rhs: String): NestedList = if (rhs.isEmpty) this else this :+ rhs.toInt
  def :+(rhs: Int): NestedList = NestedList(list :+ Left(rhs))
  def :+(rhs: NestedList): NestedList = NestedList(list :+ Right(rhs))

  def le(rhs: NestedList, tab: Int): Option[Boolean] = {
    val result = list.indices.foldLeft(None : Option[Boolean]){(result, i) =>
      if (result.nonEmpty) result
      else if (i < rhs.list.length) (list(i), rhs.list(i)) match {
        case (Left(a), Left(b)) =>
          println("  " * tab + s"- Compare $a vs $b")
          if (a < b) Some(true) else if (a > b) Some(false) else None
        case (Left(a), Right(b)) =>
          println("  " * tab + s"- Compare $a vs $b:")
          NestedList(Array(Left(a))).le(b, tab + 1)
        case (Right(a), Left(b)) =>
          println("  " * tab + s"- Compare $a vs $b:")
          a.le(NestedList(Array(Left(b))), tab + 1)
        case (Right(a), Right(b)) =>
          println("  " * tab + s"- Compare $a vs $b:")
          a.le(b, tab + 1)
      } else {
        println("  " * tab + "Right side is shorter, wrong order")
        Some(false)
      }
    }
    if (result.nonEmpty) result else if (rhs.list.length > list.length) Some(true) else None
  }
  def <=(rhs: NestedList): Boolean = this.le(rhs, 1).getOrElse{
    throw new Exception(s"Undefined ordering for $this vs $rhs")
  }

  override def toString: String = "[" + list.map{
    case Left(i) => i.toString
    case Right(v) => v.toString
  }.mkString(",") + "]"
}
object NestedList {
  case class State(list: List[NestedList], num: String, done: Option[NestedList])
  object State {
    def empty: State = State(Nil, "", None)
  }
  def parse(filename: String, no: Int, line: String): NestedList = {
    val result = line.zipWithIndex.foldLeft(State.empty){
      case (State(list, num, done), (',', _)) => State((list.head :+ num) +: list.tail, "", done)
      case (State(list, num, done), ('[', _)) => State(NestedList() +: list, "", done)
      case (State(head :: rest, num, done), (']', _)) =>
        val completed = head :+ num
        if (rest.nonEmpty) State((rest.head :+ completed) +: rest.tail, "", done) else State(Nil, "", Some(completed))
      case (State(list, num, done), (c,_)) if c.isDigit => State(list, num :+ c, done)
      case (state, (c, col)) =>
        sys.error(s"$line\n${" "*col}^\n$filename:$no:${col+1}: Unrecognized character $c.")
        throw new Exception("Unable to parse nested list.")
    }
    result.done.getOrElse{
      sys.error(s"$line\n$filename:$no: Failure while parsing list.")
      throw new Exception("Unable to parse nested list (" + result + ")")
    }
  }
}

object Day13 extends App {
  val filename = "data/2022/13"
  val file = scala.io.Source.fromFile(filename)
  val lists: Array[NestedList] = file.getLines().zipWithIndex.filter(_._1.nonEmpty).map{case (line,no) =>
    NestedList.parse(filename, no, line)
  }.toArray
  val part1 = lists.grouped(2).zipWithIndex.filter{case (pair, _) => pair(0) <= pair(1) }.map(_._2 + 1).sum
  println(s"Part1: $part1")

  val l2 = NestedList.parse("L2", 0, "[[2]]")
  val l6 = NestedList.parse("L6", 0, "[[6]]")
  val l2_pos = (lists.count(_ <= l2) + 1)
  val l6_pos = (lists.count(_ <= l6) + 2) // [[2]] <= [[6]] always
  println(s"Part2: ${l2_pos * l6_pos} ($l2_pos * $l6_pos)")
}
