package Year2023

object Day19 extends Year2023(19) {
  case class Part(x: Int, m: Int, a: Int, s: Int) { def sum: Long = x + m + a + s }
  object Part {
    val getters: Map[Char,Part => Int] = Map('x' -> (_.x), 'm' -> {_.m}, 'a' -> (_.a), 's' -> (_.s))
    private val regex = "x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)".r
    def unapply(line: String): Option[Part] = regex.findFirstMatchIn(line).map{m =>
      Part(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt)
    }
  }
  case class Rule(cond: Part => Boolean, dest: String)
  object Rule {
    private object Cond {
      private val conds: Map[Char, (Int,Int) => Boolean] = Map('>' -> (_ > _), '<' -> (_ < _))
      def unapply(x: String): Option[Part => Boolean] = {
        val field = Part.getters(x.charAt(0))
        val cond = conds(x.charAt(1))
        val num = x.drop(2).toInt
        Some(field.andThen{f => cond(f, num) })
      }
    }
    def unapply(line: String): Option[Rule] = line.split(':') match {
      case Array(Cond(cond), dest) => Some(Rule(cond, dest))
      case Array(dest) => Some(Rule(_ => true, dest))
      case _ => None
    }

  }
  case class Workflow(rules: Array[Rule]) {
    def apply(part: Part): String = rules.find(_.cond(part)).get.dest
  }
  object Workflow {
    val A: (String,Workflow) = "A" -> Workflow(Array(Rule(_ => true, "A")))
    val R: (String,Workflow) = "R" -> Workflow(Array(Rule(_ => true, "R")))
    def unapply(line: String): Option[(String, Workflow)] = {
      val Array(name, list) = line.split('{')
      Some((name, Workflow(list.dropRight(1).split(',').flatMap(Rule.unapply))))
    }
  }
  val lines = data.getLines()
  val workflows = (lines.takeWhile(_.nonEmpty).flatMap(Workflow.unapply) ++ Iterator(Workflow.A, Workflow.R)).toMap
  val parts = lines.flatMap(Part.unapply).toArray
  def isNotDone(x: (Part, String)): Boolean = x._2 != "A" && x._2 != "R"
  val part1 = LazyList.iterate(parts.map(_ -> "in")){_.map{case (part, bin) => part -> workflows(bin)(part) }}
                      .dropWhile(_.exists(isNotDone)).head
  //part1.foreach{case (part, bin) => println(s"$part -> $bin")}
                      //.groupMapReduce(_._2){x => Array(x._1)}(_ ++ _)
  println(s"Part 1: ${part1.iterator.filter(_._2 == "A").map(_._1.sum).sum}")
}
