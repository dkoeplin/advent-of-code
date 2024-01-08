package Year2023

import common.immutable.Range

object Day19 extends common.AoC(19, 2023) {
  case class Part(fields: Map[Char, Range]) {
    def nonEmpty: Boolean = fields.forall(_._2.nonEmpty)
    def sum: Long = "xmas".map{c => fields(c).min }.sum
    def product: Long = "xmas".map{c => fields(c).length }.product
  }
  object Part {
    val any: Part = Part("xmas".map{c => c -> Range.inclusive(1, 4000)}.toMap)
    val empty: Part = Part("xmas".map{c => c -> Range.empty}.toMap)
    private val regex = "([xmas])=([0-9]+)".r
    def unapply(line: String): Option[Part]
      = Some(Part(regex.findAllMatchIn(line).map{m => m.group(1)(0) -> Range.unit(m.group(2).toInt)}.toMap))
  }

  case class Rule(cond: Part => (Part, Part), dest: String)
  object Rule {
    def all(dest: String): Rule = Rule(p => (p, Part.empty), dest)
    private object Cond {
      private val split: Map[Char, (Range,Int) => (Range,Range)] = Map('>' -> (_ > _), '<' -> (_ < _))

      def unapply(x: String): Option[Part => (Part, Part)] = {
        val field = x.charAt(0)
        Some({p: Part =>
          val (pass, fail) = split(x(1))(p.fields(field), x.drop(2).toInt)
          (Part(p.fields + (field -> pass)), Part(p.fields + (field -> fail)))
        })
      }
    }
    def unapply(line: String): Option[Rule] = line.split(':') match {
      case Array(Cond(cond), dest) => Some(Rule(cond, dest))
      case Array(dest) => Some(Rule.all(dest))
      case _ => None
    }
  }

  case class Workflow(rules: Array[Rule]) {
    def apply(part: Part): List[(Part, String)] = rules.foldLeft(List(part -> "")){(parts, rule) =>
      val (pass, fail) = rule.cond(parts.head._1)
      (fail -> "") +: (pass -> rule.dest) +: parts.tail
    }.tail.filter(_._1.nonEmpty)
  }
  object Workflow {
    val A: (String,Workflow) = "A" -> Workflow(Array(Rule.all("A")))
    val R: (String,Workflow) = "R" -> Workflow(Array(Rule.all("R")))
    def unapply(line: String): Option[(String, Workflow)] = {
      val Array(name, list) = line.split('{')
      Some((name, Workflow(list.dropRight(1).split(',').flatMap(Rule.unapply))))
    }
  }

  def sort(parts: Array[Part], workflows: Map[String, Workflow]): (Array[Part], Array[Part]) = {
    def isNotDone(x: (Part, String)): Boolean = x._2 != "A" && x._2 != "R"
    val sorted = LazyList.iterate(parts.map(_ -> "in")){_.flatMap{case (part, bin) => workflows(bin)(part) }}
                         .dropWhile(_.exists(isNotDone)).head
    val (pass, fail) = sorted.iterator.partition(_._2 == "A")
    (pass.map(_._1).toArray, fail.map(_._1).toArray)
  }

  val lines = data.getLines()
  val workflows = (lines.takeWhile(_.nonEmpty).flatMap(Workflow.unapply) ++ Iterator(Workflow.A, Workflow.R)).toMap
  val parts = lines.flatMap(Part.unapply).toArray

  val part1 = sort(parts, workflows)
  println(s"Part 1: ${part1._1.iterator.map(_.sum).sum}")

  val part2 = sort(Array(Part.any), workflows)
  println(s"Part 2: ${part2._1.iterator.map(_.product).sum}")
}
