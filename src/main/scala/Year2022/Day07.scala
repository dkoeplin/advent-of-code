package Year2022

object CD {
  def unapply(x: String): Option[String] = if (x.startsWith("$ cd ")) Some(x.substring(5)) else None
}
case class File(name: String, size: Int)
case class Directory(dirs: List[String], files: List[File])
case class Parsing(ls: Boolean, dirs: Map[String, Directory], stack: List[String])

object Day07 extends common.AoC(7, 2022) {
  val lines = data.getLines().toArray
  val structure = lines.foldLeft(Parsing(ls=false, Map.empty, Nil)){(state, line) =>
      val ls = state.ls && !line.startsWith("$")
      if (ls) {
        val current = state.stack.reverse.mkString("/")
        val dir = state.dirs.getOrElse(current, Directory(Nil, Nil))
        val next = if (line.startsWith("dir")) {
          val child = current + "/" + line.substring(4)
          Directory(child +: dir.dirs, dir.files)
        } else {
          val split = line.split(" ")
          val size = split(0).toInt
          val name = split(1)
          Directory(dir.dirs, File(name, size) +: dir.files)
        }
        Parsing(ls, state.dirs + (current -> next), state.stack)
      } else line match {
        case "$ cd .." => Parsing(ls=false, state.dirs, state.stack.tail)
        case CD(name)  => Parsing(ls=false, state.dirs, name +: state.stack)
        case "$ ls"    => Parsing(ls=true, state.dirs, state.stack)
      }
  }

  var worklist: List[String] = Nil
  var sizes: Map[String, Int] = Map.empty
  worklist = "/" +: worklist
  while (worklist.nonEmpty) {
    val current = worklist.head
    val dir = structure.dirs.getOrElse(current, Directory(Nil, Nil))
    val pend = dir.dirs.filterNot(sizes.contains)
    if (pend.isEmpty) {
      sizes = sizes + (current -> (dir.dirs.map(sizes.apply).sum + dir.files.map(_.size).sum))
      worklist = worklist.tail
    } else {
      worklist = pend ++ worklist
    }
  }
  val part1 = sizes.filter(_._2 <= 100000).values.sum
  val used = sizes("/")
  val unused = 70000000 - used
  val required = 30000000 - unused
  val part2 = sizes.filter(_._2 >= required).minBy(_._2)._2
  println(s"Part1: $part1")
  println(s"Part2: $part2")
}
