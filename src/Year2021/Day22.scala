package Year2021

case class Cube(x1: Long, x2: Long, y1: Long, y2: Long, z1: Long, z2: Long, value: Long) {
  def shape: Seq[Long] = Seq(x2 - x1 + 1, y2 - y1 + 1, z2 - z1 + 1)
  def elements: Long = shape.product
  def isEmpty: Boolean = elements <= 0
  def nonEmpty: Boolean = elements > 0
  def overlaps(rhs: Cube): Boolean = overlap(rhs).nonEmpty
  def overlap(rhs: Cube): Option[Cube] = {
    Cube.overlap(x1, x2, rhs.x1, rhs.x2).flatMap{case (xMin,xMax) =>
      Cube.overlap(y1, y2, rhs.y1, rhs.y2).flatMap{case (yMin,yMax) =>
        Cube.overlap(z1, z2, rhs.z1, rhs.z2).map{case (zMin,zMax) =>
          Cube(xMin, xMax, yMin, yMax, zMin, zMax, rhs.value)
        }
      }
    }
  }

  def remove(rhs: Cube): Seq[Cube] = overlap(rhs) match {
    case Some(union) => Cube.fragments.flatMap { case (x, y, z) =>
      val (xMin,xMax) = if (x == -1) (x1,union.x1-1) else if (x == 0) (union.x1,union.x2) else (union.x2+1,x2)
      val (yMin,yMax) = if (y == -1) (y1,union.y1-1) else if (y == 0) (union.y1,union.y2) else (union.y2+1,y2)
      val (zMin,zMax) = if (z == -1) (z1,union.z1-1) else if (z == 0) (union.z1,union.z2) else (union.z2+1,z2)
      val cube = Cube(xMin, xMax, yMin, yMax, zMin, zMax, value)
      Some(cube).filter(_.nonEmpty)
    }
    case None => Seq(this)
  }
  override def toString: String = s"$x1:$x2,$y1:$y2,$z1:$z2 [$value] ($elements)"
}
object Cube {
  private val Num = "(-?[0-9]+)"
  private val CmdPattern = (s"(o.+) x=$Num\\.\\.$Num,y=$Num\\.\\.$Num,z=$Num\\.\\.$Num").r

  def parse(line: String): Option[Cube] = line match {
    case CmdPattern(cmd,x1,x2,y1,y2,z1,z2) => Some(Cube(x1.toInt,x2.toInt,y1.toInt,y2.toInt,z1.toInt,z2.toInt, if (cmd == "on") 1 else 0))
    case _ => None
  }
  def overlap(x1: Long, x2: Long, x3: Long, x4: Long): Option[(Long, Long)] = {
    val min = Math.max(x1, x3)
    val max = Math.min(x2, x4)
    if (min <= max) Some((min, max)) else None
  }
  def overlaps(x1: Long, x2: Long, x3: Long, x4: Long): Boolean = overlap(x1, x2, x3, x4).nonEmpty
  protected val fragments: Seq[(Int,Int,Int)] = (-1 to 1).flatMap{ x =>
    (-1 to 1).flatMap{ y => (-1 to 1).flatMap{ z => if (x == 0 && y == 0 && z == 0) None else Some((x,y,z)) }}
  }
}

object Day22 extends App {
  val file = io.Source.fromFile("./data/22")
  val cmds = file.getLines().flatMap(Cube.parse).toArray

  def addCube(cubes: Seq[Cube], next: Cube): Seq[Cube] = {
    if (next.value >= 1)
      cubes ++ cubes.foldLeft(Seq(next)){case (added,exist) => added.flatMap(_.remove(exist)) }
    else
      cubes.flatMap{exist => exist.remove(next) }
  }

  // Part 1
  val initial = Cube(-50, 50, -50, 50, -50, 50, value=0)
  val part1 = cmds.flatMap{cube => initial.overlap(cube)}
                  .foldLeft(Seq.empty[Cube]){case (cubes, cube) => addCube(cubes, cube) }
  println(s"Part 1 (Total): ${part1.map(_.elements).sum}")

  // Part 2
  val part2 = cmds.foldLeft(Seq.empty[Cube]){case (cubes, cube) => addCube(cubes, cube) }
  println(s"Part 2 (Total): ${part2.map(_.elements).sum}")
}
