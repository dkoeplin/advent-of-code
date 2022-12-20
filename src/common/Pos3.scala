package common

case class Pos3(x: Int, y: Int, z: Int) {
  def +(rhs: Pos3): Pos3 = Pos3(x + rhs.x, y + rhs.y, z + rhs.z)
  def -(rhs: Pos3): Pos3 = Pos3(x - rhs.x, y - rhs.y, z - rhs.z)
  def *(rhs: Pos3): Pos3 = Pos3(x * rhs.x, y * rhs.y, z * rhs.z)
  def min(rhs: Pos3): Pos3 = Pos3(Math.min(x, rhs.x), Math.min(y, rhs.y), Math.min(z, rhs.z))
  def max(rhs: Pos3): Pos3 = Pos3(Math.max(x, rhs.x), Math.max(y, rhs.y), Math.max(z, rhs.z))
  def to(rhs: Pos3): Iterator[Pos3] = {
    (x to rhs.x).iterator.flatMap{i =>
      (y to rhs.y).flatMap{j =>
        (z to rhs.z).map{k => Pos3(i, j, k) }
      }
    }
  }
  def isIn(min: Pos3, max: Pos3): Boolean = {
    x >= min.x && x <= max.x && y >= min.y && y <= max.y && z >= min.z && z <= max.z
  }
}

object Pos3 {
  def parse(line: String, delim: String): Pos3 = {
    val Seq(x, y, z) = line.split(delim).toSeq
    Pos3(x.toInt, y.toInt, z.toInt)
  }
  def parse(line: String): Pos3 = parse(line, ",")
  val NegX: Pos3 = Pos3(-1, 0, 0)
  val PosX: Pos3 = Pos3(1, 0, 0)
  val NegY: Pos3 = Pos3(0, -1, 0)
  val PosY: Pos3 = Pos3(0, 1, 0)
  val NegZ: Pos3 = Pos3(0, 0, -1)
  val PosZ: Pos3 = Pos3(0, 0, 1)
  // Non-diagonal adjacencies
  val adjs: Seq[Pos3] = Seq(NegX, PosX, NegY, PosY, NegZ, PosZ)
}