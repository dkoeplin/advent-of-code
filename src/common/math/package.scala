package common

package object math {
  def lcm(x: BigInt, y: BigInt): Long = (x * y / x.gcd(y)).toLong
  def lcm(ns: Iterable[Long]): Long = ns.foldLeft(1L){(a,b) => lcm(a, b) }
}
