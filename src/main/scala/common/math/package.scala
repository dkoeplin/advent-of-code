package common

package object math {
  def lcm(x: BigInt, y: BigInt): Long = (x * y / x.gcd(y)).toLong
  def lcm(ns: Iterable[Long]): Long = ns.foldLeft(1L){(a,b) => lcm(a, b) }

  /// Sum of numbers [1,n] (inclusive)
  def sumOver(n: Long): Long = n*(n + 1)/2

  // Sum over just the even numbers in [1,n] (inclusive)
  //   Sum over the first N even numbers is n*(n+1)
  //   Number of even numbers in [1,n] is just n/2
  def sumOverEven(n: Long): Long = (n/2)*((n/2) + 1)

  // Sum over just the odd numbers in [1,n] (inclusive)
  def sumOverOdd(n: Long): Long = sumOver(n) - sumOverEven(n)

  def signum(x: Int): Int = if (x == 0) 0 else if (x > 0) 1 else -1
}
