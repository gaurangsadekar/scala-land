// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new IllegalArgumentException("no pred for 0")
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new IllegalStateException("no -ve numbers")
}

/**
  * Succ(n) is the number after n,
  * so its predecessor is n
  * Succ(Succ(n)) is the number after the current number
  * - logic is awesome. Thinking of everything as functions
  * is Hard.
  */
class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def +(that: Nat): Nat = new Succ(n + that)
  def -(that: Nat): Nat = if (that.isZero) n else n - that.predecessor
}