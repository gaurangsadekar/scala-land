import scala.annotation.tailrec

object Week1 {
  def abs(x: Double) = {
    if (x > 0) x else -x
  }

  def sqrt(x: Double) = {
    // since x is the same in all the blocks
    // no need to pass it around
    @tailrec
    def sqrtIter(y: Double): Double = {
      if (isGoodEnough(y)) y
      else sqrtIter(improve(y))
    }

    def isGoodEnough(y: Double) = {
      abs(y * y - x) < 1e-3 * x
    }

    def improve(y: Double) = {
      (y + x/y) / 2
    }
    sqrtIter(1)
  }
  sqrt(4)
  sqrt(256)
  sqrt(0.001)
  sqrt(1e-20)
  sqrt(1.0e20)
  sqrt(1.0e50)

  // tail recursive factorial
  def factorial(n:Int): Int = {
    @tailrec
    def aux(res: Int, n: Int): Int = {
      if (n == 0)
        res
      else
        aux(res * n, n - 1)
    }
    aux(1, n)
  }
  factorial(4)
  factorial(3)
  factorial(10)
}