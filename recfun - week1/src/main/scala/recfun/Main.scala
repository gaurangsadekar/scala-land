package recfun
import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(r: Int, n: Int): Int = {
    // aux function computes nCr
    @tailrec
    def aux(n: Int, r: Int, num: Int, den: Int): Int = {
       if (r == 0)
         num / den
       else
         aux(n - 1, r - 1, num * n, den * r)
    }
    val col: Int = if (r < n - r) r else n - r
    aux(n, col, 1, 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def aux(stack: List[Char], chars: List[Char]): Boolean = {
      // out of characters, then stack should be empty
      if (chars.isEmpty)
        stack.isEmpty
      else {
        val ch: Char = chars.head
          // if open bracket, put it into the stack
          if (ch == '(')
            aux(ch :: stack, chars.tail)
          // if closed bracket, pop one ( from stack
          else if (ch == ')') {
            if (!stack.isEmpty)
              aux(stack.tail, chars.tail)
            else false
          }
          else
          // neither bracket, then go about business
            aux(stack, chars.tail)
      }
    }
    aux(List.empty, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    // if current estimate overshoots or no more coins
    else if (money < 0 || coins.isEmpty) 0
    else
    // recurrence is natural
    // count change not taking top coin + taking top coin
    // standard DP-Like recurrence without memoization
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
