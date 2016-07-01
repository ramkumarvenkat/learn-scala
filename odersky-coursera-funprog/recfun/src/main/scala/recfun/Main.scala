package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c > r + 1 || c < 0) 0
    else if (c == 0 && r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], openCount: Int, closeCount: Int): Boolean = {

      if (chars.isEmpty) openCount == closeCount
      else if (closeCount > openCount) false
      else {
        val current = chars.head
        if (current == '(') balanceIter(chars.tail, openCount + 1, closeCount)
        else if (current == ')') balanceIter(chars.tail, openCount, closeCount + 1)
        else balanceIter(chars.tail, openCount, closeCount)
      }
    }

    balanceIter(chars, 0, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (coins.isEmpty) if (money == 0) 1 else 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
