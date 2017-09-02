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
      if (c < 0 || c > r*2) 0
      else if (c == 0 && r == 0) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance_helper(open_brace_count: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) (open_brace_count == 0)
        else {
          if (chars.head == '(') balance_helper(open_brace_count + 1, chars.tail)
          else if (chars.head == ')') {
            if (open_brace_count < 1) false
            else balance_helper(open_brace_count-1, chars.tail)
          }
          else balance_helper(open_brace_count, chars.tail)
        }
      }
      balance_helper(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def count_helper(money: Int, curr_coin:Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0) 0
        else { // (money > 0)
          if (coins.isEmpty) {
            count_helper(money - curr_coin, curr_coin, coins)
          }
          else {
            count_helper(money - curr_coin, curr_coin, coins) +
            count_helper(money, coins.head, coins.tail)
          }
        }
      }
      if (coins.isEmpty) 0
      else count_helper(money, coins.head, coins.tail)
    }

  }
