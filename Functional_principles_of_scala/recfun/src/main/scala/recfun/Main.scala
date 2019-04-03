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
      if (c<0 || r<0 || c>r) throw  new IllegalArgumentException("Column and row cannot be 0. Columns can never be greater than rows") else
        if (c==0 || c==r) 1 else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balIter(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty){
          if (count == 0) true else false
        } else{
          if (chars.head.toString == ")") {
            if (count < 1) false else balIter(chars.tail, count - 1)
          } else if (chars.head.toString == "("){
            balIter(chars.tail, count + 1)
          } else {
            balIter(chars.tail, count)
          }
        }
      }
      balIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def loop(money: Int, coins: List[Int]): Int = {
        if (money < 0 || coins.isEmpty ) 0
        else if (money == 0 ) 1
        else loop(money, coins.tail) + loop(money - coins.head, coins)
      }

      loop(money, coins)
    }
  }
