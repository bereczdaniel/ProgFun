package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    print(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerFunc(chars: List[Char], a: Int): Boolean = {
      if (a < 0)
        false
      else if (chars.isEmpty)
        true
      else {
        chars.head match {
          case '(' => innerFunc(chars.tail, a + 1)
          case ')' => innerFunc(chars.tail, a - 1)
          case _ => innerFunc(chars.tail, a)
        }
      }
    }
    innerFunc(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }
}
