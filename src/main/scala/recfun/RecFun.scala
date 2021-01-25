package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /*
    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

   (0,0)
   (1,0) (1,1)
   (2,0) (2,1) (2,2)
   (3,0) (3,1) (3,2) (3,3)
   (4,0) (4,1) (4,2) (4,3) (4,4)

   T(r,0)=T(r,r)=1
   T(r,c)=T(r-1,c-1)+T(r-1,c), for 0<c<r
   */
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else if (r == 0) c
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceHelper(chars: List[Char], openParenCount: Int): Boolean = {
      if (chars.isEmpty) openParenCount == 0
      else if (chars.head == '(') balanceHelper(chars.tail, openParenCount+1)
      else if (chars.head == ')') openParenCount > 0 && balanceHelper(chars.tail, openParenCount-1)
      else balanceHelper(chars.tail, openParenCount)
    }

    balanceHelper(chars, 0)
  }

  /*
   For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
