package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def recFunction(c: Int, r: Int): Int ={
      if(c == r || c == 0) then 1
      else recFunction(c, r - 1) + recFunction(c - 1, r - 1)
    }
    recFunction(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceAcc(chars: List[Char], acc: Int): Boolean = {
        if(chars.isEmpty || acc < 0) then
          acc == 0
        else if(chars.head == ')') then
          balanceAcc(chars.tail, acc - 1)
        else if(chars.head == '(') then
          balanceAcc(chars.tail, acc + 1)
        else
          balanceAcc(chars.tail, acc)
    }

    balanceAcc(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeRec(money: Int, coins: List[Int]): Int = {
          if(money == 0) then 1
          else if(coins.isEmpty || money < 0) then 0
          else
            countChangeRec(money - coins.head, coins) + countChangeRec(money, coins.tail)
    }

    countChangeRec(money, coins)
  }
