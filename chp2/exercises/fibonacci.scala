object Fibonacci {
  def fib(n: Int): Int = {
    def go(one: Int, two: Int, acc: Int, n: Int): Int =
      if(acc == n) one + two
      else  go(two, one + two, acc + 1, n)

    go(0, 1, 2, n)
  }

  def main(args: Array[String]): Unit =
    println(fib(10))
}


