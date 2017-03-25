def findFirst(ss: Array[String], key: String): Int = {
  @annotation.tailred
  def loop(n: Int): Int =
    if (n >= ss.length) -1
    else if (ss(n) == key) n
    else loop(n + 1)

  loop(0)
}
