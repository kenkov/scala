object MemoSearch {
  def suckSolveSearch(n: Int): Int = {
    if (n <= 3) {
      return n
    } else {
      for (i <- 1 to 3) {
        if (suckSolveSearch(n - i) == 0) {
          return i
        }
      }
    }
    return 0
  }

  def solveMemo(n: Int, memo: Array[Int]): Int = {
    if (memo(n) >= 0)
      return memo(n)
    else if (n <= 3) {
      memo(n) = n
      return n
    } else {
      for (i <- 1 to 3) {
        if (solveMemo(n - i, memo) == 0) {
          memo(n) = i
          return i
        }
      }
    }
    memo(n) = 0
    return 0
  }
  def solve(n: Int): Int = {
    // initialize memo
    val memo = new Array[Int](n+1)
    for (i <- 0 to n) {
      memo(i) = -1
    }
    // solve
    solveMemo(n, memo)
  }
}

object Main {
  def main(args: Array[String]) = {
    for (i <- 0 to 30) {
      println(i + ": " + MemoSearch.suckSolveSearch(i))
    }
    for (i <- 0 to 30) {
      println(i + ": " + MemoSearch.solve(i))
    }
  }
}
