object Sort {
  def bubbleSort(xs: Array[Int]): Array[Int] = {
    /*
     * 非破壊的バブルソート
     */
    val lst = new Array[Int](xs.length)

    Array.copy(xs, 0, lst, 0, xs.length)

    if (xs.length <= 1) lst

    for (j <- 2 to xs.length) {
      for (i <- 0 to xs.length - j) {
        if (lst(i) > lst(i+1)) {
          val tmp = lst(i)
          lst(i) = lst(i+1)
          lst(i+1) = tmp
        }
      }
    }
    lst
  }
  def selectionSort(xs: Array[Int]): Array[Int] = {
    /*
     * 非破壊的選択ソート
     */
    val lst = new Array[Int](xs.length)
    Array.copy(xs, 0, lst, 0, xs.length)

    if (xs.length <= 1) lst

    for (i <- 0 to xs.length - 1) {
      var minIndex = i
      for (j <- i + 1 to xs.length -1) {
        if (lst(minIndex) > lst(j)) {
          minIndex = j
        }
      }
      val tmp = lst(i)
      lst(i) = lst(minIndex)
      lst(minIndex) = tmp
    }
    lst
  }

  def insertionSort(xs: Array[Int]): Array[Int] = {
    /*
     * 非破壊的挿入ソート
     */
    val lst = new Array[Int](xs.length)
    Array.copy(xs, 0, lst, 0, xs.length)

    if (xs.length <= 1) lst

    for (i <- 1 to xs.length - 1) {
      var j = i
      while(lst(j) < lst(j-1) && j > 0) {
        val tmp = lst(j)
        lst(j) = lst(j-1)
        lst(j-1) = tmp
        j -= 1
      }
    }
    lst
  }

  def merge(xs: Array[Int], ys: Array[Int]): Array[Int] = {
    var xIndex: Int = xs.length - 1
    var yIndex: Int = ys.length - 1
    var list: List[Int] = List()
    while (xIndex >= 0 && yIndex >= 0) {
      if(xs(xIndex) <= ys(yIndex)) {
        list = ys(yIndex) :: list
        yIndex -= 1
      } else {
        list = xs(xIndex) :: list
        xIndex -= 1
      }
    }
    while (yIndex >= 0) {
      list = ys(yIndex) :: list
      yIndex -= 1
    }
    while (xIndex >= 0) {
      list = xs(xIndex) :: list
      xIndex -= 1
    }
    list.toArray
  }

  def mergeSort(xs: Array[Int]): Array[Int] = {
    if (xs.length == 1) xs
    else {
      val left = 0
      val right = xs.length
      val mid: Int = left + (right - left) / 2
      merge(mergeSort(xs.slice(0, mid)), mergeSort(xs.slice(mid, xs.length)))
    }
  }

}

object Main {
  def main(args: Array[String]) {
    Sort.bubbleSort(Array(1,4,3,5,6,3,9,1)) foreach println
    Sort.selectionSort(Array(1,4,3,5,6,3,9,1)) foreach println
    Sort.insertionSort(Array(1,4,3,5,6,3,9,1)) foreach println
    Sort.mergeSort(Array(1,4,3,5,6,3,9,1)) foreach println
  }
}
