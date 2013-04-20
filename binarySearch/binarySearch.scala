object BinarySearch {
  def binarySearch(v: Int, vs: Array[Int]): Boolean = {
    /*
     * vs: **sorted** array
     */
    if (vs.length == 0) false
    var left: Int = 0
    var right: Int = vs.length - 1
    var mid: Int = 0
    while (left != right) {
      // 32bit Int のオーバーフローを防ぐ
      mid = left + (right - left) / 2
      if (v <= vs(mid))
        right = mid
      else
        left = mid + 1
    }
    return vs(left) == v
  }
  def binarySearchNewIndex(v: Int, vs: Array[Int]): Boolean = {
    /*
     * こっちの方がindex がきれい
     */
    if (vs.length == 0) false
    var left: Int = 0
    var right: Int = vs.length
    var mid: Int = 0
    while (right - left != 1) {
      // 32bit Int のオーバーフローを防ぐ
      mid = left + (right - left) / 2
      if (v < vs(mid))
        right = mid
      else
        left = mid
    }
    return vs(left) == v
  }
}
object Main {
  def main(args: Array[String]) {
    println(BinarySearch.binarySearch(0, Array(1, 2, 3)))
    println(BinarySearch.binarySearch(4, Array(1, 2, 3)))
    println(BinarySearch.binarySearch(2, Array(1, 2, 3)))
    println(BinarySearch.binarySearch(3, Array(1, 2, 4)))
    println(BinarySearch.binarySearchNewIndex(0, Array(1, 2, 3)))
    println(BinarySearch.binarySearchNewIndex(4, Array(1, 2, 3)))
    println(BinarySearch.binarySearchNewIndex(2, Array(1, 2, 3)))
    println(BinarySearch.binarySearchNewIndex(3, Array(1, 2, 4)))
  }
}
