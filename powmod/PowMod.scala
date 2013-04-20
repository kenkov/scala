object PowMod {
  def naivePowMod(a: Int, k: Int, m: Int): Int = {
    var t: Long = 1
    var aMod: Int = a % m

    for (i <- 1 to k) {
      t = ((t % m) * aMod) % m
    }
    t.toInt
  }
  def powMod(a: Int, k: Int, m: Int): Int = {
    if (k == 0) 1
    else {
      val t = powMod(a, k / 2, m)
      (if ((k % 2) == 0) (t * t) % m else (t * t * a) % m).toInt
    }
  }
}

object PowModMain {
  def main(args: Array[String]) {
    // println(PowMod.naivePowMod(3, Int.MaxValue, 10000))
    println(PowMod.powMod(3, Int.MaxValue, 10000))
  }
}
