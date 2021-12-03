object Day3 extends App {

  /**
   * Read a text file and return a list of strings
   */
  def readFile(filename:String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  /***
   * Utility to find the most common binary digit in a list of lists of 1/0
   */
  def mostCommonByPos(data: Seq[Seq[Int]]): Seq[Int] = {
    data.transpose.map(x => x.sum/x.size.toDouble).map(x => if(x >= 0.5) 1 else 0)
  }

  /**
   * Invert binary digits
   */
  def invert(x: Int): Int = {
    if (x == 0) 1 else 0
  }

  /**
   * convert a binary string to Int
   */
  def binToInt(binary: String): Int = {
    Integer.parseInt(binary, 2)
  }

  /***
  Recursive Method to
   ***/
  def getLines(data: Seq[Seq[Int]], flip: Boolean = false, pos: Int = 0): Seq[Int] = {
    if (data.length == 1) data(0)
    else {
      // Determine most common digit at each position
      val c = if (flip) mostCommonByPos(data).map(invert(_)) else mostCommonByPos(data)
      // Generate New data by filtering based on most common digit in target position
      val newData = data.filter(_(pos) == c(pos))
      // Tail Recursion
      getLines(newData, flip, pos + 1)
    }
  }

  val data3 = readFile("data/day3.txt").map(_.split("").toList.map(_.toInt))

  // Part 1: Gamma and Epsilon
  val gamma = binToInt(mostCommonByPos(data3).mkString(""))
  val epsilon = binToInt(mostCommonByPos(data3).map(invert(_)).mkString(""))
  println(gamma * epsilon)

  // Part 2:
  val O2 = getLines(data3).mkString("")
  val CO2 = getLines(data3, true).mkString("")
  println(binToInt(O2) * binToInt(CO2))
}
