object Day1 extends App {

  def readFile(filename:String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  val data1 = readFile("data/day1.txt").map(_.toInt)

  val count1a = data1.sliding(2).map{case Seq(x,y) => y > x}.toList.count(_ == true)
  println(s"Day1A: $count1a")

  val count1b = data1.sliding(3).map(_.sum).sliding(2).map{case Seq(x,y) => y > x}.toList.count(_ == true)
  println(s"Day1B: $count1b")
}
