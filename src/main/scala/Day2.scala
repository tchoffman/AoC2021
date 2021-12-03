object Day2 extends App {

  def readFile(filename:String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }

  val data2 = readFile("data/day2.txt").map(_.split(" ").toList)
  var pos_x = 0
  var pos_y = 0

  for (line <- data2) yield line match {
    case Seq("forward", d) =>  pos_x += d.toInt
    case Seq("up", d) => pos_y -= d.toInt
    case Seq("down", d) => pos_y += d.toInt
  }
  println(pos_x, pos_y, pos_x * pos_y)

  pos_x = 0
  pos_y = 0
  var aim = 0

  for (line <- data2) yield line match {
    case Seq("forward", d) =>  {
      pos_x += d.toInt
      pos_y += (d.toInt * aim)
    }
    case Seq("up", d) => aim -= d.toInt
    case Seq("down", d) => aim += d.toInt
  }
  println(pos_x, pos_y, pos_x * pos_y)
}
