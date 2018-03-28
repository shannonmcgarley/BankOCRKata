package bank

object BankOCR extends App {

  val zero: String =
      " _ " +
      "| |" +
      "|_|"
  val one: String =
      "   " +
      "  |" +
      "  |"
  val two: String =
      " _ " +
      " _|" +
      "|_ "
  val three: String =
      " _ " +
      " _|" +
      " _|"
  val four: String =
      "   " +
      "|_|" +
      "  |"
  val five: String =
      " _ " +
      "|_ " +
      " _|"
  val six: String =
      " _ " +
      "|_ " +
      "|_|"
  val seven: String =
      " _ " +
      "  |" +
      "  |"
  val eight: String =
      " _ " +
      "|_|" +
      "|_|"
  val nine: String =
      " _ " +
      "|_|" +
      " _|"

  val numberList: List[String] = List(zero,one,two,three,four,five,six,seven,eight,nine)

  def faxConverter(Input: String): Any = {
    val output = numberList.indexOf(Input)
    if (output == -1){'?'}
    else output
  }

  def numberSplitter(input : String): List[String] = {
    val key = 28
    val grid = List(0,1,2,key, key+1,key+2, 2*key, 2*key+1,2*key+2 )
    val fax = List.range(0,9)
    fax.map(x => grid.map(_+(x*3)).map(input(_)).mkString)
   }

  def numberChanger(input : List[String]):String = {
    input.map(faxConverter).mkString
  }

  def scanToString(scan:String):String = {
    val split = numberSplitter(scan)
    numberChanger(split)
  }

  def checkSum(numbers:String): String = {
    if (numbers.contains('?')) "ERR"
    else {
      val digits: List[Int] = numbers.toList.map(_.asDigit)
      val Sum = digits.reverse.zipWithIndex.map(x => x._1 * (x._2 + 1)).sum
      if (Sum % 11 == 0) {
        "valid"
      }
      else {
        "ILL"
      }
    }
  }

  def possibleNumbers(missRead: String) : List[Int] = {
    numberList
      .map(x => missRead.zip(x)
        .map(l => l._2.compareTo(l._1))
        .filter(x => x !=0))
      .zipWithIndex
      .filter(x =>  x._1 == List() || x._1.length ==1)
      .map(_._2)
  }

def aMBProducer(scan: String) : List[String] = {
  val splitNums = numberSplitter(scan)

  for {
    a <- possibleNumbers(splitNums.head).map(_.toString)
    b <- possibleNumbers(splitNums(1)).map(_.toString)
    c <- possibleNumbers(splitNums(2)).map(_.toString)
    d <- possibleNumbers(splitNums(3)).map(_.toString)
    e <- possibleNumbers(splitNums(4)).map(_.toString)
    f <- possibleNumbers(splitNums(5)).map(_.toString)
    g <- possibleNumbers(splitNums(6)).map(_.toString)
    h <- possibleNumbers(splitNums(7)).map(_.toString)
    i <- possibleNumbers(splitNums(8)).map(_.toString)

  } yield {
    a+b+c+d+e+f+g+h+i
  }
}


  def apply(scan: String) : String = {
    val numString = scanToString(scan)
    val compare = numString.toList
    val cSnS = checkSum(numString)
    cSnS match {
      case "valid" => "valid"
      case _ => {
        val out = aMBProducer(scan).filter(x => checkSum(x) == "valid")
        val out2 = out.map(x => x.toList)
        println(out2)
        val out3 = out2.map(x => x.zip(compare))
        println(out3)
        val out4 = out3.filter(x => x.count(x => x._2 != x._1) == 1)
        println(out4)
        val out5 = out4.map(x => x.map(x => x._1).mkString)
        s"$numString AMB $out5"
      }

    }
  }

  val derek ={
        " _     _  _  _  _  _  _    \n" +
        "| || || || || || || ||_   |\n" +
        "|_||_||_||_||_||_||_| _|  |"
  }

  val derek2 = {
      " _  _  _  _  _  _  _  _  _ \n" +
      "|_||_||_||_||_||_||_||_||_|\n" +
      "|_||_||_||_||_||_||_||_||_|"
  }
println(apply(derek2))


}
