package bank

object BankOCR {

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
      .filter(x => x._1 == List(63) || x._1 == List(92))
      .map(_._2)
  }





  def apply(scan: String):String = {
    val numString = scanToString(scan)
    checkSum(numString)
  }
}
