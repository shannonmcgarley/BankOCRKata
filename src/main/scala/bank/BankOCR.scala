package bank

object BankOCR {

  def faxConverter(Input: String): Int = {
    val zero =
        " _ " +
        "| |" +
        "|_|"
    val one =
        "   " +
        "  |" +
        "  |"
    val two =
        " _ " +
        " _|" +
        "|_ "
    val three =
        " _ " +
        " _|" +
        " _|"
    val four =
        "   " +
        "|_|" +
        "  |"
    val five =
        " _ " +
        "|_ " +
        " _|"
    val six =
        " _ " +
        "|_ " +
        "|_|"
    val seven =
        " _ " +
        "  |" +
        "  |"
    val eight =
        " _ " +
        "|_|" +
        "|_|"
    val nine =
        " _ " +
        "|_|" +
        " _|"
    val numberList: List[String] = List(zero,one,two,three,four,five,six,seven,eight,nine)
    numberList.indexOf(Input)
  }

  def numberSplitter(input : String): List[String] = {
   val K = 28
    val grid = List(0,1,2,K, K+1,K+2, 2*K, 2*K+1,2*K+2 )
    val fax = List.range(0,9)
    fax.map(x => grid.map(_+(x*3)).map(input(_)).mkString)
   }

  def numberChanger(input : List[String]):String = {
    input.map(faxConverter).mkString
  }

  def scanToString(scan:String):String ={
    val split = numberSplitter(scan)
    numberChanger(split)
  }
}
