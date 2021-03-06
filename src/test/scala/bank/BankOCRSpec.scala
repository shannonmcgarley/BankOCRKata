package bank

import org.scalatest.{MustMatchers, WordSpec}

class BankOCRSpec extends WordSpec with MustMatchers {


  "Bank" must {

    "return 5 when given Faxed5" in{
      BankOCR.faxConverter(
        " _ " +
        "|_ " +
        " _|") mustEqual 5

    }

    "return 7 when given Faxed7" in{
      BankOCR.faxConverter(
        " _ " +
        "  |" +
        "  |") mustEqual 7
    }

    "return 3 when given Faxed3" in{
      BankOCR.faxConverter(
        " _ " +
        " _|" +
        " _|") mustEqual 3
    }

    "return List(fax3,...,fax3)when given Faxed333333333" in{

      BankOCR.numberSplitter(
            " _  _  _  _  _  _  _  _  _ \n" +
            " _| _| _| _| _| _| _| _| _|\n" +
            " _| _| _| _| _| _| _| _| _|\n") mustEqual List(" _  _| _|"," _  _| _|"," _  _| _|"," _  _| _|"," _  _| _|"," _  _| _|"," _  _| _|"," _  _| _|"," _  _| _|")

    }

    "return List(fax1,...,fax1) when given Faxed111111111" in{

      BankOCR.numberSplitter(
          "                           \n" +
          "  |  |  |  |  |  |  |  |  |\n" +
          "  |  |  |  |  |  |  |  |  |\n") mustEqual List("     |  |","     |  |","     |  |","     |  |","     |  |","     |  |","     |  |","     |  |","     |  |")
    }
    "return '111111111' when given List(fax1,...,fax1)" in {

      BankOCR.numberChanger(List("     |  |","     |  |","     |  |","     |  |","     |  |","     |  |","     |  |","     |  |","     |  |")) mustEqual "111111111"


    }

    "return 333333333when given Faxed333333333" in{

      BankOCR.scanToString(
          " _  _  _  _  _  _  _  _  _ \n" +
          " _| _| _| _| _| _| _| _| _|\n" +
          " _| _| _| _| _| _| _| _| _|\n") mustEqual "333333333"

    }


    "return 123456789 when given Faxed123456789" in{

      BankOCR.scanToString(
          "    _  _     _  _  _  _  _ \n" +
          "  | _| _||_||_ |_   ||_||_|\n" +
          "  ||_  _|  | _||_|  ||_| _|") mustEqual "123456789"

    }


    "return valid when given 345882865" in{

      BankOCR.checkSum("345882865") mustEqual "valid"

    }

    "return ILL when given '123356789'" in{

      BankOCR.checkSum("123356789") mustEqual "ILL"

    }

    "return valid when given '123456789'" in{

      BankOCR.checkSum("123456789") mustEqual "valid"

    }

    "return '?' when given Fax L" in{
      BankOCR.faxConverter(
          "   " +
          "|  " +
          "|_ ") mustEqual '?'
    }

    "return ERR when given 345?82865" in{

      BankOCR.checkSum("345?82865") mustEqual "ERR"

    }
    "return valid when given scan123..89 " in {

    BankOCR.apply(
        "    _  _     _  _  _  _  _ \n" +
        "  | _| _||_||_ |_   ||_||_|\n" +
        "  ||_  _|  | _||_|  ||_| _|") mustEqual "valid"

  }

    "return ERROR when given scanL23..89 " in {

      BankOCR.apply(
          "    _  _     _  _  _  _  _ \n" +
          "|   _| _||_||_ |_   ||_||_|\n" +
          "|_ |_  _|  | _||_|  ||_| _|") mustEqual "ERR"

    }

    "return ILLEGAL when given scan113..89 " in {

      BankOCR.apply(
          "       _     _  _  _  _  _ \n" +
          "  |  | _||_||_ |_   ||_||_|\n" +
          "  |  | _|  | _||_|  ||_| _|") mustEqual "ILL"

    }


    "return List(3) when given Fax backwards F" in{
      BankOCR.possibleNumbers(
          " _ " +
          " _|" +
          "  |") mustEqual List(3)
    }

    "return List(4) when given Fax -|" in{
      BankOCR.possibleNumbers(
          "   " +
          " _|" +
          "  |") mustEqual List(4)
    }

    "return List(0,8) when given Fax [] " in{
      BankOCR.possibleNumbers(
          " _ " +
          "| |" +
          "|_|") mustEqual List(0,8)
    }


    "return List(0,9) when given Fax that could be 0 or a 9 " in{
      BankOCR.possibleNumbers(
          " _ " +
          "| |" +
          " _|") mustEqual List(0,9)
    }


    "return List(6,9) when given Fax 5 that could be a 6 or a 9 " in{
      BankOCR.possibleNumbers(
          " _ " +
          "|_ " +
          " _|") mustEqual List(5,6,9)
    }


    "return List(1) when given Fax i " in{
      BankOCR.possibleNumbers(
          "   " +
          "   " +
          "  |") mustEqual List(1)
    }


    "return List() when given Fax jibberish " in{
      BankOCR.possibleNumbers(
          "   " +
          "|  " +
          "  |") mustEqual List()
    }

    "return List(1,7) when given Fax1 " in{
      BankOCR.possibleNumbers(
        "   " +
          "  |" +
          "  |") mustEqual List(1,7)
    }



  }

}
