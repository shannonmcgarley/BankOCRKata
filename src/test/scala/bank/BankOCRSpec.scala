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

  }

}
