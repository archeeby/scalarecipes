package task

import scala.io.Source

object Process extends App {
  def parseRanges( args: Array[String] ) : Unit = {
    val regex = """.*?(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*""".r
    for (x <- args) {
      x match {
        case regex(ip1, ip2, ip3, ip4) => println(ip1, ip2, ip3, ip4)
        case _ => println("No match.")
      }
    }
  }

  def parseTransactions( args: Array[String] ) : Unit = {
    val regex = """.*?(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*""".r
    for (x <- args) {
      x match {
        case regex(ip1, ip2, ip3, ip4) => println(ip1, ip2, ip3, ip4)
        case _ => println("No match.")
      }
    }
  }

  val ranges = "D:\\Docs\\scala\\ranges.tsv"
  val transactions = "D:\\Docs\\scala\\transactions.tsv"

  val rangeLines = Source.fromFile(ranges).getLines.toArray
  val transactionLines = Source.fromFile(transactions).getLines.toArray

  parseRanges(rangeLines)
  parseTransactions(transactionLines)

}

