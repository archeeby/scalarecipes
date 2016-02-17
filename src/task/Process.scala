package task

import scala.collection.mutable.MutableList
import scala.io.Source

object Process extends App {
  def parseRanges( args: Array[String] ) : MutableList[IpAddress] = {
    val l = MutableList[IpAddress]()

    val regex = """.*?(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\-(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\t(.*?)""".r
    for (x <- args) {
      x match {
        case regex(ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, name) =>
          l += IpAddress(ip1.toShort, ip2.toShort, ip3.toShort, ip4.toShort, IpAddressType.OPENING)
          l += IpAddress(ip5.toShort, ip6.toShort, ip7.toShort, ip8.toShort, IpAddressType.ENDING)
        case _ => println("No match for " + x)
      }
    }

    l
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

  println(parseRanges(rangeLines))
  //parseTransactions(transactionLines)

}

