package task

import java.io.{File, BufferedWriter, FileWriter}

import task.IpAddressType._

import scala.annotation.tailrec
import scala.io.Source

object Process extends App {
  type IpModel = (Short, Short, Short, Short, String, IpAddressType)
  type OutputModel = (String, String)

  def parseAllRanges( args: List[String] ) : List[IpModel] = {
    var list = List[IpModel]()

    val regex = """(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\-(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\t(.*?)""".r
    for (x <- args) {
      x match {
        case regex(ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, name) =>
          list = (ip1.toShort, ip2.toShort, ip3.toShort, ip4.toShort, name, IpAddressType.OPENING) ::
            (ip5.toShort, ip6.toShort, ip7.toShort, ip8.toShort, name, IpAddressType.ENDING) ::
            list
        case _ => println("No match for " + x)
      }
    }
    list
  }

  def parseAllTransactions( args: List[String] ) : List[IpModel] = {
    var list = List[(Short, Short, Short, Short, String, IpAddressType)]()

    val regex = """.*?(.*?)\t(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*?""".r
    for (x <- args) {
      x match {
        case regex(userId, ip1, ip2, ip3, ip4) =>
          list = (ip1.toShort, ip2.toShort, ip3.toShort, ip4.toShort, userId, IpAddressType.INNER) :: list
        case _ => println("No match for " + x)
      }
    }

    list
  }

  def readFileToList(filename: String) : Option[List[String]] = {
    try {
      Some(Source.fromFile(filename).getLines().toList)
    } catch {
      case e: Exception =>
        println("Unable to read file: " + filename)
        None
    }
  }

  def writeToFile(filename: String, list: List[OutputModel]) : Unit = {
    try {
      val bw = new BufferedWriter(new FileWriter(new File(filename)))
      for (x <- list) {
        bw.write(x._1 + "\t" + x._2 + "\n")
      }
      bw.close()
    } catch {
      case e: Exception => println("Unable to write to file: " + filename)
    }
  }

  def generate(input: List[IpModel]) : List[OutputModel] = {
    @tailrec
    def go(segment: String, list: List[IpModel], output: List[OutputModel]) : List[OutputModel] = list match {
      case x :: xs =>
        x._6 match {
          case IpAddressType.OPENING => go(x._5, xs, output)
          case IpAddressType.INNER =>
            if (segment != null) go(segment, xs, (x._5, segment) :: output)
            else go(segment, xs, output)
          case IpAddressType.ENDING => go(null, xs, output)
        }
      case Nil => output
    }

    go(null, input, List[OutputModel]())
  }

  val rangeLines = readFileToList("D:\\Docs\\scala\\ranges.tsv")
  val transactionLines = readFileToList("D:\\Docs\\scala\\transactions.tsv")

  val l = (parseAllRanges(rangeLines.getOrElse(List[String]())) ::: parseAllTransactions(transactionLines.getOrElse(Nil))).sortBy(x => (x._1, x._2, x._3, x._4, x._5, x._6))

  writeToFile("D:\\Docs\\scala\\output.tsv", generate(l))
}