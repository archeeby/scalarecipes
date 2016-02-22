package task

import java.io.{File, BufferedWriter, FileWriter}

import task.IpAddressType._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Process extends App {
  type IpModel = (Short, Short, Short, Short, String, IpAddressType)
  type OutputModel = (String, String)

  val rangesRegex = """(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\-(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\t(.*?)""".r
  val transactionsRegex = """.*?(.*?)\t(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*?""".r

  def parseAll(list: List[String], regex: Regex)(f: (String, Regex) => List[IpModel]) : List[IpModel] = {
    @tailrec
    def go(input: List[String], output: List[IpModel]) : List[IpModel] = input match {
      case x :: xs =>
        go(xs, f(x, regex) ::: output)
      case Nil => output
    }

    go(list, List[IpModel]())
  }

  def parseRange(str: String, regex: Regex) : List[IpModel] = str match {
    case regex(ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8, name) =>
      (ip1.toShort, ip2.toShort, ip3.toShort, ip4.toShort, name, IpAddressType.OPENING) ::
        (ip5.toShort, ip6.toShort, ip7.toShort, ip8.toShort, name, IpAddressType.ENDING) :: List[IpModel]()
    case _ => List[IpModel]()
  }

  def parseTransactions(str: String, regex: Regex) : List[IpModel] = str match {
    case regex(userId, ip1, ip2, ip3, ip4) =>
      (ip1.toShort, ip2.toShort, ip3.toShort, ip4.toShort, userId, IpAddressType.INNER) :: List[IpModel]()
    case _ => List[IpModel]()
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
    def go(segment: String, list: List[IpModel], output: List[OutputModel]): List[OutputModel] = list match {
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

  val t = System.currentTimeMillis()

  val rangeLines = readFileToList("d:\\workspace-scala\\scalarecipes\\src\\task\\ranges10k6.tsv")
  val transactionLines = readFileToList("d:\\workspace-scala\\scalarecipes\\src\\task\\transactions.tsv")

  val ranges = parseAll(rangeLines.getOrElse(List[String]()), rangesRegex)(parseRange)
  val transactions = parseAll(transactionLines.getOrElse(Nil), transactionsRegex)(parseTransactions)

  val fullSortedList = ranges ::: transactions

  writeToFile("d:\\workspace-scala\\scalarecipes\\src\\task\\output.tsv", generate(fullSortedList))
  println("Time (sec): " + (System.currentTimeMillis() - t) / 1000)
}