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

  def remove(element: String, list: List[String]) = list diff List(element)

  def create(list: List[String], model: IpModel) : List[OutputModel] = {
    @tailrec
    def go(l: List[String], output: List[OutputModel]) : List[OutputModel] = l match {
      case x :: xs => go(xs, (model._5, x) :: output)
      case Nil => output
    }

    go(list, List[OutputModel]())
  }

  def generateFull(input: List[IpModel]) : List[OutputModel] = {
    @tailrec
    def go(segments: List[String], list: List[IpModel], output: List[OutputModel]): List[OutputModel] = list match {
      case x :: xs =>
        x._6 match {
          case IpAddressType.OPENING => go(x._5 :: segments, xs, output)
          case IpAddressType.INNER => go(segments, xs, create(segments, x) ::: output)
          case IpAddressType.ENDING => go(remove(x._5, segments), xs, output)
        }
      case Nil => output
    }

    go(List[String](), input, List[OutputModel]())
  }

  val t = System.currentTimeMillis()

  val rangeLines = readFileToList("d:\\workspace-scala\\scalarecipes\\src\\task\\ranges10k6.tsv")
  println("Read ranges (msec): " + (System.currentTimeMillis() - t))

  val transactionLines = readFileToList("d:\\workspace-scala\\scalarecipes\\src\\task\\transactions.tsv")
  println("Read transactions (msec): " + (System.currentTimeMillis() - t))

  val ranges = parseAll(rangeLines.getOrElse(List[String]()), rangesRegex)(parseRange)
  println("Parse ranges (msec): " + (System.currentTimeMillis() - t))

  val transactions = parseAll(transactionLines.getOrElse(Nil), transactionsRegex)(parseTransactions)
  println("Parse transactions (msec): " + (System.currentTimeMillis() - t))

  val fullSortedList = (ranges ::: transactions).sortBy(x => (x._1, x._2, x._3, x._4, x._6))
  println("Lists concatenation + sorting (msec): " + (System.currentTimeMillis() - t))

  val outputList = generateFull(fullSortedList).distinct.sortBy(x => x._2)
  println("Processing (msec): " + (System.currentTimeMillis() - t))

  writeToFile("d:\\workspace-scala\\scalarecipes\\src\\task\\output.tsv", outputList)
  println("Write to file (msec): " + (System.currentTimeMillis() - t))
}