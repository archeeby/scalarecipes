package task

object Test extends App {
  val rangesRegex = """(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\-(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})\t(.*?)""".r
  val transactionsRegex = """.*?(.*?)\t(\d{1,3})\.(\d{1,3})\.(\d{1,3})\.(\d{1,3}).*?""".r

  val rangeLines = Process.readFileToList("d:\\workspace-scala\\scalarecipes\\src\\task\\complexRanges.tsv")
  val transactionLines = Process.readFileToList("d:\\workspace-scala\\scalarecipes\\src\\task\\complexTransactions.tsv")

  val ranges = Process.parseAll(rangeLines.getOrElse(List[String]()), rangesRegex)(Process.parseRange)
  val transactions = Process.parseAll(transactionLines.getOrElse(Nil), transactionsRegex)(Process.parseTransactions)

  val fullSortedList = (ranges ::: transactions).sortBy(x => (x._1, x._2, x._3, x._4, x._6))

  for (x <- fullSortedList) {
    println(x)
  }
}