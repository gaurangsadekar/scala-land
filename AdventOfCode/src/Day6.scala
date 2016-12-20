class RepetitionCode {
  def mostCommonByColumn(input: List[String], sortTupleKey: ((Char, Int)) => Int) = {
    input
      .map(_.toCharArray)
      .toArray
      .transpose
      .map(_.groupBy(identity)
        .mapValues(_.length)
        .maxBy[Int](sortTupleKey))
      .map(_._1)
      .mkString("")
  }
}
object Day6 {
  def apply(inputList: List[String]) = {
    val rc = new RepetitionCode()
    type charIntTup = (Char, Int)
    println(rc.mostCommonByColumn(inputList, _._2))
    println(rc.mostCommonByColumn(inputList, -_._2))
  }
}
