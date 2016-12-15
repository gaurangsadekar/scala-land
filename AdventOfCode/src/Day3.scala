import scala.annotation.tailrec

class Triangles {

  def validTriangle(edges: Array[Int]) = {
    def triangleInequality(x: Int, y: Int, z: Int) = x + y > z
    edges match {
      case Array(a, b, c) => triangleInequality(a, b, c) &&
        triangleInequality(c, a, b) &&
        triangleInequality(b, c, a)
    }
  }

  def countTriangles(input: Array[Array[Int]]) = input.filter(validTriangle).length
}
object Day3 {
  def apply(inputList: List[String]) = {
    val input = inputList.map(_.trim().split("\\s+").map(_.toInt))
    println(input.length)
    val t = new Triangles()
    println(t.countTriangles(input.toArray))
    println(t.countTriangles(transform(input)))
  }

  def transform(data: List[Array[Int]]): Array[Array[Int]] = {
    @tailrec
    def transformIntoAcc(data: List[Array[Int]], acc: List[Array[Int]]): List[Array[Int]] = data match {
      case x :: y :: z :: ds => {
        val trans = Array(x, y, z).transpose.toList
        transformIntoAcc(ds, trans ::: acc)
      }
      case Nil => acc
    }
    transformIntoAcc(data, List.empty).toArray
  }
}
