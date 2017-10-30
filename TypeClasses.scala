trait Show[A] {
  def show(a: A): String
}

object Show {
  // implicitly is a function that takes an implicit as a param.
  // The context bound A : Encap is syntactic sugar to get an addition param (implicit <generatedName>: Encap[A]) for the function
  // this implicit param is taken by the implicitly fn to return an the instance (identified by generatedName) of Encap[A]

  // useful if this implicit has to be used across multiple functions,
  // instead of declaring implicit param everywhere. Same effect as implicitly
  def apply[A](implicit sh: Show[A]) = sh

  def show[A : Show](a: A) = Show[A].show(a)

  implicit class ShowOps[A : Show](a : A) {
    def show = Show[A].show(a)
  }

  // **** helpers and tangible implicit implementations
  def instance[A](func: A => String): Show[A] = new Show[A] {
    override def show(a: A) = func(a)
  }

  implicit val intShow: Show[Int] = instance(x => s"int $x")


  implicit val stringShow: Show[String] = instance(x => s"str $x")
}

object Test {
  import Show._
  def main(args: Array[String]): Unit = {
    println(20.show)
  }
}
