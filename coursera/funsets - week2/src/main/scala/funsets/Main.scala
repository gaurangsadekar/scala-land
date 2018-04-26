package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(2), 1))
}

/*
    the anonymous function defined with the => operator
    and methods defined using the def keyword are 2 completely different types
    
    this is why we have to use _ after partial def evaluations many times.
    the compiler gives different toString type signatures to both of them
    
    => gives the FunctionN type and def gives the MethodN type
*/
