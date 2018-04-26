object HigherOrderFunctions {
  /*
   sum is a function that returns the sum
   of a function applied between a range of values
   sum takes the function which has to be applied to
   each element of the range as a single argument
   it returns a function which takes start and end
   of the range
   */
  def sum1(f: Int => Int) : (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else sumF(a + 1, b) + f(a)
    }
    sumF
  }

  /*
   with syntactic sugar and anonymous functions,
   we can define the sum function as
   multiple param lists combining the functions
   first arg list is a function that takes the 2nd
   arg list as arguments
   cute way of currying functions
   intuitive to understand order of application
  */
  def sum(f: Int => Int) (a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)
  }

  // call sum functions by first calling sum
  // and then passing the range to the returned function
  sum(x => x) (1, 5)
  /*
   since functions are first class, I can assign them
   to values and use the values as functions
   _ denotes partially applied functions
   both the following are equivalent
   */
  val s = sum(x => x * x)_
  def same = sum(x => x * x)_
  s(1,5)
  // this shows that function application associates to
  // the left

  // in class exercise -> product function
  def prod (f: Int => Int) (a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * prod(f)(a + 1, b)
  }

  prod (x => x * x) (3,4)

  // factorial in terms of prod
  // f is the identity, and prod starts at 1 and goes
  // to a parameter n
  def fact(n: Int) = prod (x => x)(1, n)
  fact(5)

  // function that generalizes sum and product
  def applyAndCombine (f: Int => Int) (combine: (Int, Int) => Int, identity: Int) (a: Int, b: Int) : Int = {
    if (a > b) identity
    else
      combine(f(a), applyAndCombine(f)(combine, identity)(a + 1, b))
  }

  def newFact(n: Int) = applyAndCombine(x => x)((x,y) => x * y, 1)(1, n)
  newFact(5)

  def newFunctionOnSquares = applyAndCombine(x => x * x)_

  // why there is no _ after this is baffling
  def sumSquares = newFunctionOnSquares((x,y) => x + y, 0)
  sumSquares(3, 5)

  def newProd(f: Int => Int) = applyAndCombine(f)((x,y) => x * y, 1)_
  newProd(x => x)(1,5)
}