
object ClassesAndObjects {
  // class rational that has 2 components,
  // a numerator and a denominator
  // much more concise syntax than Java.
  // Literally no repeated writing
  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator should be non-zero")

    /*
    if the keyword this is used as a def, it is actually
    a custom constructor for the class
    it can return a new object using 'this' as the implicit
    primary constructor.
     */
    def this(x: Int) = this(x, 1)
    def numer = x
    def denom = y

    def add(arg: Rational) = {
      new Rational(
        numer * arg.denom + arg.numer * denom,
        denom * arg.denom
      )
    }

    def neg = {
      new Rational(-numer, denom)
    }

    def sub (arg: Rational) = {
      add(arg.neg)
    }
    override def toString = numer + "/" + denom
  }

  val default = new Rational(3)
  val p = new Rational(1, 3)
  p.numer
  p.denom
  p.neg.toString
  val q = new Rational(5, 7)
  val r = new Rational(3, 2)
  // p - q - r
  p.sub(q).sub(r)
  p sub q sub r
  /*
   now that toString has been overridden, the rational
   automatically calls toString when rendering in the
   worksheet result
  */

  // vals are always evaluated only once, at time of declaration
  // defs are evaluated every time the def
  // is called
}

