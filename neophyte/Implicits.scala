case class From(x: Int)

object From {
    implicit def toTo(from: From) = To(from.x)
}

case class To(y: Int) {
    def addAnother(another: Int) = To(y + another)
}

object Run extends App {
    val from = From(5)
    println(from.addAnother(10))
}

/**
 * Order of Implicit Resolution:
 * 1. First look in current scope:
 * - Implicits defined in current scope
 * - Explicitly imported implicits
 * - Wildcard imports, bringing the whole object into scope
 *
 * 2. Associated types:
 * - Companion Objects of a type:
 *   - If the conversion required is From -> To:
 *   - Look in the companion Object of the From type for a conversion to the To type
 *   - Look in the companion object of the To type for a conversion from the From type
 * - Implicit scope of an argument's type
 *   Within an operation, the implicit scope of an argument is also used to resolve implicits,
 *   using the same order of resolution as defined above
 * - Implicit scope of type arguments:
 *   eg. If we require an Ordering[A] where A is a class, if there is an implicit Ordering[A] defined in the companion object of A,
 *   that is found. This is called a 'type argument' because it is a type argument to Ordering[_].
 *   Since it is not possible to add all possible implicit conversions to the outer type (since it could be library code),
 *   this is a way for the Type Class pattern to work very effectively.
 */
