/*
 * Self recursive types allows us to add powerful compile time constraints into the type definition
 */

// -- first attempt
trait Doubler[T <: Doubler[T]] {
    def double: T
}

case class Square(base: Int) extends Doubler[Square] {
    override def double: Square = Square(base * 2)
}

// this allows me to write something like
case class Apple(kind: String) extends Doubler[Square] {
    override def double: Square = Square(5)
}

// which is a weird implementation
// this is allowed because the self type of Doubler is satisfied by Square
// this lets Apple return a Square without a problem

// --- block weird behavior by using a self type in conjuction
trait SafeDoubler[T <: Doubler[T]] { self: T =>
    def double: T
}

// this will not allow Apple to compile, because:
// Apple extends Doubler[Square], which is fine since Square extends Doubler (recursive type)
// however, Apple does not also extend Square (which is enforced by self: T from Doubler)
// this causes illegal inheritance

