import scala.language.higherKinds
//object BlogPost {
//// consider a function that creates a list of tuples from 2 lists
//// this can be applied even to options,or even fancier types like Eithers etc
//def tuple[A, B](as: List[A], bs: List[B]): List[A, B] = as.flatMap { a =>
//  bs.map((a, _))
//}
//def tuple[A, B](as: Option[A], bs: Option[B]): Option[A, B]
///*
// * the 2 functions above are very similar, (all flatmaps are the same)
// * what's different is:
// * 1. the flatMap and map implementations (based on the types of the collection of as and bs)
// * 2. the type constructor
// *
// * we know how to pass implementations (higher order functions).
// * for the type constructor, we need 'type level functions as arguments'
// */
///* we give definition to type parameterized definitions by substitution
// * the parameter declaration F[_] means that F may not be a simple type like Int or String
// * instead. it is a one-argument type constructor, like List or Option (which themselves take a type argument)
//*/
//// this is a type signature that is basically what we are looking for
//// now we need to provide the implementations of flatMap and map for any F - create a typeclass and provide implementations for concrete types
//def tuplef[F[_], A, B](as: F[A], bs: F[B]) = ???
//// ^ cannot be implemented as written above, because we don't have functions that operate on F-constructed values
//// here are a couple of possibilities
//trait Bindable[F[_], +A] {
//  def map[B](f: A => B): F[B]
//  def flatMap[B](f: A => F[B]): F[B]
//}
//
//trait BindableTM[+A] {
//  type F[X]
//  def map[B](f: A => B): F[B]
//  def flatMap[B](f: A => F[B]): F[B]
//}
//// now that we have a Bindable, we make every type we'd like to support either inherit or implicity convert to Bindable
//// then tuplef becomes
//def tupleBindable[F[_], A, B](fa: Bindable[F, A], fb: Bindable[F, B]): F[(A, B)] = fa.flatMap { a =>
//  bs.map((a, _))
//}
///*
// * there are 2 major problems with Bindable's representation of map and flatMap
// * Both inheritance and implicits are bad to add functionality to known types
// * Knowledge of new type signature is too magical, and doing anything more general might involve screwing up the type even further
// */
//}
//
// TYPECLASSES CONSTRAIN HIGHER KINDED TYPES ELEGANTLY
trait Bind[F[_]] {
//              | note new F[A] arg
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object Bind {
  implicit val bindList = new Bind[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
    def flatMap[A, B](as: List[A])(f: A => List[B]) = as.flatMap(f)
  }

  implicit val bindOption = new Bind[Option] {
    def map[A, B](as: Option[A])(f: A => B): Option[B] = as.map(f)
    def flatMap[A, B](as: Option[A])(f: A => Option[B]) = as.flatMap(f)
  }

  /*
   * using type classes, we can keep the declaration of tuplef the same, just add an implicit param
   */
  def tupleTypeClassed[F[_] : Bind, A, B](fa: F[A], fb: F[B]) = {
    implicitly[Bind[F]].flatMap(fa){ a =>
      implicitly[Bind[F]].map(fb)((a, _))
    }
  }
}
