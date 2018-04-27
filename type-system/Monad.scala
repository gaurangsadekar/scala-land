/*
Monad is a wrapper that provides 2 operations:
- identity - the author calls is `unit`, but I am sure this is a misnomer, at least looking at the signature
    across literature, also called return, pure, and zero
- bind (flatMap)

FlatMap gives us the ability to chain operations together.
Map is a sub-functionality of having a flatMap functionality
Using identity and flatMap, we can get a map functionality

If we only had map and unit, we cannot make flatMap because both those functions don't know how to flatten
Only having maps without flattening is a category called 'FUNCTORS'.
Functor = can map
*/

trait Monad[A] {
    def flatMap[B](f: A => Monad[B]): Monad[B]

    def map[B](g: A => B): Monad[B] = flatMap(x => Monad.identity(g(x)))
}

object Monad {
    def apply[A](x: A): Monad[A]
}

/*
having the 2 methods and the correct signatures/definitions doesn't make a Monad a true Monad. They have to follow monad laws:

- left identity law:
unit(x).flatMap(f) == f(x)

- right identity law:
m.flatMap(Monad.apply) == m

- associativity law:
m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
*/

/*
Monads help to make your code functional, concise and easier to debug. It also helps you reason about code and transformations in a clearer way.
*/
