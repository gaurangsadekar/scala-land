## Type Linearization vs The Diamond Problem
### The Diamond Problem
    A
  /   \
 B     C
  \   /
    D
* A defines a method,
* B and C override it
* D extends B and C and calls the common method. Which override is invoked?

Scala solves this problem by type linearization, by which we can always and deterministically determine what super method will be called. *Algorithm:*

* Start building a list of types, first element is the type being linearized right now.
* Expand each supertype recursively and put all their types into this list.
* Remove duplicates from resulting list, scanning from left
* To resolve a call, look in the linearized list from right to left.
