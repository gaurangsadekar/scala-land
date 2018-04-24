/*
standard approach of concurrency is shared mutable state - a number of stateful objects which can be modified by multiple different parts in code.
Locks are used to control access to the shared state. These are low level constructs that can be very hard to reason about.

Actor model requires you to design and write your application with concurrency in mind from the ground up.
The idea is that your application consists of lots of light-weight entities called actors. Each actor is only responsible for a very small task.

Each Actor resides in and is created by an actor system.

Actors implement a method called `receive` which returns a PartialFunction[Any, Unit]
We can decide what to do based on pattern matching of the messages received even if they are untyped.

Receive is a side-effecting method, which makes sense because these actors would store state.
It is also untyped, which allows you to forward messages and do a bunch of other fancy stuff.
Actors are asynchronous and non-blocking. Senders don't wait for receivers to have received messages in their queues.
Receivers are not blocking, waiting to receive messages. They are notified by a dispatcher when a message is received, and allocated an available thread if they are idle.
The only time an actor blocks is when it is processing a message. Hence the rule is to stay in the Receive pfs as little as possible.

Actors are created using the actorOf method, which returns an ActorRef. Actors never communicate with each other directly. Components acquire references to the actors they need to send messages to.
ActorRefs act as proxies to the actual actors, hence the component using the actor is agnostic of where the actor is actually executing. This is called Location Transparency.
*/
