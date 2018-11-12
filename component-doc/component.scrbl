#lang scribble/manual

@(require (for-label component
                     racket))

@title{Component}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[component]

@section[#:tag "intro"]{Introduction}

This library helps you manage the lifecycle of stateful objects in
your application.  It ensures that objects are started, linked
together and stopped in the correct order.

By writing programs in this style, you trade some flexibility for
clarity around how and when your objects are initialized and your code
becomes easier to test since swapping out real implementations of
components for stubs is trivial.

@subsection{Guide}

Let's assume that you're writing a web application that emails users
when they sign up.  Your components are probably going to be:

@itemlist[
  @item{the database (no dependencies),}
  @item{the mailer (no dependencies),}
  @item{the user manager (depends on the database) and}
  @item{the http frontend (depends on each of the above).}
]

Assuming that each of the identified components is a struct that
implements the @racket[gen:component] interface, your system might
look something like this:

@racketblock[
  (define-system prod
    [db make-database]
    [mailer make-mailer]
    [user-manager (db) (lambda (db) (make-user-manager db))]
    [http (db mailer user-manager) (lambda (db mailer user-manager)
                                     (make-http db mailer user-manager))])

  (system-start prod-system)
  (system-stop prod-system)
]

The system specification is made up of a list of component
specifications.  Each component specification is made up of the unique
name of a component in the system, an optional list of dependencies
(other component names) and a function that can be used to construct
that component from its dependencies.  There are no constraints on the
names of the components in the system and you can have multiple
components of the same type.

The @racket[define-system] form builds the system struct and its
internal dependency graph but does not start the system.

The call to @racket[system-start] starts the db and the mailer first
(one or the other may be first since neither has any dependencies),
then the user-manager and finally the http server.

Finally, the call to @racket[system-stop] stops all the components in
the system in the reverse order that they were started in.

@subsection[#:tag "limitations"]{Limitations}

Components that have no dependencies @emph{and} no dependents are
never started.

When a component fails during startup or shutdown (i.e. raises an
exception), systems don't attempt to perform any sort of cleanup.
This isn't really a limitation, but something to be aware of.


@section[#:tag "reference"]{Reference}
@subsection[#:tag "components"]{Components}

Components are the basic building blocks of this library.  Every
component needs to know how to start and stop itself.

To define your own components, implement the @racket[gen:component]
interface and its @racket[component-start] and @racket[component-stop]
functions.  Here's an minimal component implementation:

@racketblock[
  (struct mailer (started)
    #:methods gen:component
    [(define (component-start a-mailer)
       (struct-copy mailer a-mailer [started #t]))

     (define (component-stop a-mailer)
       (struct-copy mailer a-mailer [started #f]))])
]

The implementations of @racket[component-start] and
@racket[component-stop] must follow the contracts defined below.

@deftogether[(
  @defidform[#:kind "interface" gen:component]
  @defproc[(component? [component any/c]) bool?]
)]{
  The generic interface that specifies components.
}

@defproc[(component-start [component component?]) component?]{
  Starts a component.
}

@defproc[(component-stop [component component?]) component?]{
  Stops a component.
}


@subsection[#:tag "systems"]{Systems}

Systems group components together according to a declarative
specification.

When a system is started, its components are each started in
dependency order (if @racket[a] depends on @racket[b] which depends on
@racket[c] then @racket[c] is started first, then @racket[b] then
@racket[a]) and injected into their dependents' factory functions
(@racket[c] is passed to @racket[b] which is finally passed to
@racket[a]).

When a system is stopped, its components are stopped in the reverse
order that they were started in.

@defproc[(system? [system any/c]) boolean?]{
  Returns @racket[#t] if @racket[system] is a system.
}

@defproc[(make-system [spec (listof (or/c (list/c symbol? any/c)
                                          (list/c symbol? (listof symbol?) any/c)))]) system?]{
  Creates a system object according to the given dependency
  specification, but does not start it.  A user error is raised if the
  spec contains any circular dependencies between components.
}

@defform[(define-system name component ...+)
         #:grammar
         [(name id)
          (component [component-name factory]
                     [component-name (dependency-name ...) factory])
          (dependency-name component-name)
          (component-name id)
          (factory expr)]]{
  Syntactic sugar for @racket[define] and @racket[make-system].
  @racket[-system] is appended to the given name so

  @racketblock[
    (define-system prod
      [db make-db]
      [app (db) make-app])
  ]

  defines a system called @racket[prod-system].
}

@defproc[(system-start [system system?]) void?]{
  Starts a system.
}

@defproc[(system-stop [system system?]) void?]{
  Stops a system.
}

@defproc[(system-get [system system?]
                     [name symbol?]) component?]{
  Get a component by its name from a system.  Raises @racket[exn:fail]
  if called before the system is started or if @racket[name] refers to
  a component that wasn't defined.
}

@defproc[(system->dot [system system?]) string?]{
  Generate @hyperlink["https://www.graphviz.org/doc/info/lang.html"]{dot}
  notation representing a system's dependency graph.
}

@defproc[(system->png [system system?]
                      [output-path path-string?]) boolean?]{
  Generate a PNG of a system's dependency graph.  Requires Graphviz to
  be installed and its dot command to be on the PATH.
}

@section[#:tag "ack"]{Acknowledgements}

This library draws inspiration from Stuart Sierra's "component"
library for Clojure.
