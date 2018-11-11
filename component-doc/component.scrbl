#lang scribble/manual

@(require (for-label component
                     racket))

@title{Component}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[component/base]

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
  #:style 'unordered
  @item{the database (no dependencies),}
  @item{the mailer (no dependencies),}
  @item{the user manager (depends on the database) and}
  @item{the server (depends on the database, the mailer and the user manager).}
]

Assuming that each of the identified components is a struct that
implements the @racket[gen:component] interface, your system might
look something like this:

@racketblock[
  (define prod-system
    (make-system `((db ,make-database)
                   (mailer ,make-mailer)
                   (user-manager [db] ,(lambda (db) (make-user-manager db)))
                   (server [db mailer user-manager] ,(lambda (db mailer user-manager)
                                                        (make-server db mailer user-manager))))))

  (start-system prod-system)
  (stop-system prod-system)
]

The system specification is made up of a list of component
specifications. Each component specification is made up of the unique
id of a component in the system, an optional list of dependencies
(other component ids) and a function that can be used to construct
that component from its dependencies.

The call to @racket[make-system] builds the system struct and its
internal dependency graph but does not start the system.

The call to @racket[system-start] starts the db and the
mailer first (one or the other may be first since neither has any
dependencies), then the user-manager and finally the server.

Finally, the call to @racket[system-stop] stops each component in the
system in the reverse order that they were started.

The above definition can be simplified by using the
@racket[define-system] syntax:

@racketblock[
  (define-system prod
    [db make-database]
    [mailer make-mailer]
    [user-manager (db) (lambda (db) (make-user-manager db))]
    [server (db mailer user-manager) (lambda (db mailer user-manager)
                                        (make-server db mailer user-manager))])
]

The order in which components are declared in the specification is not
important and there are no constraints on the names of each component
in a system specification so having more than one component of each
type is perfectly fine:

@racketblock[
  (define-system prod
    [app (read-db write-db) make-app]
    [read-db (lambda ()
               (make-database #:name "read-replica"))]
    [write-db (lambda ()
                (make-database #:name "master"))])
]


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
dependency order (if @racket['a] depends on @racket['b] which depends
on @racket['c] then @racket['c] is started first, then @racket['b]
then @racket['a]) and injected into their dependents' factory
functions (@racket['c] is passed to @racket['b] which is finally
passed to @racket['a]).

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
  "-system" is appended to the given name so

  @racketblock[
    (define-system prod
      [db make-db]
      [app (db) make-app])
  ]

  defines a system called "prod-system".
}

@defproc[(system-start [system system?]) void?]{
  Starts a system.
}

@defproc[(system-stop [system system?]) void?]{
  Stops a system.
}


@section[#:tag "ack"]{Acknowledgements}

This library draws inspiration from Stuart Sierra's "component"
library for Clojure.
