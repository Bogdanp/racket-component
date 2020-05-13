#lang scribble/manual

@(require (for-label component
                     component/testing
                     db
                     racket/base
                     racket/contract
                     rackunit
                     rackunit/text-ui)
          scribble/example)

@title{Component}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

@defmodule[component]

@section[#:tag "intro"]{Introduction}

This library helps you manage the lifecycle of stateful
@tech{components} in your application.  It ensures that they are
started, linked together and, finally, stopped in the correct order.

By writing programs in this style, you trade some flexibility for
clarity around how and when the individual parts of your application
are initialized.

@subsection{Guide}

Let's assume that you're writing a web application that emails users
when they sign up.  Your components might be:

@itemlist[
  @item{the database,}
  @item{the mailer,}
  @item{the user manager, which depends on the database, and}
  @item{the http frontend, which depends on the mailer and the user manager.}
]

Given those components, your system might look like this:

@racketblock[
  (define-system prod
    [db make-database]
    [mailer make-mailer]
    [users (db) (lambda (db)
                  (make-user-manager db))]
    [http (mailer users) (lambda (m um)
                           (make-http m um))])

  (system-start prod-system)
  (system-stop prod-system)
]

The system is made up of a list of component declarations where each
one is made up of the unique id of a component in the system, an
optional list of dependencies (other component ids) and a function
that can be used to construct that component from its dependencies.
There are no constraints on the ids of the components in the system
and you can have multiple components of the same type (for example,
read-only and read-write databases).

The @racket[define-system] form creates a value that represents the
system and its internal dependency graph but does not start it.

The call to @racket[system-start] starts the db and the mailer first
(one or the other may be started first since neither has any
dependencies), then the user-manager and finally the http server.

Finally, the call to @racket[system-stop] stops all the components in
the system in the reverse order that they were started in.

@subsection[#:tag "limitations"]{Limitations}

Components that have no dependencies @emph{and} no dependents are
never started.

@(define e (make-base-eval))
@(e '(require component))


@section[#:tag "reference"]{Reference}
@subsection[#:tag "components"]{Components}

@deftech{Components} are plain @racket[struct]s that implement the
@racket[gen:component] interface.  All that's required of a component
is that it needs to know how to start and then stop itself.

Here's a component that doesn't do anything except flip a flag when it
gets started and stopped:

@examples[
  #:eval e
  #:label #f
  (struct mailer (started?)
    #:transparent
    #:methods gen:component
    [(define (component-start a-mailer)
       (struct-copy mailer a-mailer [started? #t]))

     (define (component-stop a-mailer)
       (struct-copy mailer a-mailer [started? #f]))])
]

@examples[
  #:eval e
  #:label #f
  (define m (mailer #f))
  (mailer-started? m)
  (mailer-started? (component-start m))
]

And here's what a component that encapsulates a database connection
pool might look like:

@(e '(require db))

@examples[
  #:eval e
  #:label #f
  (struct db (connector custodian pool)
    #:transparent
    #:methods gen:component
    [(define (component-start the-db)
       (define custodian (make-custodian))
       (struct-copy db the-db
                    [custodian custodian]
                    [pool (parameterize ([current-custodian custodian])
                           (connection-pool (db-connector the-db)))]))

     (define (component-stop the-db)
       (custodian-shutdown-all (db-custodian the-db))
       (struct-copy db the-db
                    [custodian #f]
                    [pool #f]))])
]

@examples[
  #:eval e
  #:label #f
  (define (make-db connector)
    (db connector #f #f))
]

@examples[
  #:eval e
  #:label #f
  (component-start
   (make-db (lambda ()
              (sqlite3-connect #:database 'temporary))))
]

@defidform[#:kind "interface" gen:component]{
  The generic interface that @tech{components} must implement.
}

@defproc[(component? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a @tech{component}.
}

@defproc[(component-start [c component?]) component?]{
  Starts @racket[c].
}

@defproc[(component-stop [c component?]) component?]{
  Stops @racket[c].
}

@subsection[#:tag "systems"]{Systems}

@deftech{Systems} group components together according to a declarative
specification.

When a system is started, its components are started in dependency
order (if @racket[a] depends on @racket[b] which depends on @racket[c]
then @racket[c] is started first, then @racket[b] then @racket[a]) and
injected into their dependents' factory functions (@racket[c] is
passed to @racket[b] which is finally passed to @racket[a]).

When a system is stopped, its components are stopped in the reverse
order that they were started in.

@defproc[(system? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a system.
}

@defproc[(make-system [spec (listof (or/c (list/c symbol? any/c)
                                          (list/c symbol? (listof symbol?) any/c)))]) system?]{
  Creates a system according to the given specification, but does not
  start it.  A user error is raised if the spec contains any circular
  dependencies between components.
}

@defform[(define-system id component ...+)
         #:grammar
         [(component [component-id factory-expr]
                     [component-id (dependency-id ...) factory-expr])]
         #:contracts
         [(factory-expr (-> any/c ... component?))]]{

  Combines @racket[define] and @racket[make-system].  @racket[-system]
  is appended to the given @racket[id] so

  @racketblock[
    (define-system prod
      [db make-db]
      [app (db) make-app])
  ]

  defines a system called @racket[prod-system].
}

@defproc[(system-start [s system?]) void?]{
  Starts @racket[s].  Parameterizes @racket[current-system] so that it
  refers to @racket[s] before any components are started.
}

@defproc[(system-stop [s system?]) void?]{
  Stops @racket[s].
}

@defproc*[([(system-ref [id symbol?]) component?]
           [(system-ref [s system?]
                        [id symbol?]) component?])]{
  Get a component by id from a system.  Raises @racket[exn:fail] if
  called before the system is started or if @racket[id] refers to a
  nonexistent component.

  The first variant attempts to look up @racket[id] from the
  @racket[current-system], failing if one isn't installed.
}

@defproc[(system-replace [s system?]
                         [id symbol?]
                         [factory any/c]) system?]{
  Returns a stopped copy of @racket[s] with the factory for the
  @racket[id] component replaced by @racket[factory].  This is useful
  if you have a large system and you want to replace one of its
  components with a stub (eg. for a web app's end-to-end tests).
}

@(define dot-url "https://www.graphviz.org/doc/info/lang.html")

@defproc[(system->dot [s system?]) string?]{
  Generate @hyperlink[dot-url]{dot} notation representing a system's
  dependency graph.
}

@defproc[(system->png [s system?]
                      [output-path path-string?]) boolean?]{
  Generate a PNG of a system's dependency graph.  Requires Graphviz to
  be installed and its dot command to be available on the system
  @tt{PATH}.
}


@subsection[#:tag "hatch"]{The Escape Hatch}

Manually passing components around can be painful in highly-dynamic
applications so the library provides an escape hatch for those
use-cases.  When a system is started, @racket[current-system] is
parameterized to point to the system itself.  That way, components'
start and stop functions as well as any @racket[thread]s started by
components are able to directly reference the system they are a part
of.

@defparam[current-system s (or/c false/c system?)]


@subsection[#:tag "testing"]{Testing}
@defmodule[component/testing]

When integration testing components, you often need to put together a
subset of components, start them up and reference them from within
your tests.  The @racket[system-test-suite] form provides a convenient
way to do this.

@defform[(system-test-suite id (component ...+)
           maybe-before
           maybe-after
           body-expr ...+)

         #:grammar
         [(component (code:line (component-id factory-expr)
                                (component-id (dependency-id ...) factory-expr)))

          (maybe-before (code:line)
                        (code:line #:before before-expr))

          (maybe-after (code:line)
                       (code:line #:after after-expr))]]{

  Produces a @racket[test-suite] that is associated with a custom
  @tech{system} made up of the specified @racket[component]s.  The
  system is started before the test suite runs and stopped after it
  finishes.  During startup, each individual component is bound to an
  identifier in the scope of the test suite so that tests may easily
  reference the components.

  @(e '(require component/testing rackunit rackunit/text-ui))
  @examples[
    #:eval e
    #:label #f
    (struct mailer ()
      #:methods gen:component [])
    (struct app (mailer)
      #:methods gen:component [])

    (run-tests
     (system-test-suite app ([a (m) app]
                             [m mailer])
       (test-case "instantiated"
         (check-true (system? app-system))
         (check-true (app? a))
         (check-true (mailer? (app-mailer a))))))
  ]
}


@section[#:tag "ack"]{Acknowledgements}

This library draws inspiration from Stuart Sierra's "component"
library for Clojure.
