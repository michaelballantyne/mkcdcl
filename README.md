# mkcdcl
miniKanren with conflict-driven clause learning using a SAT solver.
Currently supports the same syntax as
(https://github.com/minikanren/simple-minikanren): relation definitions
as function definitions, `run`, `run*`, `==`,
`fresh`, and `conde`.

## Build prerequisites

* `gcc` (or clang pretending to be gcc, on mac)
* `cmake`

## Building minisat

```
make submods
```

to checkout the submodules for `minisat` and `minisat-c-bindings`;

```
make
```

to build the libraries, and install them in `minisat/build/install` and
`minisat-c-bindings/install`.

## Running

## Chez Scheme

`mk-chez.scm` is an R6RS library. Load it with:

```
(import (mk-chez))
```

It should be in the search path when you run Chez in the directory with the
file; otherwise you'll have to set the path with Chez's `--libdirs` flag.

## Racket


```
(require "mk.rkt")
```


## Options

The following are parameters that configure SAT behavior, optimizations,
and logging. Set parameters globally by calling `(param-name val)` or
locally with `parameterize`.

```
use-set-var-val!-optimization: (parameter/c boolean?)
default #t
```

When `#t`, the implementation uses faster-minikanren's `set-var-val!`
optimization.


```
log-stats: (parameter/c boolean?)
default #f
```

When `#t`, log statistics on query satisfiability frequencies and the
total number of assertion variables and unifications used in a query.
Logs every 1000 SAT calls.

```
check-every: (parameter/c (or/c integer? #f))
default 30
```

Controls how often the SAT solver is queried. The solver will be queried for each state after this many steps of computation have been taken in miniKanren, roughly counted by disjunction and conjunction nodes entered.


```
debug-soundness: (parameter/c boolean?)
default #f
```

When this parameter is #t, miniKanren will query the SAT solver, but
not cut off branches using the result. Instead, stores that the solver
thinks are UNSAT will be marked. If any in fact succeed, meaning the
solver's answer was incorrect, an error will be raised.
