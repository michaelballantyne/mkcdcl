# mkcdcl
miniKanren with conflict-driven clause learning using a SAT solver.
Currently supports the same syntax as
(https://github.com/minikanren/simple-minikanren): relation definitions
as function definitions, `run`, `run*`, `==`,
`fresh`, and `conde`.

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

The following are parameters that configure SMT behavior, optimizations,
and logging. Set parameters globally by calling `(param-name val)` or
locally with `parameterize`.

```
use-set-var-val!-optimization: (parameter/c boolean?)
default #t
```

When `#t`, the implementation uses faster-minikanren's `set-var-val!`
optimization.


```
smt-timeout: (parameter/c integer?)
default 3
```

Sets the timeout for queries enforced by z3, in milliseconds. For
changes in this parameter to take effect, call `(smt-hard-reset!)` to
restart the solver.

```
smt-log-stmts: (parameter/c boolean?)
default #f
```

When `#t`, the statements issued to z3 are logged to stdout.

```
smt-log-stats: (parameter/c boolean?)
default #f
```

When `#t`, log statistics on query satisfiability frequencies and the
total number of assertion variables and unifications used in a query.
Logs every 500 SMT calls.

```
smt-check-every: (parameter/c (or/c integer? #f))
default 30
```

Controls how often the SMT solver is queried. The solver will be queried for each state after this many steps of computation have been taken in miniKanren, roughly counted by disjunction and conjunction nodes entered.
