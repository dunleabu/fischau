# fischau
**Finite-scheme-automata**

A mash-up of Clojure's thread first macro and a finite state machine.

The `fischau` module defines macros that can be used to define a graph.
Each edge on the graph can have an optional acceptance function
```
f: X -> [X, false]
```
and (again optionally) one of more action functions
```
g_i: X -> X
```
where `X` here is some state type.
If no acceptance funciton is supplied then the identity
function is used as a default.

The syntax to define the graph is
```scheme
(define-graph g
  (a -> b) ; edge from 'a to 'b
  (b -> c where f1)
  (b -> d where f2 then g1 g2)
  (b -> e where (f3 2) then g3) ; f3 called with extra parameter
  (d -> a then (g4 4 5)) ; g4 called with two extra parameters
```

One steps through the graph by providing a starting node
(say `'a` in this case) and a state.
The code steps through the edges applying the acceptance function.
If this function returns false the transition is rejected.
If the function returns a truthy value this state is then
passed to the action functions (which are chained) to determine
the final state.
If there are no action functions then the final state is the
state returned by the acceptance function.
This new state can then be checked against the edges for
the new node and we recursively traverse the graph.
Do this using `step-graph-recur` or (to see each transtion
printed to screen) `step-graph-verbose`.

If there are no suitable edges defined for a given node
then we return with the node being `#f` and most recent state.

Note that if there is a list in a function position in the
graph definition such as `(f3 2)` then this is used in a
thread-first way and called as `(f3 state 2)`.

## Examples

A finite state machine outputting a random binary string with form defined by the graph.
Pass in the probability(s) of exiting after emitting a `1`

```
./automaton.scm 0.1 0.01 0.001
```

A simple (and probably inefficient) lexer that can tokenize simple mathematical
expressions.

```
./lex.scm "1+2" "a + (c / (5 * 2))"
```

An example push-down automaton checking for balanced brackets in strings

```
./push-down.scm "aa" "a(a" "a)a" "a(a)a"
```

## Notes

Strictly there is no need for separate acceptance and action functions.
A single function that returns `#f` for not accepted and the (potentially)
modified state otherwise would do the same job.
