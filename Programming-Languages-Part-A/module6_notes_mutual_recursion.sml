(**
Mutual recursion
**)

(*
e.g. Allow f to call g and g to call f

...but problem in ML, needs to define bindings in order

1/ ML has special support
2/ Exists also workaround with higher-order functions

...1/ the 'and' keyword (functions and datatype bindings)
*)
fun f1 p1 = e1
and f2 p2 = e2
and f3 p3 = e3

datatype t1 = ...
and t2 = ...
and t3 = ...

(* State-machine example *)
(*
Each 'state of the computation' is a function
...state transition is 'call another function' with 'rest of input'
*)
fun state1 input_left = ...
and state2 input_left = ...
and ...

(*
.../2 the workaround with higher-order functions
define the earlier function to take in a (later) function as an argument
*)
fun earlier (f,x) = ...f y...
...(* no need to be nearby *)
fun later x = ... earlier (later, y) ...