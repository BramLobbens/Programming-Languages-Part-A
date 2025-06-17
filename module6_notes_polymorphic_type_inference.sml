(**
Type inference (polymorphic example)
**)

fun compose (f,g) = fn x => f (g x)

(*
fn compose: (T1 * T2) -> T3
f = T1
g = T2
x = T4

...fn x => f (g x)
function body of compose (T3) is a function (in itself) that takes x
T4 -> T5,
so T3 = T4 -> T5

... g being passed x
T2 = (T4 -> T6) some T6

...f taking the result of g T6 return some T7
T1 = T6 -> T7

... !! from call to f being body of T5
T5=T7

(T6 -> T7) * (T4 -> T6) -> T4 -> T7
(a' -> b') * (c' -> a') -> 'c -> b'

the compose function takes 2 functions
fn(f) that takes an a' and returns a b'
fn(g) that takes a c' and returns an a'
this function returns a function that takes a c' (to apply to the function g)
    ... the output of g will be applied to f
    the output of that f b' is the output b' of the compose
*)

(**
value restriction
**)

(* ...won't type check: *)
(*  The right‑hand side runs code (it is a function application),
so the compiler treats it as expansive *)
val foo = List.map (fn x => (x,1))
(* ...but this will *)
(* – Here the right‑hand side is just a λ‑expression,
fn xs => …, which is non‑expansive. *)
val foo = fn xs => (List.map (fn x => (x,1))) xs
fun foo xs = List.map (fn x => (x,1)) xs

(* Non‑expansive (just a value) *)
(* Expansive (does some computation) *)