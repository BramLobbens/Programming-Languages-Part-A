(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution. *)
val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x,0)))

(* 2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive). *)
val longest_string1 = foldl (fn (x,y) => if String.size x > String.size y then x else y) ""

(* 3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size .*)
val longest_string2 = foldl (fn (x,y) => if String.size x >= String.size y then x else y) ""

(* 4. Write functions longest_string_helper, longest_string3, and longest_string4 such that [...] *)
(*  (int * int -> bool) -> string list -> string  *)
fun longest_string_helper predicate =
	foldl (fn (x,y) => if predicate (String.size x, String.size y) then x else y) ""
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* 5. Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2. *)
val longest_capitalized = longest_string3 o only_capitals