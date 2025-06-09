(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s: string, xs: string list) : string list option =
    case xs of
        [] => NONE
        | x :: xs =>
            if same_string(s, x) then
                SOME xs
            else
                case all_except_option(s, xs) of
                    NONE => NONE
                    | SOME ys => SOME (x :: ys)

fun get_substitutions1 (lists: string list list, s: string) : string list =
    case lists of
        [] => [] (* no sublists *)
        | list :: rest =>
            case all_except_option(s, list) of
                NONE => get_substitutions1(rest, s)
                | SOME xs => xs @ get_substitutions1(rest, s)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

