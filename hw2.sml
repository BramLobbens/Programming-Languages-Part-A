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

fun get_substitutions2 (lists: string list list, s: string) : string list =
    let fun rec_helper (lists, acc_list) =
        case lists of
            [] => acc_list (* if no more lists, return the acc_list result *)
            | list :: rest =>
                case all_except_option(s, list) of
                    NONE => rec_helper(rest, acc_list)
                    | SOME xs =>
                    let
                        fun build_list (list, acc_list): string list =
                            case list of
                                [] => acc_list
                                | x :: xs => build_list(xs, x :: acc_list)
                    in
                        rec_helper(rest, build_list(xs, acc_list)) (* build the acc_list recursively from each item *)
                end
    in
        rec_helper(lists, [])
    end

type fullname = { first: string, middle: string, last: string }
fun similar_names (lists: string list list, fullname: fullname ) =
    case fullname of
        { first = f, middle = m, last = l } =>
            let
                fun make_fullname (xs: string list) = (* helper to build result with record constructs for eacht list item *)
                    case xs of
                        [] => []
                        | x::xs => [{ first=x, middle=m, last=l }] @ make_fullname(xs)
            in
                let val subs = f :: get_substitutions1(lists, f) (* step 1 create a list of substitutions by fullname's first name *)
                in
                    make_fullname(subs)
                end
            end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit, _) =
    case suit of
        Clubs => Black
        | Spades => Black
        | _ => Red

fun card_color2 (Clubs, _) = Black
    | card_color3 (Spades, _) = Black
    | card_color3 _ = Red

fun card_value (_, rank) =
    case rank of
        Num(i) => i
        | Ace => 11
        | _ => 10

fun card_value2 (_, Num(i)) = i
    | card_value2 (_, Ace) = 11
    | card_value2 _ = 10

fun remove_card (cs: card list, c: card, e: exn) =
    let
        fun helper (xs, acc) =
        case xs of
            [] => raise e
            | x::xs' => if (x = c) then acc @ xs'
                        else helper (xs', x::acc)
    in
       helper (cs, [])
    end

fun all_same_color [] = false
    | all_same_color (_::[]) = true
    | all_same_color (card1::(card2::tl)) =
        card_color card1 = card_color card2 andalso all_same_color(card2::tl)
