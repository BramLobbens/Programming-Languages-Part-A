(* 1-4 *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail ({grade=SOME(i), id=_}) =
        if i >= 75 then pass else fail
    | pass_or_fail _ = fail

fun has_passed grade = pass_or_fail grade = pass

fun number_passed (final_grades) =
    let
        fun helper ([], acc) = acc
            | helper (x::xs, acc) =
                let val r = if (has_passed x) then acc + 1 else acc
                in
                    helper (xs, r)
                end
    in
        helper (final_grades, 0)
    end

fun number_misgraded (list) =
    let
        fun helper ([], acc) = acc
            | helper ((result, grade)::tl, acc) =
                case (result, pass_or_fail grade) of
                    (pass, pass) => helper (tl, acc)
                    | (fail, fail) => helper (tl, acc)
                    | (_, _) => helper (tl, acc + 1)
    in
        helper (list, 0)
    end


(* 5-7 *)
datatype 'a tree = leaf
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height tree =
    let fun help (leaf, acc) = acc
            | help (node { value, left, right }, acc) =
                1 + Int.max (help (left, acc), help (right, acc))
    in
        help (tree, 0)
    end

(* fun tree_height leaf = 0
    | tree_height (node { value, left, right }) =
        1 + Int.max (tree_height left, tree_height right) *)

fun sum_tree (tree: int tree) =
    let fun help (leaf, acc) = acc
        | help (node { value, left, right }, acc) =
            help (left, help (right, acc + value))
    in
        help (tree, 0)
    end

fun gardener (tree: flag tree) =
    case tree of
        leaf => leaf
        | node { value, left, right } =>
            case value of
                prune_me => leaf
                | leave_me_alone => node {
                    value=leave_me_alone,
                    left=gardener left,
                    right=gardener right
                    }

fun gardener2 leaf = leaf
    | gardener2 (node { value = prune_me, left, right }) = leaf
    | gardener2 (node { value = leave_me_alone, left, right  }) =
        node {
            value = leave_me_alone,
            left = gardener2 left,
            right = gardener2 right
        }