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
