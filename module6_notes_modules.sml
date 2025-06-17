(**
Modules
**)
(* main purpose: hiding implementation details *)
signature MYLIB =
sig
    val my_fun : int -> int
    val foo : string -> string (* module will not compile *)
end

structure MyLib :> MYLIB = struct (* if given a signature MYLIB via :>,
                                    will only type check if type definition
                                   from signature is included *)
    fun my_fun x = x + 1
    fun doubler x = x * 2 (* Any binding not defined in the signature cannot be used outside the module *)
    val bar = "bar"
end