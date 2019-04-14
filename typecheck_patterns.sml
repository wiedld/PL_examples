(*
    Example of SML (from online courses).
*)


(**** provided datatypes (contraints) ****)
exception NoAnswer

(* Syntatic *)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

(* Semantics *)
datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


(**** my code ****)

(* definition of the pipeline operator *)
infix !>
fun x !> f = f x


(* (’a -> ’b list option) -> ’a list -> ’b list option *)
fun all_answers f = fn xlist =>
    let
        fun build_acc (x, (acc, none)) = case f x of
            NONE => (acc, true)
            | SOME y => (acc @ y, none)
        val (i, has_none) = List.foldl build_acc ([], false) xlist
    in
        if has_none then NONE else SOME i
    end


fun typecheck_patterns ((clist: (string * string * typ) list), (plist: pattern list)) =
    let
        fun extractor x = x !> valOf !> hd (* SOME [x] -> x *)

        (* In order to work with all_answers,
            helpers have to return typ list options (not typ option). *)

        fun matching_t (t0, t1) = (* (typ * typ) -> typ list option *)
            case (t0, t1) of
                (t, Anything) => SOME [t]
                | (Anything, t) => SOME [t]
                | (UnitT, UnitT) => SOME [UnitT]
                | (IntT, IntT) => SOME [IntT]
                | (TupleT xs, TupleT ys) => if length xs = length ys
                    then let val inner = all_answers matching_t (ListPair.zip(xs,ys))
                        in
                            if isSome inner then SOME [(TupleT (inner !> valOf))] else NONE
                        end
                    else NONE
                | (Datatype x, Datatype y) => if x=y then SOME [Datatype x] else NONE
                | (_,_) => NONE

        fun convert_p_to_t p = (* pattern -> typ list option *)
            case p of
                Wildcard => SOME [Anything]
                | Variable s => SOME [Anything]
                | UnitP => SOME [UnitT]
                | ConstP i => SOME [IntT]
                | TupleP ps => let val inner = all_answers convert_p_to_t ps
                    in
                        if isSome inner
                        then SOME [TupleT (inner !> valOf)]
                        else NONE
                    end
                | ConstructorP (s, p') =>
                    let val constructor = List.find (fn(cId,_,_) => cId = s) clist
                        val innerT = convert_p_to_t p'
                        val type_checked = isSome constructor andalso isSome innerT
                            andalso isSome (
                                matching_t (constructor !> valOf !> #3, innerT !> extractor))
                    in
                        if type_checked
                        then SOME [Datatype (constructor !> valOf !> #2)]
                        else NONE
                    end

        val types = all_answers convert_p_to_t plist
            handle NoAnswer => NONE
    in
        (* go through all the converted typ, narrowed to a common type *)
        if isSome types
        then (List.foldl
            (fn (t, acc) => let val widened_type = matching_t(t, (acc !> valOf))
                in
                    if isSome widened_type then SOME (widened_type !> extractor)
                    else raise NoAnswer
                end)
            (SOME Anything)  (* acc is of type: typ option *)
            (valOf types)  (* [Anything, Anything, UnitT] *)
        ) handle NoAnswer => NONE
        else NONE
    end


(*
    TESTING
*)

(* ("makeInt", "ReturnedDataType", IntT) *)
val clist = [
	("year", "YearType", IntT),
	("location", "LocationType", TupleT [IntT, IntT]),
	("nested", "NestedType", TupleT [IntT, TupleT [IntT, IntT] ])
]
val p1 = TupleP [Variable "x", Variable "y"]
val p2 = TupleP [Wildcard, Wildcard]
val test_a = typecheck_patterns(clist, [p1]) = SOME (TupleT[Anything,Anything])
val test_b = typecheck_patterns(clist, [p2]) = SOME (TupleT[Anything,Anything])

(* nesting *)
val p3 = TupleP [Wildcard, TupleP [Wildcard, Wildcard ]]
val test_c = typecheck_patterns(clist,[p3]) = SOME (TupleT [Anything,TupleT [Anything,Anything]])

(* single constructor, with wildcard matching *)
val p4 = ConstructorP("nested", TupleP[ConstP 4, TupleP[Wildcard, Variable "b"]])
val test_d = typecheck_patterns(clist, [p4]) = SOME (Datatype "NestedType")

(* How handle multiple patterns in plist? *)
(* type narrowing, due to wildcard *)
val test_e = typecheck_patterns(clist, [p2,p3]) = SOME (TupleT[Anything,TupleT[Anything,Anything]])

