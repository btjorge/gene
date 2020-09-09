open Ast

let fresh_ident =
  let i = ref (-1) in
  fun () -> i := !i + 1; "_x" ^ string_of_int !i

let rec generalize_expr = function
  | Number _ -> let id = fresh_ident () in ([id], Var id)
  | Var id -> ([], Var id)
  | FunApply (id, el) ->
     let folder (vl, el) e =
       let (vl', e) = generalize_expr e in (vl @ vl', el @ [e]) in

     let (vl, el) = List.fold_left folder ([], []) el in
     (vl, FunApply (id, el))
  | Binop (op, e1, e2) ->
     let (vl1, e1) = generalize_expr e1 in
     let (vl2, e2) = generalize_expr e2 in
     (vl1 @ vl2, Binop (op, e1, e2))

let rec generalize_bexpr = function
  | True -> ([], True)
  | False -> ([], False)
  | And (be1, be2) ->
     let (vl1, be1) = generalize_bexpr be1 in
     let (vl2, be2) = generalize_bexpr be2 in
     (vl1 @ vl2, And (be1, be2))
  | Or (be1, be2) ->
     let (vl1, be1) = generalize_bexpr be1 in
     let (vl2, be2) = generalize_bexpr be2 in
     (vl1 @ vl2, Or (be1, be2))
  | Not be ->
     let (vl, be) = generalize_bexpr be in
     (vl, Not be)
  | Rel (r, e1, e2) ->
     let (vl1, e1) = generalize_expr e1 in
     let (vl2, e2) = generalize_expr e2 in
     (vl1 @ vl2, Rel (r, e1, e2))

let rec generalize_command = function
  | Assign (id, e) ->
     let (vl, e) = generalize_expr e in
     (vl, Assign (id, e))
  | IfThenElse (be, c1, c2) ->
     let (vl, be) = generalize_bexpr be in
     let (vl1, c1) = generalize_command c1 in
     let (vl2, c2) = generalize_command c2 in
     (vl @ vl1 @ vl2, IfThenElse (be, c1, c2))
  | AssertEq (e1, e2) ->
     let (vl1, e1) = generalize_expr e1 in
     let (vl2, e2) = generalize_expr e2 in
     (vl1 @ vl2, AssertEq (e1, e2))
  | Seq (c1, c2) ->
     let (vl1, c1) = generalize_command c1 in
     let (vl2, c2) = generalize_command c2 in
     (vl1 @ vl2, Seq (c1, c2))
  | Return e ->
     let (vl, e) = generalize_expr e in
     (vl, Return e)

let generalize (args, c) =
  let (vl, c) = generalize_command c in
  (args @ vl, c)

(*let rec specialize_expr vals = function
  | Number n -> Number n
  | Var id ->
     begin
       match List.assoc_opt id vals with
       | Some n -> Number n
       | None -> Var id
     end
  | FunApply (id, el) ->
     FunApply (id, List.map (specialize_expr vals) el)
  | Binop (op, e1, e2) ->
     Binop (op, specialize_expr vals e1, specialize_expr vals e2)

let rec specialize_bexpr vals = function
  | True -> True
  | False -> False
  | And (be1, be2) ->
     And (specialize_bexpr vals be1, specialize_bexpr vals be2)
  | Or (be1, be2) ->
     Or (specialize_bexpr vals be1, specialize_bexpr vals be2)
  | Not be ->
     Not (specialize_bexpr vals be)
  | Rel (r, e1, e2) ->
     Rel (r, specialize_expr vals e1, specialize_expr vals e2)

let rec specialize_command vals = function
  | Assign (id, e) ->
     Assign (id, specialize_expr vals e)
  | IfThenElse (be, c1, c2) ->
     IfThenElse (specialize_bexpr vals be, specialize_command vals c1,
                 specialize_command vals c2)
  | AssertEq (e1, e2) ->
     AssertEq (specialize_expr vals e1, specialize_expr vals e2)
  | Seq (c1, c2) ->
     Seq (specialize_command vals c1, specialize_command vals c2)
  | Return e ->
     Return (specialize_expr vals e)*)
