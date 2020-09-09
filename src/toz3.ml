open Ast
open Formula

open Z3
open Arithmetic

type model = (ident * string) list

type result = Satisfiable of model
            | Unsatisfiable
            | Unknown

let ctx = mk_context []

let binop_func op t1 t2 =
  match op with
  | Plus -> mk_add ctx [t1; t2]
  | Minus -> mk_sub ctx [t1; t2]

let rel_func = function
  | LessEq -> mk_le ctx
  | Equals -> Boolean.mk_eq ctx

let rec term = function
  | Number n -> Integer.mk_numeral_i ctx n
  | Var id -> Integer.mk_const_s ctx id
  | Binop (op, t1, t2) -> binop_func op (term t1) (term t2)

let rec formula f =
  let open Boolean in
  match f with
  | True -> mk_true ctx
  | False -> mk_false ctx
  | Rel (r, t1, t2) -> rel_func r (term t1) (term t2)
  | Not f -> mk_not ctx (formula f)
  | And (f1, f2) -> mk_and ctx [formula f1; formula f2]
  | Or (f1, f2) -> mk_or ctx [formula f1; formula f2]
  | Implies (f1, f2) -> mk_implies ctx (formula f1) (formula f2)

let find_model f =
  let get_value m id =
    let id = Integer.mk_const_s ctx id in
    match Model.get_const_interp_e m id with
      | Some v -> Integer.numeral_to_string v
      | None -> "0" in

  let vars = vars f in
  let f = formula f in

  let solver = Solver.mk_simple_solver ctx in

  Solver.add solver [f];

  match Solver.check solver [] with
  | SATISFIABLE ->
     let m = match Solver.get_model solver with
       | Some m -> m
       | None -> assert false in

     Satisfiable (List.map (fun id -> (id, get_value m id)) vars)
  | UNSATISFIABLE -> Unsatisfiable
  | UNKNOWN -> Unknown
