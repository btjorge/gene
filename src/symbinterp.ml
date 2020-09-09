open Ast
open NonDetMonad

exception UndeclaredVar of ident
exception UndefinedFunc of ident

module IdentMap = Map.Make (String)

type value = Formula.term

type store = value IdentMap.t

type path = Formula.formula list

type ctx = { path : path; asserts : Formula.formula list }

let empty = {path = []; asserts = []}

type command_result' = CReturn of value
                     | CStore of store

type command_result = (ctx * command_result') NonDetMonad.t

let binop_func op x y = Formula.Binop (op, x, y)

let rel_func r x y = Formula.Rel (r, x, y)

let fmap_ctx f = fmap (fun (c, v) -> (c, f v))

let merge_ctx {path = p1; asserts = a1} {path = p2; asserts = a2} =
  {path = p1 @ p2; asserts = a1 @ a2}

let conjug_ctx f =
  conjug (fun (c1, v1) (c2, v2) -> (merge_ctx c1 c2, f v1 v2))

let add_path bv m =
  fmap (fun ({path; asserts}, r) -> ({path = bv :: path; asserts}, r)) m

let add_assert a m =
  fmap (fun ({path; asserts}, v) -> ({path; asserts = a :: asserts}, v)) m

let concat_ctx {path = p; asserts = a} m =
  fmap (fun ({path; asserts}, r) ->
      ({path = p @ path; asserts = a @ asserts}, r)) m

(*let printer_path p =
  let open PPrint in
  let open Printer in
  List.fold_left (fun acc f -> acc ++ Printer.formula f ^^ string ";")
    PPrint.empty p

let print_path p =
  PPrint.ToChannel.pretty 1. 80 stdout (printer_path p)*)

let rec eval_expr funcs s = function
  | Number n -> return (empty, Formula.Number n)
  | Var id ->
     begin
       try return (empty, IdentMap.find id s) with
       | Not_found -> raise (UndeclaredVar id)
     end
  | FunApply (id, args) ->
     let margs = List.map (eval_expr funcs s) args in
     let margs = List.fold_left
                   (conjug_ctx (fun va v -> va @ [v]))
                   (return (empty, [])) margs in
     margs >>= (fun (ctx, args) -> concat_ctx ctx (eval_func funcs id args))
  | Binop (op, e1, e2) ->
     let m1 = eval_expr funcs s e1 in
     let m2 = eval_expr funcs s e2 in
     conjug_ctx (fun v1 v2 -> binop_func op v1 v2) m1 m2

and eval_bexpr funcs s = function
  | True -> return (empty, Formula.True)
  | False -> return (empty, Formula.False)
  | And (be1, be2) ->
     let m1 = eval_bexpr funcs s be1 in
     let m2 = eval_bexpr funcs s be2 in
     conjug_ctx (fun v1 v2 -> Formula.And (v1, v2)) m1 m2
  | Or (be1, be2) ->
     let m1 = eval_bexpr funcs s be1 in
     let m2 = eval_bexpr funcs s be2 in
     conjug_ctx (fun v1 v2 -> Formula.Or (v1, v2)) m1 m2
  | Not be -> fmap (fun (p, v) -> (p, Formula.Not v)) (eval_bexpr funcs s be)
  | Rel (r, be1, be2) ->
     let m1 = eval_expr funcs s be1 in
     let m2 = eval_expr funcs s be2 in
     conjug_ctx (fun v1 v2 -> rel_func r v1 v2) m1 m2

and eval_command funcs s = function
  | Assign (id, e) ->
     let m = eval_expr funcs s e in
     fmap_ctx (fun v -> CStore (IdentMap.add id v s)) m

  | IfThenElse (be, c1, c2) ->
     let m0 = eval_bexpr funcs s be in
     let m1 = eval_command funcs s c1 in
     let m2 = eval_command funcs s c2 in

     m0 >>= (fun (ctx, bv) ->
       let m1 = add_path bv (concat_ctx ctx m1) in
       let m2 = add_path (Formula.Not bv) (concat_ctx ctx m2) in

       merge m1 m2)

  | AssertEq (e1, e2) ->
     let m1 = eval_expr funcs s e1 in
     let m2 = eval_expr funcs s e2 in

     conjug (fun (c1, v1) (c2, v2) ->
         (merge_ctx ({path = []; asserts = [Formula.Rel (Equals, v1, v2)]})
            (merge_ctx c1 c2), CStore s)) m1 m2


  | Seq (c1, c2) ->
     let m = eval_command funcs s c1 in

     m >>= (fun (ctx, res) ->
       match res with
       | CReturn _ -> return (ctx, res)
       | CStore s -> concat_ctx ctx (eval_command funcs s c2))

  | Return e ->
     let m = eval_expr funcs s e in
     fmap_ctx (fun v -> CReturn v) m

and eval_func funcs id (args : value list) : (ctx * value) t =
  let (fargs, c) = try List.assoc id funcs with
                   | Not_found -> raise (UndefinedFunc id) in
  let s = List.fold_left (fun m (id, v) -> IdentMap.add id v m)
            IdentMap.empty (List.combine fargs args) in
  let m = eval_command funcs s c in

  fmap_ctx (function
      | CStore _ -> if id <> "test" then assert false else Formula.Number 0
      | CReturn v -> v) m

let eval_prog (funcs, ((args, _) as t)) =
  let args = List.map (fun id -> Formula.Var id) args in
  compute (eval_func (funcs @ [("test", t)]) "test" args)
