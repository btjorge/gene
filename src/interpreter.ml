open Ast

exception UndeclaredVar of ident
exception UndefinedFunc of ident

module IdentMap = Map.Make (String)

type value = int

type store = value IdentMap.t

type command_result = CReturn of value
                    | CStore of store

let binop_func = function
  | Plus -> (+)
  | Minus -> (-)

let rel_func = function
  | LessEq -> (<=)
  | Equals -> (=)

let rec eval_expr funcs s = function
  | Number n -> n
  | Var id ->
     begin
       try IdentMap.find id s with
       | Not_found -> raise (UndeclaredVar id)
     end
  | FunApply (id, args) ->
     eval_func funcs id (List.map (eval_expr funcs s) args)
  | Binop (op, e1, e2) ->
     let v1 = eval_expr funcs s e1 in
     let v2 = eval_expr funcs s e2 in
     binop_func op v1 v2

and eval_bexpr funcs s = function
  | True -> true
  | False -> false
  | And (be1, be2) -> eval_bexpr funcs s be1 && eval_bexpr funcs s be2
  | Or (be1, be2) -> eval_bexpr funcs s be1 || eval_bexpr funcs s be2
  | Not be -> not (eval_bexpr funcs s be)
  | Rel (r, be1, be2) -> let v1 = eval_expr funcs s be1 in
                         let v2 = eval_expr funcs s be2 in
                         rel_func r v1 v2

and eval_command funcs s = function
  | Assign (id, e) -> CStore (IdentMap.add id (eval_expr funcs s e) s)
  | IfThenElse (be, c1, c2) ->
     if eval_bexpr funcs s be
     then eval_command funcs s c1
     else eval_command funcs s c2
  | AssertEq (e1, e2) ->
     let v1 = eval_expr funcs s e1 in
     let v2 = eval_expr funcs s e2 in

     if v1 = v2
     then Printf.printf "assertion passed: %s = %s\n"
            (Printer.string_of_expr e1)
            (Printer.string_of_expr e2)
     else Printf.printf "assertion failed: %d != %d\n" v1 v2;

     CStore s
  | Seq (c1, c2) ->
     let res = eval_command funcs s c1 in
     begin
       match res with
       | CReturn _ -> res
       | CStore s -> eval_command funcs s c2
     end
  | Return e -> CReturn (eval_expr funcs s e)

and eval_func funcs id args =
  let (fargs, c) = try List.assoc id funcs with
                   | Not_found -> raise (UndefinedFunc id) in
  let s = List.fold_left (fun m (id, v) -> IdentMap.add id v m)
            IdentMap.empty (List.combine fargs args) in
  match eval_command funcs s c with
  | CStore _ -> if id <> "test" then assert false else 0
  | CReturn v -> v

let eval_prog (funcs, (_, c)) =
  eval_command funcs IdentMap.empty c
