open Ast

exception UndeclaredVar of ident
exception UndefinedFunc of ident

module IdentMap = Map.Make (String)

module Deps = struct
  type tag = string

  module TagSet = Set.Make (String)

  type t = { arith : TagSet.t; control : TagSet.t; }

  let empty = { arith = TagSet.empty; control = TagSet.empty; }

  let union d1 d2 = {
      arith = TagSet.union d1.arith d2.arith;
      control = TagSet.union d1.control d2.control;
    }

  let add_control tags deps = {
      arith = deps.arith;
      control = TagSet.union deps.control tags;
    }
end

type value = {
    concrete : int;
    deps : Deps.t;
    symbolic : Formula.term;
  }

type bvalue = {
    bconcrete : bool;
    deps : Deps.t;
    bsymbolic : Formula.formula;
  }

type store = value IdentMap.t

type command_result' = CReturn of value
                     | CStore of store

type path = Formula.formula list

type command_result = path * command_result'

let concat_path f (p1, v1) (p2, v2) =
  (p1 @ p2, f v1 v2)

let binop_func op v1 v2 =
  {
    concrete = Interpreter.binop_func op v1.concrete v2.concrete;
    deps = Deps.union v1.deps v2.deps;
    symbolic = Formula.Binop (op, v1.symbolic, v2.symbolic);
  }

let rel_func r v1 v2 =
  {
    bconcrete = Interpreter.rel_func r v1.concrete v2.concrete;
    deps = Deps.union v1.deps v2.deps;
    bsymbolic = Formula.Rel (r, v1.symbolic, v2.symbolic);
  }

let rec eval_expr funcs s = function
  | Number n ->
     ([], {
        concrete = n;
        deps = Deps.empty;
        symbolic =  Formula.Number n;
     })

  | Var id ->
     begin
       try ([], IdentMap.find id s) with
       | Not_found -> raise (UndeclaredVar id)
     end

  | FunApply (id, args) ->
     let folder (p, vl) e =
       let (p', v) = eval_expr funcs s e in
       (p @ p', vl @ [v]) in

     let (p, args) = List.fold_left folder ([], []) args in

     let (p', v) = eval_func funcs id args in

     (p @ p', v)
  | Binop (op, e1, e2) ->
     let (p1, v1) = eval_expr funcs s e1 in
     let (p2, v2) = eval_expr funcs s e2 in
     (p1 @ p2, binop_func op v1 v2)

and eval_bexpr funcs s = function
  | True ->
     ([], {
        bconcrete = true;
        deps = Deps.empty;
        bsymbolic = Formula.True
     })

  | False ->
     ([], {
        bconcrete = false;
        deps = Deps.empty;
        bsymbolic = Formula.False
     })

  | And (be1, be2) ->
     let (p1, bv1) = eval_bexpr funcs s be1 in
     let (p2, bv2) = eval_bexpr funcs s be2 in
     if not bv1.bconcrete then
       (p1, {
          bconcrete = false;
          deps = bv1.deps;
          bsymbolic = Formula.And (bv1.bsymbolic, bv2.bsymbolic);
       })
         (* is Formula.And (s1, s2) really consistent with lazy evaluation ? *)
     else
       (p1 @ p2, {
          bconcrete = bv2.bconcrete;
          deps = Deps.union bv1.deps bv2.deps;
          bsymbolic = Formula.And (bv1.bsymbolic, bv2.bsymbolic);
       })

  | Or (be1, be2) ->
     let (p1, bv1) = eval_bexpr funcs s be1 in
     let (p2, bv2) = eval_bexpr funcs s be2 in
     if bv1.bconcrete then
       (p1, {
          bconcrete = true;
          deps = bv1.deps;
          bsymbolic = Formula.Or (bv1.bsymbolic, bv2.bsymbolic);
       })
     else
       (p1 @ p2, {
          bconcrete = bv2.bconcrete;
          deps = Deps.union bv1.deps bv2.deps;
          bsymbolic = Formula.Or (bv1.bsymbolic, bv2.bsymbolic)
       })

  | Not be ->
     let (p, bv) = eval_bexpr funcs s be in
     (p, { bv with bconcrete = not bv.bconcrete })

  | Rel (r, be1, be2) ->
     let (p1, v1) = eval_expr funcs s be1 in
     let (p2, v2) = eval_expr funcs s be2 in
     (p1 @ p2, rel_func r v1 v2)

and eval_command funcs store ctags = function
  | Assign (id, e) ->
     let (p, v) = eval_expr funcs store e in
     let v = { v with deps = Deps.add_control ctags v.deps } in
     (p, CStore (IdentMap.add id v store))

  | IfThenElse (be, c1, c2) ->
     begin
       let (p, bv) = eval_bexpr funcs store be in
       let ctags =
         Deps.TagSet.(union ctags (union bv.deps.arith bv.deps.control)) in
       match bv.bconcrete with
       | true ->
          let (p', r) = eval_command funcs store ctags c1 in
          (p @ bv.bsymbolic :: p', r)
       | false ->
          let (p', r) = eval_command funcs store ctags c2 in
          (p @ Formula.Not bv.bsymbolic :: p', r)
     end

  | AssertEq (e1, e2) ->
     let (p1, {concrete = i1; _}) = eval_expr funcs store e1 in
     let (p2, {concrete = i2; _}) = eval_expr funcs store e2 in

     if i1 = i2
     then Printf.printf "assertion passed: %s = %s\n"
            (Printer.string_of_expr e1)
            (Printer.string_of_expr e2)
     else Printf.printf "assertion failed: %d != %d\n" i1 i2;

     (p1 @ p2, CStore store)

  | Seq (c1, c2) ->
     let (p, res) = eval_command funcs store ctags c1 in
     begin
       match res with
       | CReturn _ -> (p, res)
       | CStore store ->
          let (p', res') = eval_command funcs store ctags c2 in
          (p @ p', res')
     end

  | Return e ->
     let (p, v) = eval_expr funcs store e in
     (p, CReturn { v with deps = Deps.add_control ctags v.deps })

and eval_func funcs id (args : value list) =
  let (fargs, c) = try List.assoc id funcs with
                   | Not_found -> raise (UndefinedFunc id) in
  let s = List.fold_left (fun m (id, v) -> IdentMap.add id v m)
            IdentMap.empty (List.combine fargs args) in
  match eval_command funcs s Deps.TagSet.empty c with
  | (_, CStore _)-> failwith "do not eval test this way"
  | (p, CReturn v) -> (p, v)

(*let eval_prog (funcs, (_, c)) =
  eval_command funcs IdentMap.empty c*)
