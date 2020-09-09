open Symbinterp
open Formula

(* The logic of the gene tool *)

(* Generate concrete values for arguments so that AssertEquals (e1, e2)
 * succeed (resp fail) *)
(*let generate_concrete p args e1 e2 =
  let funcs = List.map (fun (id, args, c) -> (id, (args, c))) p in
  let s = List.fold_left (fun m id -> IdentMap.add id (Formula.Var id) m)
            IdentMap.empty args in

  let m1 = eval_expr funcs s e1 in
  let m2 = eval_expr funcs s e2 in

  let sv = conjug_ctx (fun v1 v2 -> Formula.Rel (Equals, v1, v2)) m1 m2 in
  let fv = conjug_ctx
             (fun v1 v2 -> Formula.(Not (Rel (Equals, v1, v2)))) m1 m2 in

  let sv = NonDetMonad.compute sv in
  let fv = NonDetMonad.compute fv in

  let sv = List.map (fun ({path; asserts}, f) ->
               Formula.conj (path @ asserts @ [f])) sv in
  let fv = List.map (fun ({path; asserts}, f) ->
               Formula.conj (path @ asserts @ [f])) fv in

  let mapper f =
    match Toz3.find_model f with
    | Satisfiable m -> m
    | _ -> [] in

  let sv = List.map mapper sv in
  let fv = List.map mapper fv in

  (sv, fv)*)

let wrap_and = function
  | (True, f) -> f
  | (f, True) -> f
  | (f, g) -> And (f, g)

(* Generate concrete values for test arguments so that the it succeed
 * (resp fail) *)
let generate_concrete p =
  let exec = eval_prog p in

  (* We might want to generate values for every combination of failing
   * assertions. *)

  let exec = List.map
               (fun ({path; asserts}, _) -> Formula.(conj path, conj asserts))
               exec in

  let sv = List.map (fun (p, a) -> wrap_and (p, a)) exec in
  let fv = List.map (fun (p, a) -> wrap_and (p, Formula.Not a)) exec in

  let mapper f =
    match Toz3.find_model f with
    | Satisfiable m -> Some m
    | Unknown -> failwith "geneLogic: z3 says unknown"
    | Unsatisfiable -> None in

  let sv = List.filter_map mapper sv in
  let fv = List.filter_map mapper fv in

  (sv, fv)

(* Get the list of the test's parameters that are not used in the formula *)
let unused_parameters (params, _) f =
  let params = List.fold_right IdentSet.add params IdentSet.empty in

  let vars = Formula.vars_formula f in

  IdentSet.elements (IdentSet.diff params vars)

let is_generic defs id args =
  let open Depsinterp in

  let mapper (id, n) = {
      concrete = n;
      deps = {arith = Deps.TagSet.singleton id; control = Deps.TagSet.empty};
      symbolic = Var id;
    } in

  let tagged_args = List.map mapper args in

  let (path, {concrete = _; deps; _}) = eval_func defs id tagged_args in

  let path = conj path in

  let control_only = Deps.TagSet.diff deps.control deps.arith in

  let pred id =
    let f = conj [ Not (Rel (Equals, Var "_a", Number (List.assoc id args)));
                   substitute id (Var "_a") path;
                   Not (Rel (Equals, Var "_b", Var "_c"));
                   Not (substitute id (Var "_b") path);
                   Not (substitute id (Var "_c") path) ] in
    match Toz3.find_model f with
    | Satisfiable _ -> true
    | Unknown -> failwith "Don't know what to do"
    | Unsatisfiable -> false in

  not (Deps.TagSet.(is_empty (union deps.control deps.arith)))
  && (Deps.TagSet.(is_empty control_only || exists pred control_only))
