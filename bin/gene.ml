open Genelib
open Symbinterp
open Ast
open PPrint

let opt_print_generic = ref false

let opt_print_formula = ref false

let opt_generate_tests = ref false

(* If the test is just one assert, we don't want to print the full definitition
 *)
let print_test ids (args, c) =
  match c with
  | Ast.AssertEq (_, _) -> Printer.print_command ids c; print_newline ()
  | _ -> Printer.print_fundef ids ("test", (args, c))

let check_test (_, c) =
  let number = function
    | Ast.Number n -> n
    | _ -> failwith "not a number" in

  let rec aux = function
    | AssertEq (FunApply (id, args), Number n) ->
       [(id, List.map number args, n)]
    | Seq (c1, c2) -> aux c1 @ aux c2
    | _ -> failwith "unexpected command in test" in

  match aux c with
  | [] -> assert false
  | (id, args, n) :: t ->
     if List.for_all (fun (s, _, _) -> s = id) t
     then (id, (args, n) :: List.map (fun (_, a, n) -> (a, n)) t)
     else failwith "all tests must be on the same function"

let process_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in

  let (defs, t) = Parser.prog Lexer.token lexbuf in

  (*print_endline "Your test:";
  Printer.print_prog [t];*)

  let t' = Generalizer.generalize t in

  let f = Symbinterp.eval_prog (defs, t') in
  let f = List.map (fun ({path; asserts}, _) ->
              Formula.conj (path @ asserts)) f in
  let f = Formula.disj f in

  if !opt_print_generic then begin
      print_endline "Generalized test:";
      Printer.print_fundef [] ("test", t')
    end;

  if !opt_print_formula then begin
      print_endline "Inferred formula:";
      Printer.print_formula f;
      print_newline ()
    end;

  if not (!opt_print_generic || !opt_print_formula) then begin
      let unused = GeneLogic.unused_parameters t' f in

      if unused <> [] then begin
          print_endline
            ("The following test parameters are not used by the code"
             ^ " (see the generic test):");

          let rec printer unused =
            match unused with
            | [] -> assert false
            | [id] -> print_endline id
            | id :: t -> print_string (id ^ ", "); printer t in

          printer unused;

          print_endline "Generalized test:";
          print_test [] t';
        end;
      ()
    end;

  if !opt_generate_tests then begin
      let (sv, fv) = GeneLogic.generate_concrete (defs, t') in

      print_endline "Your test:";
      print_test [] t;

      let iterator v =
        let v = List.map (fun (id, i) -> (id, string i)) v in
        print_test v t' in

      print_endline "Passing tests:";
      List.iter iterator sv;

      print_endline "Failing tests:";
      List.iter iterator fv
    end;

  if not (!opt_print_generic || !opt_print_formula || !opt_generate_tests) then
    begin

      let (id, tests) = check_test t in

      let (fargs, _) = List.assoc id defs in

      let iterator (args, n) =

        Printer.print_command [] (
            AssertEq (FunApply (id, List.map (fun n -> Number n) args),
                      Number n)
          );

        let args = List.combine fargs args in

        if GeneLogic.is_generic defs id args
        then print_endline "  genericity: 1"
        else print_endline "  genericity: 0" in

      List.iter iterator tests
    end;
  ()

let options = [
    ("--print-generic", Arg.Set opt_print_generic, "print the generic test");
    ("--print-formula", Arg.Set opt_print_formula, "print the formula");
    ("--generate-tests", Arg.Set opt_generate_tests, "generate some tests")
  ]

let usage = "usage: " ^ Sys.argv.(0) ^ " [<options>] <files>"

let () =
  Arg.parse options process_file usage
