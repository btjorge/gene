open Genelib

let prefix = "../progs/"

let progs = [ (*"max01";*) "max02"; "max03"; "max04"; "max05"; "max07";
              "max08"; "max09"; "max10"; "max11"; "max12"; "max3"; "max_imp";
              "max_imp2" ]

let interpret file =
  let ic = open_in (prefix ^ file) in
  let lexbuf = Lexing.from_channel ic in

  let p = Parser.prog Lexer.token lexbuf in

  let _ = Interpreter.eval_prog p in
  ()

let () =
  List.iter interpret progs
