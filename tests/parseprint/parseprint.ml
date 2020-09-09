open Genelib

let prefix = "../progs/"

(* In theory all the progs can be parsed *)

let progs = List.sort compare (Array.to_list (Sys.readdir prefix))

let parseprint file =
  let ic = open_in (prefix ^ file) in
  let lexbuf = Lexing.from_channel ic in

  let p = Parser.prog Lexer.token lexbuf in

  Printer.print_prog p

let () =
  List.iter parseprint progs
