open Ast
open PPrint

let commabreak = comma ^^ break 1

let ( ++ ) = ( ^/^ )

let indent n d =
  repeat n space ^^ nest n d

let vert l = separate hardline (List.map (fun d -> group d) l)

let binop = function
  | Plus -> string "+"
  | Minus -> string "-"

let rel = function
  | LessEq -> string "<="
  | Equals -> string "="

let varid ids id =
  match List.assoc_opt id ids with
  | Some d -> d
  | None -> string id

let rec expr ids = function
  | Number n -> string (string_of_int n)
  | Var id -> varid ids id
  | FunApply (id, el) ->
     group (string id ++ parens (separate_map commabreak (expr ids) el))
  | Binop (op, e1, e2) ->
     group (parens (expr ids e1 ++ binop op ++ expr ids e2))

let rec bexpr ids = function
  | True -> string "true"
  | False -> string "false"
  | And (be1, be2) ->
     group (parens (bexpr ids be1 ++ string "&&" ++ bexpr ids be2))
  | Or (be1, be2) ->
     group (parens (bexpr ids be1 ++ string "||" ++ bexpr ids be2))
  | Not be -> group (string "not" ++ parens (bexpr ids be))
  | Rel (r, e1, e2) -> group (expr ids e1 ++ rel r ++ expr ids e2)

let rec command ids = function
  | Assign (id, e) -> varid ids id ++ string ":=" ++ expr ids e
  | IfThenElse (be, c1, c2) ->
     vert [
         string "if" ++ parens (bexpr ids be) ++ lbrace;
         indent 2 (command ids c1);
         rbrace ++ string "else" ++ lbrace;
         indent 2 (command ids c2);
         rbrace
       ]
  | AssertEq (e1, e2) ->
     group (string "assertEquals"
            ++ parens (expr ids e1 ^^ commabreak ^^ expr ids e2))
  | Seq (c1, c2) ->
     vert [
         command ids c1 ^^ semi;
         command ids c2
       ]
  | Return e ->
     string "return" ++ expr ids e

let fundef ids (id, (args, c)) =
  vert [
      string id ++ parens (separate_map (commabreak) string args) ++ lbrace;
      indent 2 (command ids c);
      rbrace
    ] ^^ hardline

let prog (funcs, t) =
  separate_map hardline (fundef []) (funcs @ [("test", t)])

let string_of_expr e =
  let b = Buffer.create 10 in
  ToBuffer.pretty 1. 80 b (expr [] e);
  Buffer.contents b

let print_command ids c = ToChannel.pretty 0.9 80 stdout (command ids c)

let print_fundef ids def = ToChannel.pretty 0.9 80 stdout (fundef ids def)

let print_prog p = ToChannel.pretty 0.9 80 stdout (prog p)

open Formula

let rec term = function
  | Number n -> string (string_of_int n)
  | Var id -> string id
  | Binop (op, t1, t2) ->
     group (parens (term t1 ++ binop op ++ term t2))

let rec formula = function
  | True -> string "true"
  | False -> string "false"
  | Rel (r, t1, t2) -> group (term t1 ++ rel r ++ term t2)
  | Not f -> group (string "not" ++ parens (formula f))
  | And (f1, f2) -> group (parens (formula f1 ++ string "&&" ++ formula f2))
  | Or (f1, f2) -> group (parens (formula f1 ++ string "||" ++ formula f2))
  | Implies (f1,f2) -> group (parens (formula f1 ++ string "->" ++ formula f2))

let print_formula f = ToChannel.pretty 0.9 80 stdout (formula f)
