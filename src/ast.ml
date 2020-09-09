type ident = string

type binop = Plus | Minus (* for now *)

type rel = LessEq | Equals (* for now *)

type expr = Number of int
          | Var of ident
          | FunApply of ident * expr list
          | Binop of binop * expr * expr

type bexpr = True
           | False
           | And of bexpr * bexpr
           | Or of bexpr * bexpr
           | Not of bexpr
           | Rel of rel * expr * expr

type command = Assign of ident * expr
             | IfThenElse of bexpr * command * command
             | AssertEq of expr * expr
             | Seq of command * command
             | Return of expr

type param_code = ident list * command

type fundef = ident * param_code

type test = param_code

type prog = fundef list * test
