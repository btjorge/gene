open Ast

type term = Number of int
          | Var of ident
          | Binop of binop * term * term

type formula = True
             | False
             | Rel of rel * term * term
             | Not of formula
             | And of formula * formula
             | Or of formula * formula
             | Implies of formula * formula

let rec conj = function
  | [] -> True
  | [e] -> e
  | h :: t -> And (h, conj t)

let rec disj = function
  | [] -> False
  | [e] -> e
  | h :: t -> Or (h, disj t)

module IdentSet = Set.Make (String)

let rec vars_term = function
  | Number _ -> IdentSet.empty
  | Var id -> IdentSet.singleton id
  | Binop (_, t1, t2) -> IdentSet.union (vars_term t1) (vars_term t2)

let rec vars_formula = function
  | True | False -> IdentSet.empty
  | Rel (_, t1, t2) -> IdentSet.union (vars_term t1) (vars_term t2)
  | Not f -> vars_formula f
  | And (f1, f2) | Or (f1, f2) | Implies (f1, f2) ->
     IdentSet.union (vars_formula f1) (vars_formula f2)

let vars f = IdentSet.elements (vars_formula f)

let rec substitute_term v t = function
  | Number n -> Number n
  | Var id -> if id = v then t else Var id
  | Binop (op, t1, t2) ->
     Binop (op, substitute_term v t t1, substitute_term v t t2)

let rec substitute v t = function
  | True -> True
  | False -> False
  | Rel (r, t1, t2) -> Rel (r, substitute_term v t t1, substitute_term v t t2)
  | Not f -> Not (substitute v t f)
  | And (f1, f2) -> And (substitute v t f1, substitute v t f2)
  | Or (f1, f2) -> Or (substitute v t f1, substitute v t f2)
  | Implies (f1, f2) -> Implies (substitute v t f1, substitute v t f2)

let value_binop = function
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Div -> ( / )

let satisfies_rel = function
  | LessEq -> ( <= )
  | Less -> ( < )
  | GreatEq -> ( >= )
  | Great -> ( > )
  | Equals -> ( = )
  | Diff -> ( <> )

let rec value_term v = function
  | Number n -> n
  | Var id -> List.assoc id v
  | Binop (op, t1, t2) -> value_binop op (value_term v t1) (value_term v t2)

let rec satisfies_formula v = function
  | True -> true
  | False -> false
  | Rel (r, t1, t2) -> satisfies_rel r (value_term v t1) (value_term v t2)
  | Not f -> not (satisfies_formula v f)
  | And (f1, f2) -> satisfies_formula v f1 && satisfies_formula v f2
  | Or (f1, f2) -> satisfies_formula v f1 || satisfies_formula v f2
  | Implies (f1, f2) -> not (satisfies_formula v f1) || satisfies_formula v f2
