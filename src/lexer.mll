{
  open Parser

  exception UnexpectedChar of char
}

let alpha = [ 'a' - 'z' 'A' - 'Z' ]
let digit = [ '0' - '9' ]

let number = digit *
let ident = alpha (alpha | digit) *

rule token = parse
| [ ' ' '\n' '\r' '\t' ]  { token lexbuf }
| "(*"                    { comment 1 lexbuf }
| '('                     { LPAREN }
| ')'                     { RPAREN }
| '{'                     { LBRACE }
| '}'                     { RBRACE }
| ','                     { COMMA  }
| ';'                     { SEMICOLON }
| ":="                    { ASSIGN }
| '+'                     { PLUS }
| '-'                     { MINUS }
| '*'                     { TIMES }
| '/'                     { DIV }
| "true"                  { TRUE }
| "false"                 { FALSE }
| "&&"                    { AND }
| "||"                    { OR }
| "not"                   { NOT }
| "<="                    { LESSEQ }
| "<"                     { LESS }
| ">="                    { GREATEQ }
| ">"                     { GREAT }
| "=="                    { EQUALS }
| "!="                    { DIFF }
| "if"                    { IF }
| "else"                  { ELSE }
| "assertEquals"          { ASSERTEQ }
| "return"                { RETURN }
| "test"                  { TEST }
| number as n             { NUMBER (int_of_string n) }
| ident as id             { IDENT id }
| eof                     { EOF }
| _ as c                  { raise (UnexpectedChar c) }

and comment i = parse
| "(*"  { comment (i + 1) lexbuf }
| "*)"  { if i = 1 then token lexbuf else comment (i - 1) lexbuf }
| _     { comment i lexbuf }
