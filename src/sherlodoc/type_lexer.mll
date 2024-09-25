{
  open Type_parser
}

rule token = parse
| ' ' { token lexbuf }
| "->" { ARROW }
| "(" { PARENS_OPEN }
| ")" { PARENS_CLOSE }
| "," { COMMA }
| '_' { WILDCARD }
| '*' { STAR }
| "'" (['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']* as p) { POLY p }
| ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '.']+ as w { WORD w }
| eof { EOF }