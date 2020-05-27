open Lib
let x = sqrt 3.

(* ocamlmerlin single errors -filename main.ml < main.ml | jq *)

(* C'est rigolo, l'erreur "leak"
```
Error (alert deprecated): Lib.sqrt
I am deprecated
{
  "class": "return",
  "value": [],
  "notifications": [],
  "timing": {
    "clock": 4,
    "cpu": 0,
    "query": 0,
    "pp": 0,
    "reader": 0,
    "ppx": 0,
    "typer": 0,
    "error": 0
  }
}
``` *)