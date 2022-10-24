  $ dune build
  $ dune runtest
  $ cat lib1.ml | nl -ba
       1	let boo r = fresh a (r === a)
       2	let boo_expanded r = call_fresh (fun a -> r === a)
2:49 is a location of last `a` in `let boo_expanded r = call_fresh (fun a -> r === a)`
Reported occurences are correct
  $ ocamlmerlin single occurrences -identifier-at 2:49 -filename ./lib1.ml < ./lib1.ml | jq 'del(.timing)'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 37
        },
        "end": {
          "line": 2,
          "col": 38
        }
      },
      {
        "start": {
          "line": 2,
          "col": 48
        },
        "end": {
          "line": 2,
          "col": 49
        }
      }
    ],
    "notifications": []
  }
1.28 is a location of the last `a` in the piece of code being PPX-rewritten
occurences are empty, which is not expected.
Highlight in VsCode + lsp-server is extremly weird, see https://github.com/ocaml/ocaml-lsp/issues/147
  $ ocamlmerlin single occurrences -identifier-at 1:28 -filename ./lib1.ml < ./lib1.ml | jq 'del(.timing)'
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
