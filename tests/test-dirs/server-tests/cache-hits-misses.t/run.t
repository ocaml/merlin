  $ $MERLIN server stop-server
  $ cp ./a.ml ./a.ml.tmp && mv -f ./a.ml.tmp ./a.ml && chmod +w ./a.ml

1. cold start on a.ml (3 top-level items: x, y, z):

  $ $MERLIN server errors -log-file merlin_logs -filename ./a.ml < ./a.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 0, "miss": 1 }
    },
    "query_num": 0
  }

2. second run on unchanged a.ml:

  $ $MERLIN server errors -log-file merlin_logs -filename ./a.ml < ./a.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 3, "typed": 0 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 1
  }

3. append a 4th item to a.ml:

  $ echo "" >> ./a.ml
  $ echo 'let w = 4' >> ./a.ml
  $ $MERLIN server errors -log-file merlin_logs -filename ./a.ml < ./a.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 3, "typed": 1 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 2
  }

4. overwrite a.ml so the very first item changes:

  $ echo 'let a = 6' > ./a.ml
  $ $MERLIN server errors -log-file merlin_logs -filename ./a.ml < ./a.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 0, "typed": 1 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 3
  }

5. first and second runs on b.ml (which references A.x before a.cmi exists):

  $ $MERLIN server errors -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 8
      },
      "end": {
        "line": 1,
        "col": 9
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound module A"
    }
  ]
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 4
  }

  $ $MERLIN server errors -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value"
  [
    {
      "start": {
        "line": 1,
        "col": 8
      },
      "end": {
        "line": 1,
        "col": 9
      },
      "type": "typer",
      "sub": [],
      "valid": true,
      "message": "Unbound module A"
    }
  ]
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 1, "typed": 0 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 5
  }

6. compile a.ml (creating a.cmi and a.cmt) and query b.ml again:

  $ echo 'let x = 1' > ./a.ml
  $ $OCAMLC -c -bin-annot ./a.ml
  $ $MERLIN server errors -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 1 }
    },
    "query_num": 6
  }

7. locate on A.x in b.ml:

  $ $MERLIN server locate -position 1:10 -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value.pos"
  {
    "line": 1,
    "col": 4
  }
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 1, "typed": 0 },
      "cmt": { "hit": 0, "miss": 1 },
      "cmi": { "hit": 2, "miss": 0 }
    },
    "query_num": 7
  }

  $ $MERLIN server locate -position 1:10 -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value.pos"
  {
    "line": 1,
    "col": 4
  }
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 1, "typed": 0 },
      "cmt": { "hit": 1, "miss": 0 },
      "cmi": { "hit": 2, "miss": 0 }
    },
    "query_num": 8
  }

8. enable USE_PPX_CACHE in .merlin to test reader_phase and ppx_phase caches:

  $ echo "USE_PPX_CACHE" > .merlin
  $ $MERLIN server errors -log-file merlin_logs -filename ./c.ml < ./c.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 9
  }

  $ $MERLIN server errors -log-file merlin_logs -filename ./c.ml < ./c.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": { "reused": 1, "typed": 0 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 10
  }

9. switching files (c.ml -> b.ml -> c.ml -> b.ml) with USE_PPX_CACHE enabled


  $ $MERLIN server errors -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 2, "miss": 0 }
    },
    "query_num": 11
  }

  $ $MERLIN server errors -log-file merlin_logs -filename ./c.ml < ./c.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 1, "typed": 0 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 1, "miss": 0 }
    },
    "query_num": 12
  }

  $ $MERLIN server errors -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value"
  []
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": { "reused": 1, "typed": 0 },
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 2, "miss": 0 }
    },
    "query_num": 13
  }

10. bug: complete-prefix and expand-prefix always report "miss" for reader_phase, ppx_phase, and typer

  $ $MERLIN server complete-prefix -position 1:9 -prefix "A." -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value.entries | length"
  1
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 2, "miss": 0 }
    },
    "query_num": 14
  }

  $ $MERLIN server complete-prefix -position 1:9 -prefix "A." -log-file merlin_logs -filename ./b.ml < ./b.ml | jq ".value.entries | length"
  1
  $ cat merlin_logs | grep -A 8 '"cache":'
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 0 },
      "cmi": { "hit": 2, "miss": 0 }
    },
    "query_num": 15
  }

  $ $MERLIN server stop-server
