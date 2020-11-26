Test 1

  $ echo "let () = ()" | $MERLIN single case-analysis -start 1:4 -end 1:4 -filename stacktrace.ml | grep -E -v "Raised|Called|Re-raised"
  {
    "class": "error",
    "value": "Destruct not allowed on value_binding",
    "notifications": []
  }

Test 2

  $ $MERLIN single case-analysis -start 4:2 -end 4:1 -filename nonode.ml <<EOF | grep -B 1 Query_commands.No_nodes
  > let f (x : int option) =
  >   match w with
  >  | _ -> ()
  > EOF
    "class": "exception",
    "value": "Query_commands.No_nodes

Test 3

  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml <<EOF
  > let _ =
  >   match (None : int option) with
  >   | exception _ -> ()
  >   | Some 3 -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      " | Some 0 -> (??)
  | None -> (??)"
    ],
    "notifications": []
  }

Test 4

  $ $MERLIN single case-analysis -start 4:4 -end 4:8 -filename complete.ml <<EOF
  > let _ =
  >   match (None : int option) with
  >   | exception _ -> ()
  >   | Some _ -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      "
  | None -> (??)"
    ],
    "notifications": []
  }

Test 5

  $ $MERLIN single case-analysis -start 4:5 -end 4:5 -filename no_comp_pat.ml <<EOF
  > let _ =
  >   match (None : unit option) with
  >   | exception _ -> ()
  >   | None -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 14
        },
        "end": {
          "line": 4,
          "col": 14
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

Test 6
FIXME: `Some 0` certainly is a missing case but we can do better:

  $ $MERLIN single case-analysis -start 4:4 -end 4:8 -filename complete.ml <<EOF
  > let _ =
  >   match (None : int option) with
  >   | exception _ -> ()
  >   | Some 3 -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 16
        }
      },
      " | Some 0 -> (??)
  | None -> (??)"
    ],
    "notifications": []
  }

Test 7
Same two tests but with the exception pattern at the end

  $ $MERLIN single case-analysis -start 4:9 -end 4:11 -filename no_comp_pat.ml <<EOF
  > let _ =
  >   match (None : unit option) with
  >   | None -> ()
  >   | exception _ -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 21
        },
        "end": {
          "line": 4,
          "col": 21
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

Test 8
FIXME: `Some 0` certainly is a missing case but we can do better

  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename complete.ml <<EOF
  > let _ =
  >   match (None : int option) with
  >   | Some 3 -> ()
  >   | exception _ -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 21
        },
        "end": {
          "line": 4,
          "col": 21
        }
      },
      " | Some 0 -> (??)
  | None -> (??)"
    ],
    "notifications": []
  }

Test 9
Tests with exception in or-pattern

  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename exp_or.ml <<EOF
  > let _ =
  >   match (None : unit option) with
  >   | None | exception _ -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 28
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

Test 10

  $ $MERLIN single case-analysis -start 3:11 -end 3:11 -filename exp_or.ml <<EOF
  > let _ =
  >   match (None : unit option) with
  >   | None | exception _ -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 28
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

Test 11

  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename exp_or.ml <<EOF
  > let _ =
  >   match (None : unit option) with
  >   | exception _ | None -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 28
        },
        "end": {
          "line": 3,
          "col": 28
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

Test 12

  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename exp_or.ml <<EOF
  > let _ =
  >   match (None : unit option) with
  >   | exception Not_found | None | exception _ -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 50
        },
        "end": {
          "line": 3,
          "col": 50
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }
