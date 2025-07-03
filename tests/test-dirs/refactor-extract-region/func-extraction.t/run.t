  $ $MERLIN single refactoring-extract-region -start 5:4 -end 7:19 -extract-name is_empty < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 8,
        "col": 5
      },
      "content": "let is_empty = (function | [] -> true | _ -> false)
  let all_empty l =
    List.for_all
      is_empty 
      l",
      "selection-range": {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 12
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 10:20 -end 10:70 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 10,
        "col": 0
      },
      "end": {
        "line": 10,
        "col": 70
      },
      "content": "let fun_name2 = fun acc x -> if x > acc then x else acc
  let max l = List.fold_left fun_name2  l",
      "selection-range": {
        "start": {
          "line": 10,
          "col": 4
        },
        "end": {
          "line": 10,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 18:12 -end 18:37 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 15,
        "col": 0
      },
      "end": {
        "line": 21,
        "col": 3
      },
      "content": "let fun_name3 (x) (y) = print_endline (x ^ (y ^ z))
  let test x y =
    let fun_name2 = Fun.id in
    let m =
      let m = fun_name3 x y in
      m
    in
    m",
      "selection-range": {
        "start": {
          "line": 15,
          "col": 4
        },
        "end": {
          "line": 15,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 24:21 -end 26:37 -extract-name map_aux < func.ml
  "Nothing to do"

  $ $MERLIN single refactoring-extract-region -start 37:14 -end 37:24 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 37,
        "col": 0
      },
      "end": {
        "line": 39,
        "col": 10
      },
      "content": "let rec z x = fun_name2 x
  
  and y = 80
  and fun_name2 (x) = (10 + y) + x",
      "selection-range": {
        "start": {
          "line": 40,
          "col": 4
        },
        "end": {
          "line": 40,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 42:2 -end 43:18 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 41,
        "col": 0
      },
      "end": {
        "line": 43,
        "col": 18
      },
      "content": "let fun_name2 () = print_endline \"Wild side effect!\"; [1; 2; 3; 4]
  let f =
    fun_name2 ()",
      "selection-range": {
        "start": {
          "line": 41,
          "col": 4
        },
        "end": {
          "line": 41,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 47:4 -end 50:7 -extract-name outsider_expr < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 45,
        "col": 0
      },
      "end": {
        "line": 62,
        "col": 5
      },
      "content": "let outsider_expr () = let bar = 20 in object method foo = bar end
  class a =
    let inner_expr =
      outsider_expr ()
    in
    object
      method x = (Fun.const 10) ()
      method y = print_endline
      method z =
        let x =
          object
            method x = \"foobar\"
          end
        in
        x
    end",
      "selection-range": {
        "start": {
          "line": 45,
          "col": 4
        },
        "end": {
          "line": 45,
          "col": 17
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 56:6 -end 61:7 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 45,
        "col": 0
      },
      "end": {
        "line": 62,
        "col": 5
      },
      "content": "let fun_name2 () = let x = object method x = \"foobar\" end in x
  class a =
    let inner_expr =
      let bar = 20 in
      object
        method foo = bar
      end
    in
    object
      method x = (Fun.const 10) ()
      method y = print_endline
      method z =
        fun_name2 ()
    end",
      "selection-range": {
        "start": {
          "line": 45,
          "col": 4
        },
        "end": {
          "line": 45,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 67:2 -end 69:6 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 66,
        "col": 0
      },
      "end": {
        "line": 69,
        "col": 6
      },
      "content": "let fun_name2 () = let var = ref 0 in var := (10 * 50); !var
  let my_mutable_state =
    fun_name2 ()",
      "selection-range": {
        "start": {
          "line": 66,
          "col": 4
        },
        "end": {
          "line": 66,
          "col": 13
        }
      }
    },
    "notifications": []
  }
