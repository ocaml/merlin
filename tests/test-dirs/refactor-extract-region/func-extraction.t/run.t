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
      "content": "let is_empty = function | [] -> true | _ -> false
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
      "content": "let fun_name2 acc x = if x > acc then x else acc
  let max l = List.fold_left fun_name2 l",
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
          "col": 17
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 24:2 -end 26:6 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 23,
        "col": 0
      },
      "end": {
        "line": 26,
        "col": 6
      },
      "content": "let fun_name2 () = let var = ref 0 in var := (10 * 50); !var
  let my_function =
    fun_name2 ()",
      "selection-range": {
        "start": {
          "line": 23,
          "col": 4
        },
        "end": {
          "line": 23,
          "col": 16
        }
      }
    },
    "notifications": []
  }
