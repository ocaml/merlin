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

$ $MERLIN single refactoring-extract-region -start 24:15 -end 26:37 -extract-name map_aux < func.ml
{
"class": "return",
"value": {
"start": {
"line": 23,
"col": 0
},
"end": {
"line": 28,
"col": 9
},
"content": "let map_aux acc =
function | [] -> List.rev acc | x::xs -> loop ((f x) :: acc) xs
let map f =
let rec loop map_aux
in
loop []",
"selection-range": {
"start": {
"line": 23,
"col": 4
},
"end": {
"line": 23,
"col": 11
}
}
},
"notifications": []
}

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
  
  and fun_name2 (x) = (10 + y) + x
  ",
      "selection-range": {
        "start": {
          "line": 37,
          "col": 4
        },
        "end": {
          "line": 37,
          "col": 13
        }
      }
    },
    "notifications": []
  }
