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
      (is_empty )
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
  let max l = List.fold_left (fun_name2 ) l",
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
      "content": "let fun_name3 x y = print_endline (x ^ (y ^ z))
  let test x y =
    let fun_name2 = Fun.id in
    let m =
      let m = (fun_name3 x y) in
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

  $ $MERLIN single refactoring-extract-region -start 30:14 -end 30:24 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 30,
        "col": 0
      },
      "end": {
        "line": 32,
        "col": 10
      },
      "content": "let rec z x = (fun_name2 x)
  
  and y = 80
  and fun_name2 x = (10 + y) + x",
      "selection-range": {
        "start": {
          "line": 33,
          "col": 4
        },
        "end": {
          "line": 33,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 35:2 -end 36:18 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 34,
        "col": 0
      },
      "end": {
        "line": 36,
        "col": 18
      },
      "content": "let fun_name2 () = print_endline \"Wild side effect!\"; [1; 2; 3; 4]
  let f =
    (fun_name2 ())",
      "selection-range": {
        "start": {
          "line": 34,
          "col": 4
        },
        "end": {
          "line": 34,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 40:4 -end 43:7 -extract-name outsider_expr < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 38,
        "col": 0
      },
      "end": {
        "line": 55,
        "col": 5
      },
      "content": "let outsider_expr () = let bar = 20 in object method foo = bar end
  class a =
    let inner_expr =
      (outsider_expr ())
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
          "line": 38,
          "col": 4
        },
        "end": {
          "line": 38,
          "col": 17
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 49:6 -end 56:7 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 38,
        "col": 0
      },
      "end": {
        "line": 55,
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
        (fun_name2 ())
    end",
      "selection-range": {
        "start": {
          "line": 38,
          "col": 4
        },
        "end": {
          "line": 38,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 60:2 -end 62:6 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 59,
        "col": 0
      },
      "end": {
        "line": 62,
        "col": 6
      },
      "content": "let fun_name2 () = let var = ref 0 in var := (y * 50); !var
  let my_mutable_state =
    (fun_name2 ())",
      "selection-range": {
        "start": {
          "line": 59,
          "col": 4
        },
        "end": {
          "line": 59,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 69:14 -end 69:45 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 64,
        "col": 0
      },
      "end": {
        "line": 71,
        "col": 27
      },
      "content": "let fun_name3 x fun_name2 = [(+); (-); fun_name2] @ x
  let func () =
    let x = [] in
    Fun.protect
      (fun () ->
        let fun_name2 = ( / ) in
        let y = (fun_name3 x fun_name2) in
        List.map2 (fun op (a, b) -> op a b) y [ (1, 1); (3, 2); (8, 2) ])
      ~finally:(Fun.const ())",
      "selection-range": {
        "start": {
          "line": 64,
          "col": 4
        },
        "end": {
          "line": 64,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 75:8 -end 75:22 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 73,
        "col": 0
      },
      "end": {
        "line": 81,
        "col": 5
      },
      "content": "let fun_name2 () = [10; 20; 30]
  let rec f = List.map Fun.id
  
  and y = (fun_name2 ())
  
  and z x =
    object
      method x = x
      method y = y
    end",
      "selection-range": {
        "start": {
          "line": 73,
          "col": 8
        },
        "end": {
          "line": 73,
          "col": 17
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 79:15 -end 79:16 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 73,
        "col": 0
      },
      "end": {
        "line": 81,
        "col": 5
      },
      "content": "let fun_name2 x = x
  let rec f = List.map Fun.id
  
  and y = [ 10; 20; 30 ]
  
  and z x =
    object
      method x = (fun_name2 x)
      method y = y
    end",
      "selection-range": {
        "start": {
          "line": 73,
          "col": 8
        },
        "end": {
          "line": 73,
          "col": 17
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 83:12 -end 83:13 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 83,
        "col": 0
      },
      "end": {
        "line": 83,
        "col": 13
      },
      "content": "let const_name1 = 1
  let f = 0 + const_name1",
      "selection-range": {
        "start": {
          "line": 83,
          "col": 4
        },
        "end": {
          "line": 83,
          "col": 15
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 85:10 -end 85:17 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 85,
        "col": 0
      },
      "end": {
        "line": 85,
        "col": 21
      },
      "content": "let fun_name2 x = x * 2
  let f x = (fun_name2 x) + 3",
      "selection-range": {
        "start": {
          "line": 85,
          "col": 4
        },
        "end": {
          "line": 85,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 89:2 -end 89:10 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 87,
        "col": 0
      },
      "end": {
        "line": 89,
        "col": 13
      },
      "content": "let fun_name2 x y = x * y
  let f x =
    let y = 0 in
    (fun_name2 x y) + 3",
      "selection-range": {
        "start": {
          "line": 87,
          "col": 4
        },
        "end": {
          "line": 87,
          "col": 13
        }
      }
    },
    "notifications": []
  }

TODO: This extraction shouldn't be allowed.
  $ $MERLIN single refactoring-extract-region -start 93:2 -end 93:13 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 91,
        "col": 0
      },
      "end": {
        "line": 93,
        "col": 13
      },
      "content": "let fun_name2 () = raise Local
  let f x =
    let exception Local in
    (fun_name2 ())",
      "selection-range": {
        "start": {
          "line": 91,
          "col": 4
        },
        "end": {
          "line": 91,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 96:10 -end 96:16 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 96,
        "col": 0
      },
      "end": {
        "line": 96,
        "col": 15
      },
      "content": "let fun_name2 x = x + 1
  let f x = (fun_name2 x)",
      "selection-range": {
        "start": {
          "line": 96,
          "col": 4
        },
        "end": {
          "line": 96,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 100:10 -end 100:16 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 100,
        "col": 0
      },
      "end": {
        "line": 100,
        "col": 15
      },
      "content": "let fun_name2 x = x + y
  let f x = (fun_name2 x)",
      "selection-range": {
        "start": {
          "line": 100,
          "col": 4
        },
        "end": {
          "line": 100,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 102:10 -end 102:38 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 102,
        "col": 0
      },
      "end": {
        "line": 102,
        "col": 37
      },
      "content": "let fun_name2 x = List.map (fun y -> y + 1) x
  let f x = (fun_name2 x)",
      "selection-range": {
        "start": {
          "line": 102,
          "col": 4
        },
        "end": {
          "line": 102,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 106:2 -end 106:7 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 104,
        "col": 0
      },
      "end": {
        "line": 106,
        "col": 7
      },
      "content": "let fun_name2 y = y + 2
  let f y =
    let y = y + 1 in
    (fun_name2 y)",
      "selection-range": {
        "start": {
          "line": 104,
          "col": 4
        },
        "end": {
          "line": 104,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 108:11 -end 108:16 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 108,
        "col": 0
      },
      "end": {
        "line": 108,
        "col": 16
      },
      "content": "let fun_name2 () = y + 1
  let f () = (fun_name2 ())",
      "selection-range": {
        "start": {
          "line": 108,
          "col": 4
        },
        "end": {
          "line": 108,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 114:2 -end 114:11 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 110,
        "col": 0
      },
      "end": {
        "line": 114,
        "col": 15
      },
      "content": "let fun_name2 x m_y = x * m_y
  let f x =
    let module M = struct
      let y = 0
    end in
    (fun_name2 x M.y) + 3",
      "selection-range": {
        "start": {
          "line": 110,
          "col": 4
        },
        "end": {
          "line": 110,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 119:2 -end 119:18 -extract-name z < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 116,
        "col": 0
      },
      "end": {
        "line": 120,
        "col": 11
      },
      "content": "let z1 x y = x + y
  let f =
    let x = 1 in
    let y = 2 in
    let z = (z1 x y) in
    z + z + 1",
      "selection-range": {
        "start": {
          "line": 116,
          "col": 4
        },
        "end": {
          "line": 116,
          "col": 6
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 129:19 -end 132:71 -extract-name pp_markup < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 125,
        "col": 0
      },
      "end": {
        "line": 133,
        "col": 7
      },
      "content": "let pp_markup bold_tag =
    fun ppf markup ->
      match markup with
      | Text txt -> pp_print_string ppf txt
      | Bold txt -> pp_print_string ppf (bold_tag ^ (txt ^ bold_tag))
  let pp_document ppf doc =
    let open Format in
    let bold_tag = \"**\" in
    fprintf ppf \"%a\"
      (pp_print_list (pp_markup bold_tag))
      doc",
      "selection-range": {
        "start": {
          "line": 125,
          "col": 4
        },
        "end": {
          "line": 125,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 146:2 -end 146:26 -extract-name add < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 138,
        "col": 0
      },
      "end": {
        "line": 146,
        "col": 26
      },
      "content": "let add x m_z m_mm_y = ((x * m_z) * m_mm_y) + A.a
  let f x =
    let module Empty = struct end in
    let module M = struct
      module MM = struct
        let y = 0
      end
      let z = 0
    end in
    (add x M.z M.MM.y)",
      "selection-range": {
        "start": {
          "line": 138,
          "col": 4
        },
        "end": {
          "line": 138,
          "col": 7
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 154:17 -end 154:45 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 153,
        "col": 2
      },
      "end": {
        "line": 157,
        "col": 32
      },
      "content": "let fun_name2 () = List.map on_list [1; 2; 3]
  let x =
      let a_list = (fun_name2 ()) in
      let open Format in
      let printer = pp_print_list pp_print_int in
      printf \"%a\
  \" printer a_list",
      "selection-range": {
        "start": {
          "line": 153,
          "col": 4
        },
        "end": {
          "line": 153,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 154:18 -end 154:44 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 153,
        "col": 2
      },
      "end": {
        "line": 157,
        "col": 32
      },
      "content": "let fun_name2 () = on_list
  let x =
      let a_list = List.map (fun_name2 ()) [ 1; 2; 3 ] in
      let open Format in
      let printer = pp_print_list pp_print_int in
      printf \"%a\
  \" printer a_list",
      "selection-range": {
        "start": {
          "line": 153,
          "col": 4
        },
        "end": {
          "line": 153,
          "col": 13
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 128:2 -end 133:7 < func.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 125,
        "col": 0
      },
      "end": {
        "line": 133,
        "col": 7
      },
      "content": "let fun_name2 ppf doc bold_tag =
    fprintf ppf \"%a\"
      (pp_print_list ?pp_sep:None
         (fun ppf markup ->
            match markup with
            | Text txt -> pp_print_string ppf txt
            | Bold txt -> pp_print_string ppf (bold_tag ^ (txt ^ bold_tag))))
      doc
  let pp_document ppf doc =
    let open Format in
    let bold_tag = \"**\" in
    (fun_name2 ppf doc bold_tag)",
      "selection-range": {
        "start": {
          "line": 125,
          "col": 4
        },
        "end": {
          "line": 125,
          "col": 13
        }
      }
    },
    "notifications": []
  }
