  $ $MERLIN single refactoring-extract-region -start 3:25 -end 3:34 -extract-name pi < const.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 3,
        "col": 50
      },
      "content": "let pi = 3.14159
  let circle_area radius = pi *. (radius ** 2.)",
      "selection-range": {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 6
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 5:24 -end 5:28 -extract-name chunk_size < const.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 5,
        "col": 0
      },
      "end": {
        "line": 7,
        "col": 46
      },
      "content": "let chunk_size = 4096
  let read ?(chunk_size = chunk_size) ic =
    let buf = Bytes.create chunk_size in
    In_channel.input ic buf 0 (Bytes.length buf)",
      "selection-range": {
        "start": {
          "line": 5,
          "col": 4
        },
        "end": {
          "line": 5,
          "col": 14
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 15:18 -end 15:32 < const.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 10,
        "col": 0
      },
      "end": {
        "line": 24,
        "col": 3
      },
      "content": "let const_name2 = 1000000000L
  let my_nested_long_int =
    let o =
      let c =
        let a =
          let m =
            let l = const_name2 in
            l
          in
          m
        in
        a
      in
      c
    in
    o",
      "selection-range": {
        "start": {
          "line": 10,
          "col": 4
        },
        "end": {
          "line": 10,
          "col": 15
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 26:46 -end 26:58 -extract-name header_log < const.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 26,
        "col": 0
      },
      "end": {
        "line": 26,
        "col": 65
      },
      "content": "let header_log = \"CRITICAL: \"
  let log ppf msg = Format.pp_print_string ppf (header_log ^ msg)",
      "selection-range": {
        "start": {
          "line": 26,
          "col": 4
        },
        "end": {
          "line": 26,
          "col": 14
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 32:33 -end 32:36 < const.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 29,
        "col": 0
      },
      "end": {
        "line": 33,
        "col": 6
      },
      "content": "let const_name3 = '@'
  let f () : (module EMPTY) =
    (module struct
      let const_name2 = assert false
      let secret = String.make 100 const_name3
    end)",
      "selection-range": {
        "start": {
          "line": 29,
          "col": 4
        },
        "end": {
          "line": 29,
          "col": 15
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 36:23 -end 40:5 -extract-name my_essay < const.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 35,
        "col": 0
      },
      "end": {
        "line": 41,
        "col": 30
      },
      "content": "let my_essay = {foo|
  multi
  lines
  constant
  |foo}
  let g () =
    let multilines_cst = my_essay in
    print_endline multilines_cst",
      "selection-range": {
        "start": {
          "line": 35,
          "col": 4
        },
        "end": {
          "line": 35,
          "col": 12
        }
      }
    },
    "notifications": []
  }

  $ $MERLIN single refactoring-extract-region -start 1:0 -end 2:0 \
  > -filename foobar.mli <<EOF
  > val f : int -> int
  > EOF
  {
    "class": "error",
    "value": "Expression extraction is only allowed in implementation file",
    "notifications": []
  }
