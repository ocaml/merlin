`x` is used instead of `d_x` in the extracted function body!
Should be: (x * y) + d_x

  $ $MERLIN single refactoring-extract-region -start 7:2 -end 7:15 < foo.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 7,
        "col": 15
      },
      "content": "let fun_name1 x y d_x = (x * y) + d_x
  let complicated_function x y =
    let module D = struct
      let x = 13
    end in
    (fun_name1 x y D.x)",
      "selection-range": {
        "start": {
          "line": 3,
          "col": 4
        },
        "end": {
          "line": 3,
          "col": 13
        }
      }
    },
    "notifications": []
  }

The extracted function body is wrong. 
Should be: d_x + x + m_x

  $ $MERLIN single refactoring-extract-region -start 17:2 -end 17:16 < foo.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 9,
        "col": 0
      },
      "end": {
        "line": 17,
        "col": 15
      },
      "content": "let fun_name1 x d_x m_x = (d_x + x) + m_x
  let f () =
    let module D = struct
      let x = 42
    end in
    let module M = struct
      let x = 1
    end in
    let x = 10 in
    (fun_name1 x D.x M.x)",
      "selection-range": {
        "start": {
          "line": 9,
          "col": 4
        },
        "end": {
          "line": 9,
          "col": 13
        }
      }
    },
    "notifications": []
  }
