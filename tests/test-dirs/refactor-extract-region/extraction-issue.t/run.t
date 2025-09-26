FIXME: `x` is used instead of `d_x` in the extracted function body!
Should be: (((a + b) + ((c * x) * y)) + z) + d_x

  $ $MERLIN single refactoring-extract-region -start 10:2 -end 10:31 < foo.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 3,
        "col": 0
      },
      "end": {
        "line": 10,
        "col": 31
      },
      "content": "let fun_name1 x y a b c d_x = (((a + b) + ((c * x) * y)) + z) + x
  let complicated_function x y =
    let a = 10 in
    let b = 11 in
    let c = 12 in
    let module D = struct
      let x = 13
    end in
    (fun_name1 x y a b c D.x)",
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

FIXME: the extracted function body is wrong. 
Should be: a + b + c + d_x + x + (m_x + a)

  $ $MERLIN single refactoring-extract-region -start 20:2 -end 20:33 < foo.ml
  {
    "class": "return",
    "value": {
      "start": {
        "line": 12,
        "col": 0
      },
      "end": {
        "line": 20,
        "col": 33
      },
      "content": "let fun_name1 a b c x d_x m_x =
    ((((a + b) + c) + x) + x) + (let open M in x + a)
  let f () =
    let module D = struct
      let x = 42
    end in
    let module M = struct
      let x = 1
    end in
    let a, b, c, x = (1, 2, 3, 4) in
    (fun_name1 a b c x D.x M.x)",
      "selection-range": {
        "start": {
          "line": 12,
          "col": 4
        },
        "end": {
          "line": 12,
          "col": 13
        }
      }
    },
    "notifications": []
  }
