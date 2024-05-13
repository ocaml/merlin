Can qualify module located in the same file
  $ $MERLIN single refactor-open -action qualify -position 4:6 <<EOF
  > module M = struct
  >   let u = ()
  > end
  > open M
  > let u = u
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 8
        },
        "end": {
          "line": 5,
          "col": 9
        },
        "content": "M.u"
      }
    ],
    "notifications": []
  }

Can qualify nested modules located in the same file

  $ $MERLIN single refactor-open -action qualify -position 6:6 <<EOF
  > module M = struct
  >   module N = struct
  >     let u = ()
  >   end
  > end
  > open M.N
  > let u = u
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 8
        },
        "end": {
          "line": 7,
          "col": 9
        },
        "content": "M.N.u"
      }
    ],
    "notifications": []
  }

Can qualify a module from an external library

  $ $MERLIN single refactor-open -action qualify -position 1:6 <<EOF
  > open Sys
  > let enable_runtime_warnings = enable_runtime_warnings ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 30
        },
        "end": {
          "line": 2,
          "col": 53
        },
        "content": "Sys.enable_runtime_warnings"
      }
    ],
    "notifications": []
  }

Can qualify nested modules from the same file, including open statements, and 
does not return duplicate edits

  $ $MERLIN single refactor-open -action qualify -position 8:6 <<EOF
  > module L = struct
  >   module M = struct
  >     module N = struct
  >       let u = ()
  >     end
  >   end
  > end
  > open L 
  > open M.N
  > let () = u
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 9,
          "col": 5
        },
        "end": {
          "line": 9,
          "col": 8
        },
        "content": "L.M.N"
      },
      {
        "start": {
          "line": 10,
          "col": 9
        },
        "end": {
          "line": 10,
          "col": 10
        },
        "content": "L.M.N.u"
      }
    ],
    "notifications": []
  }
