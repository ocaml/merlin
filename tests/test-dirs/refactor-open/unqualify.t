Works for in-file modules
  $ $MERLIN single refactor-open -action unqualify -position 4:6 <<EOF
  > module M = struct
  >   let u = ()
  > end
  > open M
  > let u = M.u
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
          "col": 11
        },
        "content": "u"
      }
    ],
    "notifications": []
  }

Works for in-file nested modules

  $ $MERLIN single refactor-open -action unqualify -position 6:6 <<EOF
  > module M = struct
  >   module N = struct
  >     let u = ()
  >   end
  > end
  > open M.N
  > let u = M.N.u
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
          "col": 13
        },
        "content": "u"
      }
    ],
    "notifications": []
  }

Works for stdlib modules (stdlib modules differ from other in-file modules because their
full path is different)

  $ $MERLIN single refactor-open -action unqualify -position 1:6 <<EOF
  > open Unix
  > let times = Unix.times ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 12
        },
        "end": {
          "line": 2,
          "col": 22
        },
        "content": "times"
      }
    ],
    "notifications": []
  }

FIXME shouldn't return anything, as nothing to unqualify

  $ $MERLIN single refactor-open -action unqualify -position 1:6 <<EOF
  > open Unix
  > let times = times ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 12
        },
        "end": {
          "line": 2,
          "col": 17
        },
        "content": "times"
      }
    ],
    "notifications": []
  }

FIXME shouldn't return anything, as nothing to unqualify for multiline paths

  $ $MERLIN single refactor-open -action unqualify -position 1:6 <<EOF
  > open Unix
  > let f x = x.
  >             tms_stime
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 12
        },
        "end": {
          "line": 3,
          "col": 21
        },
        "content": "tms_stime"
      }
    ],
    "notifications": []
  }

FIXME shouldn't return anything, as nothing to unqualify for multiline paths

  $ $MERLIN single refactor-open -action unqualify -position 6:6 <<EOF
  > module M = struct
  >   module N = struct
  >     let u = ()
  >   end
  > end
  > open M
  > let u = N.
  > u
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
          "line": 8,
          "col": 1
        },
        "content": "N.u"
      }
    ],
    "notifications": []
  }


