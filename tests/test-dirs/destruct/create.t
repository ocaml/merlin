###############
## SUM TYPES ##
###############

Test 1.1 : FIXME: put each case on a different line (if it doesn't require updating
pprintast).

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename variant_exp.ml <<EOF \
  > let f (x : int option) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 3
        }
      },
      "match x with | None -> (??) | Some _ -> (??)"
    ],
    "notifications": []
  }

#############
## RECORDS ##
#############

Test 2.1
  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename record_exp.ml <<EOF \
  > let f (x : int ref) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 2
        },
        "end": {
          "line": 2,
          "col": 3
        }
      },
      "match x with | { contents } -> (??)"
    ],
    "notifications": []
  }

############
## MODULE ##
############

Test 6.1
  $ $MERLIN single case-analysis -start 4:2 -end 4:3 -filename unpack_module.ml <<EOF \
  > module type S = sig end \
  >  \
  > let g (x : (module S)) = \
  >   x \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 2
        },
        "end": {
          "line": 4,
          "col": 3
        }
      },
      "let module M = (val x) in (??)"
    ],
    "notifications": []
  }
