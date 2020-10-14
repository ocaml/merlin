###############
## SUM TYPES ##
###############

Test 1.1 : option complete
  $ $MERLIN single case-analysis -start 4:9 -end 4:10 -filename refine_pattern.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some _ -> () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 9
        },
        "end": {
          "line": 4,
          "col": 10
        }
      },
      "()"
    ],
    "notifications": []
  }

#############
## RECORDS ##
#############

Test 2.1
  $ cat >typ4.ml <<EOF \
  > type b = C | D of string \
  > type a = A | B of b   \
  > type recd = { a : a } \
  > let f (x : recd) =    \
  >   match x with        \
  >   | { a = A } -> ()   \
  >   | { a = B _ } -> () \
  > EOF

  $ $MERLIN single case-analysis -start 7:12 -end 7:12 -filename typ4.ml <typ4.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 4
        },
        "end": {
          "line": 7,
          "col": 15
        }
      },
      "{ a = B (C) }|{ a = B (D _) }"
    ],
    "notifications": []
  }

##########################
## POLYMORPHIC VARIANTS ##
##########################

##########
## GADT ##
##########

############
## ERRORS ##
############

Test 5.1 : Nothing to do

  $ $MERLIN single case-analysis -start 4:9 -end 4:11 -filename nothing_to_do.ml <<EOF \
  > let _ = \
  >   match (None : unit option) with \
  >   | None -> () \
  >   | Some () -> () \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }
