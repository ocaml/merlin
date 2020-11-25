###############
## SUM TYPES ##
###############

Test 1.1 : FIXME: put each case on a different line (if it doesn't require updating
pprintast).

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename variant_exp.ml <<EOF
  > let f (x : int option) =
  >   x
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

  $ $MERLIN single case-analysis -start 2:2 -end 2:3 -filename record_exp.ml <<EOF
  > let f (x : int ref) =
  >   x
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

##########################
## POLYMORPHIC VARIANTS ##
##########################

Test 3.1

  $ cat >typv2.ml <<EOF
  > type basic_color = [ \`Blue | \`Red | \`Yellow ]
  > let f (x : basic_color) =
  >   x
  > EOF

  $ $MERLIN single case-analysis -start 3:2 -end 3:2 -filename typv2.ml <typv2.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      "match x with|`Blue -> (??)|`Yellow -> (??)|`Red -> (??)"
    ],
    "notifications": []
  }

Test 3.1

  $ cat >typv3.ml <<EOF
  > type basic_color = [ \`Blue | \`Red | \`Yellow ]
  > type better_color = [ basic_color | \`Gold ]
  > let f (x : better_color) =
  >   x
  > EOF

  $ $MERLIN single case-analysis -start 4:2 -end 4:2 -filename typv3.ml <typv3.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
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
      "match x with|`Blue -> (??)|`Yellow -> (??)|`Red -> (??)|`Gold -> (??)"
    ],
    "notifications": []
  }

##########
## GADT ##
##########

Test 4.1

  $ cat >typ4b.ml <<EOF
  > type _ t =
  >   | I : int -> int t
  >   | B : bool t
  > let f (x : int t) : unit =
  >   x
  > EOF

  $ $MERLIN single case-analysis -start 5:2 -end 5:2 -filename typ4b.ml <typ4b.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 2
        },
        "end": {
          "line": 5,
          "col": 3
        }
      },
      "(match x with|I _ -> (??))"
    ],
    "notifications": []
  }

Test 4.2

  $ cat >typ4.ml <<EOF
  > type _ term =
  >  | Int : int -> int term
  >  | Add : (int -> int -> int) term
  >  | App : ('b -> 'a) term * 'b term -> 'a term
  > let eval : type a. a term -> a term = fun x ->
  >   x
  > EOF

  $ $MERLIN single case-analysis -start 6:2 -end 6:2 -filename typ4.ml <typ4.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 2
        },
        "end": {
          "line": 6,
          "col": 3
        }
      },
      "match x with|Int _ -> (??)|Add -> (??)|App (_,_) -> (??)"
    ],
    "notifications": []
  }

############
## MODULE ##
############

Test 5.1

  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename unpack_module.ml <<EOF
  > module type S = sig end
  > let g (x : (module S)) =
  >   x
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      "let module M = (val x) in (??)"
    ],
    "notifications": []
  }

Test 5.2 : Module path

  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename module_path.ml <<EOF
  > module T = struct type t = A | B of int end
  > let g (x : T.t) =
  >   x
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 2
        },
        "end": {
          "line": 3,
          "col": 3
        }
      },
      "match x with | T.A -> (??) | T.B _ -> (??)"
    ],
    "notifications": []
  }

test 5.3 : Abstract type


  $ $MERLIN single case-analysis -start 3:2 -end 3:3 -filename module_path.ml <<EOF
  > module T : sig type t end = struct type t = A | B of int end
  > let g (x : T.t) =
  >   x
  > EOF
  {
    "class": "error",
    "value": "Destruct not allowed on non-destructible type: t",
    "notifications": []
  }
