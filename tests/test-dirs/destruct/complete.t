###############
## SUM TYPES ##
###############

Test 1.1 : FIXME (void type no Some)

  $ cat >typ5.ml <<EOF
  > type void = |
  > let f (x : void option) =
  >   match x with
  >   | None -> ()
  > EOF

  $ $MERLIN single case-analysis -start 4:4 -end 4:4 -filename typ5.ml <typ5.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 14
        },
        "end": {
          "line": 4,
          "col": 14
        }
      },
      "|Some _ -> (??)"
    ],
    "notifications": []
  }

Test 1.2 : FIXME ?

  $ cat >typ12.ml <<EOF
  > let _ =
  >   match (None : int option option) with
  >   | Some (Some 3) -> ()
  > EOF

  $ $MERLIN single case-analysis -start 3:4 -end 3:8 -filename typ12.ml < typ12.ml \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 23
        },
        "end": {
          "line": 3,
          "col": 23
        }
      },
      "|Some (Some 0) -> (??)|Some (None) -> (??)|None -> (??)"
    ],
    "notifications": []
  }

Test 1.3 : with type constructor

  $ $MERLIN single case-analysis -start 3:5 -end 3:5 -filename funny.ml <<EOF
  > type funny = int option -> unit
  > let v : funny = function
  >   | None -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 14
        },
        "end": {
          "line": 3,
          "col": 14
        }
      },
      "
  | Some _ -> (??)"
    ],
    "notifications": []
  }

#############
## RECORDS ##
#############

Test 2.1

  $ cat >typ.ml <<EOF
  > type a = A | B of string
  > type recd = { a : a }
  > let f (x : recd) =
  >   match x with
  >   | { a = A } -> ()
  > EOF

  $ $MERLIN single case-analysis -start 5:4 -end 5:4 -filename typ.ml < typ.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 19
        },
        "end": {
          "line": 5,
          "col": 19
        }
      },
      "|{ a = B _ } -> (??)"
    ],
    "notifications": []
  }

Test 2.2 : FIXME ?

  $ cat >typ3.ml <<EOF
  > type a = A | B of string
  > type recd = { a : a }
  > let f (x : recd) =
  >   match x with
  >   | { a = A } -> ()
  >   | { a = B _ } -> ()
  > EOF

  $ $MERLIN single case-analysis -start 6:12 -end 6:12 -filename typ3.ml <typ3.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 6,
          "col": 4
        },
        "end": {
          "line": 6,
          "col": 15
        }
      },
      "{ a = B \"\" }|{ a = B _ }"
    ],
    "notifications": []
  }

##########################
## POLYMORPHIC VARIANTS ##
##########################

Test 3.1

  $ cat >typ2.ml <<EOF
  > type basic_color = [ \`Blue | \`Red | \`Yellow ]
  > let f (x : basic_color) =
  >   match x with
  >   | \`Blue -> ()
  > EOF

  $ $MERLIN single case-analysis -start 4:5 -end 4:5 -filename typ2.ml <typ2.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 15
        },
        "end": {
          "line": 4,
          "col": 15
        }
      },
      "|`Yellow|`Red -> (??)"
    ],
    "notifications": []
  }

Test 3.1

  $ cat >typv3.ml <<EOF
  > type basic_color = [ \`Blue | \`Red | \`Yellow ]
  > type better_color = [ basic_color | \`Gold ]
  > let f (x : better_color) =
  >   match x with
  >   | #basic_color -> ()
  > EOF

  $ $MERLIN single case-analysis -start 5:5 -end 5:5 -filename typv3.ml <typv3.ml | \
  > sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 5,
          "col": 22
        },
        "end": {
          "line": 5,
          "col": 22
        }
      },
      "|`Gold -> (??)"
    ],
    "notifications": []
  }

##########
## GADT ##
##########

Test 4.1

  $ cat >typ3.ml <<EOF
  > type _ term =
  >  | Int : int -> int term
  >  | Add : (int -> int -> int) term
  >  | App : ('b -> 'a) term * 'b term -> 'a term
  > let eval : type a. a term -> unit =
  >   fun (x : a term) -> match x with
  >   | Int _ -> ()
  >   | Add -> ()
  > EOF

  $ $MERLIN single case-analysis -start 8:4 -end 8:4 -filename typ3.ml <typ3.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 13
        },
        "end": {
          "line": 8,
          "col": 13
        }
      },
      "|App (_,_) -> (??)"
    ],
    "notifications": []
  }

Test 4.2

  $ cat >typ4.ml <<EOF
  > type _ term =
  >  | Int : int -> int term
  >  | Add : (int -> int -> int) term
  >  | App : ('b -> 'a) term * 'b term -> 'a term
  > let eval (type a) : a term -> unit =
  >   function
  >   | Int _ -> ()
  >   | Add -> ()
  > EOF

  $ $MERLIN single case-analysis -start 8:4 -end 8:4 -filename typ4.ml <typ4.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 13
        },
        "end": {
          "line": 8,
          "col": 13
        }
      },
      "|App (_,_) -> (??)"
    ],
    "notifications": []
  }

Test 4.3 : this match IS exhaustive

  $ cat >typ4b.ml <<EOF
  > type _ t =
  >   | I : int t
  >   | B : bool t
  > let f : int t -> unit =
  >   function
  >   | I -> ()
  > EOF

  $ $MERLIN single case-analysis -start 6:4 -end 6:4 -filename typ4b.ml <typ4b.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }

############
## MODULE ##
############

Test 5.1 : Module path

  $ $MERLIN single case-analysis -start 4:4 -end 4:4 -filename module_path.ml <<EOF
  > module T = struct type t = A | B of int end
  > let g x =
  >   match x with
  >   | T.A -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 4,
          "col": 13
        },
        "end": {
          "line": 4,
          "col": 13
        }
      },
      "
  | T.B _ -> (??)"
    ],
    "notifications": []
  }


Test 5.1 : Module path (with function)

  $ $MERLIN single case-analysis -start 3:4 -end 3:4 -filename module_path.ml <<EOF
  > module T = struct type t = A | B of int end
  > let g = function
  >   | T.A -> ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 13
        },
        "end": {
          "line": 3,
          "col": 13
        }
      },
      "
  | T.B _ -> (??)"
    ],
    "notifications": []
  }
