Make sure type-enclosing works properly even when the precise location is not
accessible:

  $ $MERLIN single type-enclosing -position 3:7 -filename hide.ml <<EOF
  > module M = struct
  >   include struct
  >     let x = 3
  >   end[@merlin.hide]
  > end
  > EOF
  {
    "class": "failure",
    "value": "hd",
    "notifications": []
  }
