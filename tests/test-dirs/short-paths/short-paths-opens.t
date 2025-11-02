# Alias on existing types (Result) without opening

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > module M = struct
  >   type ('value, 'error) t = ('value, 'error) result =
  >     | Ok of 'value
  >     | Error of 'error
  > end
  > let f = function
  >   | Ok x -> x
  >   | Error _ -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 6:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  (unit, 'a) result -> unit

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module M = struct
  >   type ('value, 'error) t = ('value, 'error) result =
  >     | Ok of 'value
  >     | Error of 'error
  > end
  > let f = function
  >   | Ok x -> x
  >   | Error _ -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 6:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  (unit, 'a) result -> unit

# Alias on existing types (Result) with opening

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > module M = struct
  >   type ('value, 'error) t = ('value, 'error) result =
  >     | Ok of 'value
  >     | Error of 'error
  > end
  > open M
  > let f = function
  >   | Ok x -> x
  >   | Error _ -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 7:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  (unit, 'a) M.t -> unit

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module M = struct
  >   type ('value, 'error) t = ('value, 'error) result =
  >     | Ok of 'value
  >     | Error of 'error
  > end
  > open M
  > let f = function
  >   | Ok x -> x
  >   | Error _ -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 7:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  (unit, 'a) t -> unit

# Alias on existing types (Result) with opening (shadow)

  $ echo "" > .merlin
  > cat > sample.ml <<EOF
  > module M = struct
  >   type ('value, 'error) t = ('value, 'error) result =
  >     | Ok of 'value
  >     | Error of 'error
  > end
  > open! M
  > let f = function
  >   | Ok x -> x
  >   | Error _ -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 7:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  (unit, 'a) M.t -> unit

  $ echo "FLG -short-paths" > .merlin
  > cat > sample.ml <<EOF
  > module M = struct
  >   type ('value, 'error) t = ('value, 'error) result =
  >     | Ok of 'value
  >     | Error of 'error
  > end
  > open! M
  > let f = function
  >   | Ok x -> x
  >   | Error _ -> ()
  > EOF
  $ $MERLIN single type-enclosing -position 7:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  (unit, 'a) t -> unit
