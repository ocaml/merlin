GADT indexation through aliases

  $ cat > sample.ml <<EOF
  > type _ g =
  >  | A : int g
  >  | B : string g
  >  | C : (int, string) result g
  >  type i = int
  >  type s = string
  >  let f = (A, B, C)
  > EOF
  $ $MERLIN single type-enclosing -position 7:6 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int g * string g * (int, string) result g

  $ $MERLIN single type-enclosing -short-paths -position 7:6 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  i g * s g * (i, s) result g

GADT indexation through aliases with abstraction

  $  cat > sample.ml <<EOF
  > module M : sig
  >  type 'a g
  >  val a : int g
  >  val b : string g
  >  val c : (int, string) result g
  > end = struct
  >   type _ g =
  >    | A : int g
  >    | B : string g
  >    | C : (int, string) result g
  >   let (a, b, c) = (A, B, C)
  > end
  > open M
  > let f = (a, b, c)
  > EOF
  $ $MERLIN single type-enclosing -position 14:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int M.g * string M.g * (int, string) result M.g

  $ $MERLIN single type-enclosing -short-paths -position 14:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int g * string g * (int, string) result g

GADT indexation through aliases with abstraction at call-site

  $ cat > sample.ml <<EOF
  > module M : sig
  >  type 'a g
  >  val a : int g
  >  val b : string g
  >  val c : (int, string) result g
  > end = struct
  >   type _ g =
  >    | A : int g
  >    | B : string g
  >    | C : (int, string) result g
  >   let (a, b, c) = (A, B, C)
  > end
  > type aaa = string type bbb = int type ccc = (int, string) result
  > open M
  > let f = (a, b, c)
  > EOF
  $ $MERLIN single type-enclosing -position 15:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  int M.g * string M.g * (int, string) result M.g

  $ $MERLIN single type-enclosing -short-paths -position 15:5 -filename sample.ml < sample.ml \
  > | tr '\r\n' ' ' \
  > | jq .value[0].type -r
  bbb g * aaa g * (bbb, aaa) result g
