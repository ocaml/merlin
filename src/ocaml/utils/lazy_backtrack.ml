type ('a,'b) t = ('a,'b) eval ref

and ('a,'b) eval =
  | Done of 'b
  | Raise of exn
  | Thunk of 'a

type undo =
  | Nil
  | Cons : ('a, 'b) t * 'a * undo -> undo

type log = undo ref

let force f x =
  match !x with
  | Done x -> x
  | Raise e -> raise e
  | Thunk e ->
      match f e with
      | y ->
        x := Done y;
        y
      | exception e ->
        x := Raise e;
        raise e

let get_arg x =
  match !x with Thunk a -> Some a | _ -> None

let create x =
  ref (Thunk x)

let create_forced y =
  ref (Done y)

let create_failed e =
  ref (Raise e)

let log () =
  ref Nil

let force_logged log f x =
  let open! Result_compat in (* merlin *)
  match !x with
  | Done x -> x
  | Raise e -> raise e
  | Thunk e ->
    match f e with
    | (Error _ as err : _ result) ->
        x := Done err;
        log := Cons(x, e, !log);
        err
    | Ok _ as res ->
        x := Done res;
        res
    | exception e ->
        x := Raise e;
        raise e

let backtrack log =
  let rec loop = function
    | Nil -> ()
    | Cons(x, e, rest) ->
        x := Thunk e;
        loop rest
  in
  loop !log

(* For compatibility with 4.02 and 4.03 *)

let is_val t = match !t with
  | Done _ -> true
  | Raise _ | Thunk _ -> false

let view t = !t

(* For compatibility with 4.08 and 4.09 *)

let force_logged_408 log f x =
  match !x with
  | Done x -> x
  | Raise e -> raise e | Thunk e ->
    match f e with
    | None ->
        x := Done None;
        log := Cons(x, e, !log);
        None
    | Some _ as y ->
        x := Done y;
        y
    | exception e ->
        x := Raise e;
        raise e
