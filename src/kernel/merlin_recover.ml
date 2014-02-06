open Std
open Raw_parser

type kind =
  | Struct
  | Struct_item

module Point = struct
  (* The preceding parser, the one we can potentially recover from *)
  type t = (kind * exn) list

  type st = Merlin_parser.t * exn * t option ref

  let drop_item = function
    | (Struct_item, _) :: t | t ->  t

  let empty _ = []

  let frame (parser, payload, _) f t =
    match Merlin_parser.value f with
    | Nonterminal (NT'recover_structure_ ()) ->
      (Struct, payload) :: t
    | Nonterminal (NT'recover_structure_item_ ()) ->
      (Struct_item, payload) :: (drop_item t)
    | _ -> t

  let delta st f t ~old:_ = t (*frame st f t*)

  let validate _ _ = true

  let evict (_, _, rt) t =
    match !rt with
    | Some t' when t == t' -> rt := None
    | _ -> ()
end


module Points = Merlin_parser.Integrate(Point)

module Rollback = struct
  type t = {
    parser: Merlin_parser.t;
    points: Points.t;
  }
  exception T of t
  
  let fresh parser = {parser; points = Points.empty (parser,Not_found, ref None)}
  
  let feed in_error t parser =
    let in_error = ref in_error in
    let points = Points.update' (t.parser, T t, in_error) parser t.points in
    !in_error, {parser; points}
  
  let rollback t =
    match Points.value t.points with
    | (kind, T t) :: _ -> Some (kind, t)
    | (_, _) :: _ -> assert false
    | _ -> None
end

type t = {
  rollback: Rollback.t;
  errors: exn list;
  in_error: Point.t option;
}

let parser t = t.rollback.Rollback.parser
let exns t = t.errors

let rollback warnings t input =
  let parser = Merlin_parser.to_step (parser t) in
  let error = Error_classifier.from parser input in
  match Rollback.rollback t.rollback with
  | None -> {t with errors = error :: (warnings @ t.errors)}
  | Some (_, rollback) -> 
    { 
      rollback; 
      errors = error :: (warnings @ t.errors);
      in_error = Some (Points.value rollback.Rollback.points); 
    }

let fresh parser = { rollback = Rollback.fresh parser; errors = []; in_error = None; }

let fold warnings token t =
  let parser = t.rollback.Rollback.parser in
  let pop w = let r = !warnings in w := []; r in
  let t =
    match token with
    | Merlin_lexer.Error _ -> t
    | Merlin_lexer.Valid (s,tok,e) ->
      Logger.debugf `internal
        (fun ppf tok -> Format.fprintf ppf "received %s"
            (Merlin_parser.Values.Token.to_string tok))
        tok;
    match Merlin_parser.feed (s,tok,e) parser with
    | `Accept _ ->
      Logger.debug `internal "parser accepted";
      {t with errors = (pop warnings) @ t.errors}
    | `Reject _ ->
      Logger.debug `internal "parser rejected";
      rollback (pop warnings) t (s,tok,e)
    | `Step parser ->
      let in_error, rollback = 
        Rollback.feed t.in_error t.rollback parser 
      in
      {t with rollback; in_error}
  in
  Logger.debugf `internal Merlin_parser.dump t.rollback.Rollback.parser;
  t

let fold token t = 
  let warnings = ref [] in
  Either.get (Merlin_parsing.catch_warnings warnings
                (fun () -> fold warnings token t))
