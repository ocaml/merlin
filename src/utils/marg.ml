open Std

(** {1 Flag parsing utils} *)

type 'a t = string list -> 'a -> string list * 'a

type 'a table = (string, 'a t) Hashtbl.t

let unit f : 'a t = fun args acc -> (args, f acc)

let param ptype f : 'a t =
 fun args acc ->
  match args with
  | [] -> failwith ("expects a " ^ ptype ^ " argument")
  | arg :: args -> (args, f arg acc)

let unit_ignore : 'a t = fun x -> unit (fun x -> x) x

let param_ignore x = param "string" (fun _ x -> x) x

let bool f =
  param "bool" (function
    | "yes" | "y" | "Y" | "true" | "True" | "1" -> f true
    | "no" | "n" | "N" | "false" | "False" | "0" -> f false
    | str ->
      failwithf "expecting boolean (%s), got %S."
        "yes|y|Y|true|1 / no|n|N|false|0" str)

let int f =
  param "int" (fun str ->
      match int_of_string_opt str with
      | None -> failwithf "expecting integer got %S." str
      | Some x -> f x)

type docstring = string

type 'a spec = string * docstring * 'a t

let rec assoc3 key = function
  | [] -> raise Not_found
  | (key', _, value) :: _ when key = key' -> value
  | _ :: xs -> assoc3 key xs

let rec mem_assoc3 key = function
  | [] -> false
  | (key', _, _) :: xs -> key = key' || mem_assoc3 key xs

let parse_one ~warning global_spec local_spec args global local =
  match args with
  | [] -> None
  | arg :: args -> (
    match Hashtbl.find global_spec arg with
    | action -> begin
      match action args global with
      | args, global -> Some (args, global, local)
      | exception Failure msg ->
        warning ("flag " ^ arg ^ " " ^ msg);
        Some (args, global, local)
      | exception exn ->
        warning ("flag " ^ arg ^ ": error, " ^ Printexc.to_string exn);
        Some (args, global, local)
    end
    | exception Not_found -> (
      match assoc3 arg local_spec with
      | action -> begin
        match action args local with
        | args, local -> Some (args, global, local)
        | exception Failure msg ->
          warning ("flag " ^ arg ^ " " ^ msg);
          Some (args, global, local)
        | exception exn ->
          warning ("flag " ^ arg ^ ": error, " ^ Printexc.to_string exn);
          Some (args, global, local)
      end
      | exception Not_found -> None))

let parse_all ~warning global_spec local_spec =
  let rec normal_parsing args global local =
    match parse_one ~warning global_spec local_spec args global local with
    | Some (args, global, local) -> normal_parsing args global local
    | None -> (
      match args with
      | arg :: args -> begin
        (* We split on the first '=' to check if the argument was
           of the form name=value *)
        try
          let name, value = Misc.cut_at arg '=' in
          normal_parsing (name :: value :: args) global local
        with Not_found ->
          warning ("unknown flag " ^ arg);
          resume_parsing args global local
      end
      | [] -> (global, local))
  and resume_parsing args global local =
    let args =
      match args with
      | arg :: args
        when not (Hashtbl.mem global_spec arg || mem_assoc3 arg local_spec) ->
        args
      | args -> args
    in
    normal_parsing args global local
  in
  normal_parsing
