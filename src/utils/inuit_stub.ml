type flags = [ `Editable | `Clickable | `Raw ]

class type ['flags] cursor =
  object ('self)
    method text : ?flags:'flags list -> string -> unit
    method clear : unit

    method sub : 'self
    method sub_action : ('self -> unit) option -> 'self

    method is_closed : bool

    constraint 'flags = [> flags]
  end

type 'a action = 'a -> unit
  constraint 'a = _ #cursor

let text ?flags c txt = c#text ?flags txt
let clear c = c#clear

let sub ?action c =
  match action with
  | None -> c#sub
  | Some action -> c#sub_action action

let link c str action =
  text (sub c ~action:(Some action)) str

let printf c fmt =
  Printf.ksprintf (text c) fmt

let null_cursor =
  object (self)
    method text ?flags:_ _str = ()
    method clear = ()
    method is_closed = true

    method sub = self
    method sub_action _ = self
  end

let is_closed c = c#is_closed

module Nav = struct

  type 'cursor t = {
    mutable prev: 'cursor page list;
    mutable page: 'cursor page;
    mutable next: 'cursor page list;

    frame: 'cursor frame option;
  } constraint 'cursor = _ #cursor

  and 'cursor page = string * ('cursor frame -> unit)
  constraint 'cursor = _ #cursor

  and 'cursor frame = {
    title: 'cursor;
    body: 'cursor;
    nav: 'cursor t;
  } constraint 'cursor = _ #cursor

  let null_page : _ page = "", ignore

  let make title body =
    let page = (title, body) in
    { prev = []; page; next = []; frame = None }

  let update_frame t = match t.frame with None -> () | Some frame ->
    let {title; body} = frame in
    clear title;
    text title (fst t.page);
    clear body;
    (snd t.page) frame

  let goto t title body =
    t.page <- (title, body);
    t.next <- [];
    update_frame t

  let push t title body =
    t.prev <- t.page :: t.prev;
    goto t title body

  let next t = match t.next with
    | [] -> ()
    | page :: pages ->
      t.prev <- t.page :: t.prev;
      t.page <- page;
      t.next <- pages;
      update_frame t

  let prev t = match t.prev with
    | [] -> ()
    | page :: pages ->
      t.next <- t.page :: t.next;
      t.page <- page;
      t.prev <- pages;
      update_frame t

  let render_header t cursor =
    link cursor "⏪" (fun _ -> prev t);
    text cursor " ";
    link cursor "↻" (fun _ -> update_frame t);
    text cursor " ";
    link cursor	"⏩" (fun _ -> next t)

  let render t cursor =
    if not (is_closed cursor) then (
      let header = sub cursor in
      text cursor " ";
      let title = sub cursor in
      text cursor "\n\n";
      let body = sub cursor in
      let rec nav = {t with frame = Some frame}
      and frame = { title = title; body = body; nav = nav }
      in
      render_header nav header;
      update_frame nav
    )
end

module Tree = struct

  type 'cursor t = {
    indent: int;
    cursor: 'cursor;
  } constraint 'cursor = _ #cursor

  let not_closed t =
    not (is_closed t.cursor)

  let make cursor =
    { indent = 0; cursor = sub cursor }

  let indent t =
    if t.indent > 0 then
      text t.cursor (String.make t.indent ' ')

  let add_leaf ?action t =
    indent t;
    (*text t.cursor "  ";*)
    let result = sub ?action t.cursor in
    text t.cursor "\n";
    result

  let add_node children ?action ?(opened=ref false) t =
    indent t;
    let body = ref None in
    link t.cursor (if !opened then "▪" else "▫") (fun c ->
        match !body with
        | None -> ()
        | Some t' when !opened ->
          opened := false;
          clear c; text c "▫";
          clear t'.cursor
        | Some t' ->
          opened := true;
          clear c; text c "▪";
          children t'
      );
    text t.cursor " ";
    let result = sub ?action t.cursor in
    text t.cursor "\n";
    let t' = { indent = t.indent + 1; cursor = sub t.cursor } in
    body := Some t';
    if !opened then children t';
    result

  let add ?children ?action ?opened t =
    if not_closed t then (
      match children with
      | None -> add_leaf ?action t
      | Some children -> add_node children ?action ?opened t
    ) else
      t.cursor

  let clear t = clear t.cursor
end
