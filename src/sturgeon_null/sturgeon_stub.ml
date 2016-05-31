type t = Server
type shell = Shell

let start _ = Server
let stop Server = ()

type flag = [ `Editable | `Clicked | `Clickable | `Invisible ]
type cursor = Cursor
let null = Cursor

module Cursor = struct
  let text Cursor _ = ()
  let printf Cursor = Printf.ksprintf ignore
  let is_closed Cursor = true
  let link Cursor = Printf.ksprintf (fun _ _ -> ())
  let sub Cursor = Cursor
  let clear Cursor = ()
  let rem_flag _ Cursor = Cursor
  let clickable Cursor _ = Cursor
end

module Widget = struct
  module Nav = struct
    type 'a t = Nav

    type 'a frame = {
      title: cursor;
      body: cursor;
      nav: 'a t;
    }

    let push Nav _ _ = ()

    let make _ _ = Nav

    let render Nav Cursor = ()
  end

end

let create_cursor Shell ~name = Cursor
