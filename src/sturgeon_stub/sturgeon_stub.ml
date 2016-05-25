open Sturgeon

type t = Recipes.server * Thread.t

let start f =
  (* Run monitor in parallel *)
  let server = Recipes.text_server "merlin" (fun ~args:_ -> f) in
  let monitor = Thread.create Recipes.main_loop server in
  (server, monitor)

let stop (server, monitor) =
  Recipes.stop_server server;
  Thread.join monitor

module Inuit = Inuit
module Cursor = Inuit.Cursor
module Widget = Inuit_widget

type flag = Stui.flag
type cursor = flag Inuit.cursor
let null = Inuit.Cursor.null

type shell = Stui.buffer_shell
let create_cursor = Stui.create_cursor
