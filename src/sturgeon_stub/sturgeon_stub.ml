open Sturgeon

type t = Sturgeon_recipes_server.server * Thread.t

let start f =
  (* Run monitor in parallel *)
  let server = Sturgeon_recipes_server.text_server "merlin" (fun ~args:_ -> f) in
  let monitor = Thread.create Sturgeon_recipes_server.main_loop server in
  (server, monitor)

let stop (server, monitor) =
  Sturgeon_recipes_server.stop_server server;
  Thread.join monitor

module Inuit = Inuit
module Cursor = Inuit.Cursor
module Widget = Inuit_widget

type flag = Stui.flag
type cursor = flag Inuit.cursor
let null = Inuit.Cursor.null

type shell = Stui.shell
let create_cursor = Stui.create_cursor
