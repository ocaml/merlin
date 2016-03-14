open Sturgeon

type t = Recipes.server * Thread.t

let start f =
  let open Sturgeon in
  (* Run monitor in parallel *)
  let server = Recipes.text_server "merlin" (fun ~args:_ -> f) in
  let monitor = Thread.create Recipes.main_loop server in
  (server, monitor)

let stop (server, monitor) =
  Sturgeon.Recipes.stop_server server;
  Thread.join monitor
