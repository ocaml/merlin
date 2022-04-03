open Brr
module Worker = Brr_webworkers.Worker

(* When a query is sent to the Worker we keep the Future result in an indexed
table so that the on_message function will be able to determine the Future when
the answer is posted by the Worker.
The Worker works synchronously so we expect answer to arrive in order. *)
type worker = {
  worker: Worker.t;
  queue: (Jv.t -> unit) Queue.t
}

let add_fut worker res = Queue.add res worker.queue
let res_fut worker v = (Queue.take worker.queue) v

let make_worker url =
  let worker = Worker.create @@ Jstr.of_string url in
  let queue = Queue.create () in
  let worker = { worker; queue } in
  let on_message m =
    let m = Ev.as_type m in
    let data = Brr_io.Message.Ev.data m in
    res_fut worker data
  in
  Ev.listen Brr_io.Message.Ev.message on_message @@
    Worker.as_target worker.worker;
  worker

(* todo share that with worker *)
type action = Completion | Type_enclosing | Errors

let query ~action worker source cursor_offset ((*todo: other queries*)) =
  let open Fut.Syntax in
  let fut, set  = Fut.create () in
  add_fut worker set;
  Worker.post worker.worker (action, source, cursor_offset);
  let+ data : Jv.t = fut in
  Console.(log ["Received:"; data]);
  (* El.(set_prop p_innerHTML (Jstr.of_string data) results_div); *)
  Ok data

let query_to_js ~action worker source cursor_offset =
  let source = Jv.to_string source in
  Fut.to_promise ~ok:Fun.id @@
    query ~action worker source cursor_offset ()

let make_worker () = make_worker "merlin_worker.bc.js"
let () = Jv.set Jv.global "make_worker" (Jv.repr make_worker)

let query_completion worker source cursor_offset =
  query_to_js ~action:Completion worker source cursor_offset

let query_type_enclosing worker source cursor_offset =
  query_to_js ~action:Type_enclosing worker source cursor_offset

let query_errors worker source cursor_offset =
  query_to_js ~action:Errors worker source cursor_offset

let () = Jv.set Jv.global "query_worker_completion" @@
  Jv.repr query_completion
let () = Jv.set Jv.global "query_worker_type_enclosing" @@
  Jv.repr query_type_enclosing
  let () = Jv.set Jv.global "query_worker_errors" @@
  Jv.repr query_errors
