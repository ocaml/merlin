type 'a t = { mutex : Mutex.t; cond : Condition.t; mutable value : 'a }

let locking_set t a =
  Mutex.protect t.mutex @@ fun () ->
  t.value <- a;
  Condition.signal t.cond

let set t a =
  t.value <- a;
  Condition.signal t.cond
let locking_get t = Mutex.protect t.mutex @@ fun () -> t.value

let get t = t.value

let create a =
  { mutex = Mutex.create (); cond = Condition.create (); value = a }

let wait a = Condition.wait a.cond a.mutex
let signal a = Condition.signal a.cond
let protect a = Mutex.protect a.mutex

(* 
  Design question :
  
  here some value are only read by one side and write by the other. The side 
  reading has to wait for a new value to be provided thus the use of the 
  condition module. However locking to do thought seems quite useless. 


  Current use :


  let d1_work shared = 
    ...
    Shared.set shared new_value
    ...

  let d2_work shared = 
    match Shared.get shared with
    | None -> Condition.wait shared.cond shared.mutex
    | Some new_value -> do something from new_value 


    Better use : 
    - Shared.get could wait until a new value is written 
  
    let await_get (t : 'a option t) =
      Mutex.protect t.mutex @@ fun () ->
      let rec loop () =
        match t.value with
        | None ->
          Condition.wait t.cond t.mutex;
          loop ()
        | Some v -> v
     in
      loop ()

      Bad idea -> the value can then be changed as the mutex is unlock.



    Issue :
  
  
  *)
