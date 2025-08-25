exception Cancel_or_Closing

type 'a msg =
  | Msg of [ `Closing | `Cancel | `Exn of exn ]
  | Config of (Mconfig.t * Msource.t * (int * int) option)
  | Result of 'a

type 'a t = { waiting : bool Atomic.t; msg : 'a msg Shared.t }

let create () = { waiting = Atomic.make false; msg = Shared.create () }
