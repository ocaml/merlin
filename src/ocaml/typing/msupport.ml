(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the "Software"),
     to deal in the Software without restriction, including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
     sell copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in
     all copies or substantial portions of the Software.

     The Software is provided "as is", without warranty of any kind, express or
     implied, including but not limited to the warranties of merchantability,
     fitness for a particular purpose and noninfringement. In no event shall
     the authors or copyright holders be liable for any claim, damages or other
     liability, whether in an action of contract, tort or otherwise, arising
     from, out of or in connection with the software or the use or other dealings
     in the Software.

   )* }}} *)

let catch_errors_with_warning warnings errors callback =
  let warnings' = Warnings.backup () in
  Warnings.restore warnings;
  Typing_recovery.catch_errors errors (fun () ->
      Misc.try_finally ~always:(fun () -> Warnings.restore warnings') callback)

let () = Msupport_parsing.msupport_raise_error := Typing_recovery.log_or_raise

let () =
  Typing_recovery.register_recoverable (function
        ( Misc.Fatal_error _
        | Persistent_env.Error _
        | Typemod.Error_forward _
        | Magic_numbers.Cmi.Error _) as exn ->
        (* Some specific error handling that can't be supported at the
           compiler level.

           This was previously handled by [Typemod.initial_env]
        *)
        Typing_recovery.log_or_raise exn;
        true

      | _ -> false
    )
