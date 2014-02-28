(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open MenhirLib.EngineTypes

type witness =
  | Zero : witness
  | Stack : int * ('s,'v) stack -> witness

let initial_depth = Zero

let rec raw_depth stack =
  if stack.next == stack
  then 0
  else 1 + raw_depth stack.next

let rec find frame depth ({next = ref_next} as ref) =
  let (==) a b = Obj.repr a == Obj.repr b in
  if ref == frame then
    depth
  else if ref == ref_next then
    raw_depth frame
  else
    find frame (depth - 1) ref_next

let inc_depth depth ref ({next = cur_next} as cur) =
  if cur == cur_next
  then 0
  else 1 + find cur_next depth ref

let stack_depth ~hint stk =
  let d = match hint with
    | Zero ->
      raw_depth stk
    | Stack (d,ref) ->
      inc_depth d ref stk
  in
  Stack (d,stk)

let env_depth ~hint env = stack_depth ~hint env.stack

let depth = function
  | Zero -> 0
  | Stack (n,_) -> n

let pop env =
  let cell = env.stack in
  let next = cell.next in
  if next == cell then
    None
  else
    Some {env with stack = next; current = cell.state}
