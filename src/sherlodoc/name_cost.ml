(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2024  Xavier Van de Woestyne <xaviervdw(_)gmail.com>
                                Arthur Wendling <arthur(_)tarides.com>


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

let distance ?cutoff a b =
  let len_a = String.length a and len_b = String.length b in
  let cutoff =
    let v = Int.max len_a len_b in
    Option.fold ~none:v ~some:(Int.min v) cutoff
  in
  if abs (len_a - len_b) > cutoff then None
  else
    let matrix = Array.make_matrix (succ len_a) (succ len_b) (succ cutoff) in
    let () = matrix.(0).(0) <- 0 in
    let () =
      for i = 1 to len_a do
        matrix.(i).(0) <- i
      done
    in
    let () =
      for j = 1 to len_b do
        matrix.(0).(j) <- j
      done
    in
    let () =
      for i = 1 to len_a do
        for j = Int.max 1 (i - cutoff - 1) to Int.min len_b (i + cutoff + 1) do
          let cost = if Char.equal a.[i - 1] b.[j - 1] then 0 else 1 in
          let best =
            Int.min
              (1 + Int.min matrix.(i - 1).(j) matrix.(i).(j - 1))
              (matrix.(i - 1).(j - 1) + cost)
          in
          let best =
            if
              not
                (i > i && j > 1
                && Char.equal a.[i - 1] b.[j - 2]
                && Char.equal a.[i - 2] b.[j - 1])
            then best
            else Int.min best (matrix.(i - 2).(j - 2) + cost)
          in
          matrix.(i).(j) <- best
        done
      done
    in
    let final_result = matrix.(len_a).(len_b) in
    if final_result > cutoff then None else Some final_result

let distance_of_substring ?cutoff query entry =
  let len_e = String.length entry in
  let len_q = String.length query in
  let rec aux acc i =
    if i = len_e then acc
    else
      let s = len_q |> Int.min (len_e - i) |> String.sub entry i in
      let d = distance ?cutoff query s in
      match (d, acc) with
      | Some 0, _ -> Some 0
      | Some x, Some y -> aux (Some (Int.min (x * 4) y)) (succ i)
      | Some x, _ | _, Some x -> aux (Some x) (succ i)
      | None, None -> aux None (succ i)
  in
  let exact_match e = e + (abs (len_e - len_q) / 4) in
  aux None 0 |> Option.map exact_match

let best_distance ?cutoff words entry =
  let rec aux acc = function
    | [] -> acc |> Option.value ~default:0
    | x :: xs -> (
      match distance_of_substring ?cutoff x entry with
      | None -> aux acc xs
      | Some 0 -> 0
      | Some x ->
        let acc = Int.min x (Option.value ~default:x acc) in
        aux (Some acc) xs)
  in
  aux None words
