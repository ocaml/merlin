Aggregating two indexes crashes while writing the output: the writer reaches a
small value whose parent link it has already rewritten to an `On_disk_ptr`.
The modules below were reduced from merlin's own `extract_ranges` and
`show_location` test helpers. The crash depends on the exact shape of the
index maps, so small changes (including renaming the modules) can hide it.
A stub for the external `Yojson` library:

  $ cat >yojson.ml <<'EOF'
  > module Safe = struct
  >   type t = [ `Assoc of (string * t) list | `Int of int | `List of t list ]
  >   let from_string _ : t = `List []
  > end
  > EOF


  $ cat >ea.ml <<'EOF'
  > let ( let> ) x f = x f
  > type pos = { line : int; col : int }
  > type range = { start : pos; end_ : pos }
  > let pos_of_json (j : Yojson.Safe.t) =
  >   match j with
  >   | `Assoc [ ("col", `Int col); ("line", `Int line) ] ->
  >     { line = line - 1; col }
  > let range_of_json (j : Yojson.Safe.t) =
  >   match j with
  >   | `Assoc (("end", pos_end) :: ("start", pos_start) :: _) ->
  >     let start = pos_of_json pos_start in
  >     let end_ = pos_of_json pos_end in
  >     { start; end_ }
  > let extract range s =
  >   s
  >   |> Array.mapi (fun i s ->
  >       if i < range.start.line then None
  >       else if i = range.start.line && range.start.line = range.end_.line then
  >         let spaces =
  >           List.init range.start.col (fun i ->
  >               if i + 3 < range.start.col then "." else ".")
  >         in
  >         let extracted =
  >           String.sub s range.start.col (range.end_.col - range.start.col)
  >         in
  >         let spaces =
  >           List.init range.start.col (fun i ->
  >               if i + 3 < range.start.col then "." else ".")
  >           |> String.concat ""
  >         in
  >         let extracted =
  >           String.sub s range.start.col (String.length s - range.start.col)
  >         in
  >         Some spaces
  >       else None)
  >   |> Array.to_list |> List.filter_map Fun.id |> String.concat ""
  > let ranges_of_json (j : Yojson.Safe.t) =
  >   match j with
  >   | `List l -> List.map range_of_json l
  > let () =
  >   let file =
  >     Sys.argv.(1)
  >   in
  >   let> ic = In_channel.with_open_bin file in
  >   let source = In_channel.input_all ic in
  >   let source = source |> String.split_on_char ' ' |> Array.of_list in
  >   let ranges =
  >     stdin |> In_channel.input_all |> Yojson.Safe.from_string |> ranges_of_json
  >   in
  >   ranges
  >   |> List.iteri @@ fun i range ->
  >      let extracted_range = extract range source in
  >      print_string extracted_range
  > EOF

  $ cat >sl.ml <<'EOF'
  > let parse_position _ = if true then `Start else if true then `Offset 0 else `Logical (0, 0)
  > let cursor = "|"
  > let () =
  >   match parse_position Sys.argv.(2) with
  >   | `Start ->
  >     let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  >     print_endline (source ^ cursor)
  >   | `Offset i ->
  >     let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  >     let pre = String.sub source 0 i in
  >     let post = String.sub source i (String.length source - i) in
  >     print_endline (pre ^ cursor ^ post)
  >   | `Logical (line, char) ->
  >     let lines = In_channel.with_open_bin Sys.argv.(1) In_channel.input_lines in
  >     List.iteri
  >       (fun i l ->
  >         if i = line then
  >           let pre = String.sub l 0 char in
  >           let post =
  >             String.sub l
  >               (char + 1)
  >               (String.length l - char - 1)
  >           in
  >           print_endline (pre ^ cursor ^ post)
  >         else print_endline l)
  >       lines
  > EOF

  $ $OCAMLC -c yojson.ml
  $ $OCAMLC -w -a -bin-annot -bin-annot-occurrences -c ea.ml sl.ml

Index each module separately:
  $ ocaml-index aggregate ea.cmt -o ea.ocaml-index
  $ ocaml-index aggregate sl.cmt -o sl.ocaml-index

Then aggregate the two indexes (with backtraces disabled, to keep the output
stable):
  $ OCAMLRUNPARAM=b=0 ocaml-index aggregate ea.ocaml-index sl.ocaml-index -o all.ocaml-index
  Fatal error: exception Invalid_argument("Granular_marshal.fetch_parent: Unexpected parent link On_disk_ptr(loc=317)")
  [2]
