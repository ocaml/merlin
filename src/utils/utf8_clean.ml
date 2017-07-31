(* utf-8 decoding dfa, from http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ *)

let utf8d =
  "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
   \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\
   \007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
   \b\b\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\
   \n\003\003\003\003\003\003\003\003\003\003\003\003\004\003\003\
   \011\006\006\006\005\b\b\b\b\b\b\b\b\b\b\b\
   \000\001\002\003\005\b\007\001\001\001\004\006\001\001\001\001\
   \001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\001\000\001\001\001\001\001\000\001\000\001\001\001\001\001\001\
   \001\002\001\001\001\001\001\002\001\002\001\001\001\001\001\001\001\001\001\001\001\001\001\002\001\001\001\001\001\001\001\001\
   \001\002\001\001\001\001\001\001\001\002\001\001\001\001\001\001\001\001\001\001\001\001\001\003\001\003\001\001\001\001\001\001\
   \001\003\001\001\001\001\001\003\001\003\001\001\001\001\001\001\001\003\001\001\001\001\001\001\001\001\001\001\001\001\001\001"

let utf8_decode index str =
  let codep = ref 0 in
  let state = ref 0 in
  let len = String.length str in
  let index' = ref !index in
  while (
    !index' < len &&
    let c = Char.code (String.get str !index') in
    let t = Char.code (String.unsafe_get utf8d c) in
    codep := (if !state <> 0 then (c land 0x3f) lor (!codep lsl 6) else (0xff lsr t) land c);
    state := Char.code (String.unsafe_get utf8d (256 + !state * 16 + t) );
    incr index';
    !state > 1
  ) do ()
  done;
  index := !index';
  if !state = 0 then !codep else - !codep

let utf8_encode buf = function
  | codep when codep < 0 ->
      Buffer.add_char buf '_'
  | codep when codep <= 0x7F ->
      Buffer.add_char buf (Char.chr codep)
  | codep when codep <= 0x7FF ->
      Buffer.add_char buf (Char.chr (0b1100_0000 lor (codep lsr 6)));
      Buffer.add_char buf (Char.chr (0b1000_0000 lor (codep land 0b00111111)))
  | codep when codep <= 0xFFFF ->
      Buffer.add_char buf (Char.chr (0b11100_0000 lor (codep lsr 12)));
      Buffer.add_char buf (Char.chr (0b1000_0000 lor ((codep lsr 6) land 0b00111111)));
      Buffer.add_char buf (Char.chr (0b1000_0000 lor (codep land 0b00111111)))
  | codep when codep <= 0x10FFFF ->
      Buffer.add_char buf (Char.chr (0b111100_0000 lor (codep lsr 18)));
      Buffer.add_char buf (Char.chr (0b1000_0000 lor ((codep lsr 12) land 0b00111111)));
      Buffer.add_char buf (Char.chr (0b1000_0000 lor ((codep lsr 6) land 0b00111111)));
      Buffer.add_char buf (Char.chr (0b1000_0000 lor (codep land 0b00111111)))
  | _ ->
      Buffer.add_char buf '_'

let utf8_clean str =
  let len = String.length str in
  let correct = ref 0 in
  let index = ref 0 in
  while !index < len do
    if utf8_decode index str < 0 then index := len else correct := !index
  done;
  if !correct = len then `Good str
  else
    let buf = Buffer.create (!correct + len - !correct * 2) in
    Buffer.add_substring buf str 0 !correct;
    let index = ref !correct in
    while !index < len do
      utf8_encode buf (abs (utf8_decode index str))
    done;
    `Fixed (Buffer.contents buf)
