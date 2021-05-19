let s = object
  val mutable v = [0; 2]

  method pop =
    match v with
    | hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd = v <- hd :: v
end

let r = s#push 3

let poly obj = obj#pouet "a"

let nopoly (obj : < pouet : string -> 'a>) = obj#pouet "a"

class virtual foobar = object
  method virtual virtu : string -> char -> int
  val virtual virt_avl : string -> char -> int
end
