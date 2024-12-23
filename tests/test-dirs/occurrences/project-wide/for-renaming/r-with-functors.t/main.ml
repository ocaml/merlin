module P = struct let txt = "Hello world!" end;;
module M = Func.Make(P);;

print_endline M.txt
