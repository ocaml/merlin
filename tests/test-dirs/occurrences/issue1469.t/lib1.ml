let boo r = fresh a (r === a)
let boo_expanded r = call_fresh (fun a -> r === a)
