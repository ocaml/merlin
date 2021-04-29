module type S = sig
  type 'a t = 'a
    constraint 'a = < m : r >
  and r = (< m : r >) t
end

module type S = sig type 'a t = 'a constraint 'a = < m : r > and r = < m : r > t end

module type T = S with type 'a t = 'b constraint 'a = < m : 'b >
