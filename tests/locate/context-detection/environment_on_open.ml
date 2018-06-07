module M = struct
  (* should jump above this comment,
     but jumps below it*)
  module M = struct end
end
open M
