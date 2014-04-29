module InfiniteArray : sig
  (* $Id: infiniteArray.mli,v 1.5 2007/09/10 21:09:37 fpottier Exp $ *)

  (** This module implements infinite arrays. **)
  type 'a t

  (** [make x] creates an infinite array, where every slot contains [x]. **)
  val make: 'a -> 'a t

  (** [get a i] returns the element contained at offset [i] in the array [a].
     Slots are numbered 0 and up. **)
  val get: 'a t -> int -> 'a

  (** [set a i x] sets the element contained at offset [i] in the array
      [a] to [x]. Slots are numbered 0 and up. **)
  val set: 'a t -> int -> 'a -> unit

  (** [extent a] is the length of an initial segment of the array [a]
      that is sufficiently large to contain all [set] operations ever
      performed. In other words, all elements beyond that segment have
      the default value. *)
  val extent: 'a t -> int

  (** [domain a] is a fresh copy of an initial segment of the array [a]
      whose length is [extent a]. *)
  val domain: 'a t -> 'a array

end = struct
  (* $Id: infiniteArray.ml,v 1.6 2007/09/10 21:09:37 fpottier Exp $ *)

  (** This module implements infinite arrays, that is, arrays that grow
      transparently upon demand. *)

  type 'a t = {
      default: 'a;
      mutable table: 'a array;
      mutable extent: int; (* the index of the greatest [set] ever, plus one *)
    }

  let default_size =
    16384 (* must be non-zero *)

  let make x = {
    default = x;
    table = Array.make default_size x;
    extent = 0;
  }

  let rec new_length length i =
    if i < length then
      length
    else
      new_length (2 * length) i

  let ensure a i =
    let table = a.table in
    let length = Array.length table in
    if i >= length then begin
      let table' = Array.make (new_length (2 * length) i) a.default in
      Array.blit table 0 table' 0 length;
      a.table <- table'
    end

  let get a i =
    ensure a i;
    a.table.(i)

  let set a i x =
    ensure a i;
    a.table.(i) <- x;
    a.extent <- max (i + 1) a.extent

  let extent a =
    a.extent

  let domain a =
    Array.sub a.table 0 a.extent


end
module PackedIntArray : sig
  (* A packed integer array is represented as a pair of an integer [k] and
     a string [s]. The integer [k] is the number of bits per integer that we
     use. The string [s] is just an array of bits, which is read in 8-bit
     chunks. *)

  (* The ocaml programming language treats string literals and array literals
     in slightly different ways: the former are statically allocated, while
     the latter are dynamically allocated. (This is rather arbitrary.) In the
     context of Menhir's table-based back-end, where compact, immutable
     integer arrays are needed, ocaml strings are preferable to ocaml arrays. *)

  type t =
    int * string

  (* [pack a] turns an array of integers into a packed integer array. *)

  (* Because the sign bit is the most significant bit, the magnitude of
     any negative number is the word size. In other words, [pack] does
     not achieve any space savings as soon as [a] contains any negative
     numbers, even if they are ``small''. *)

  val pack: int array -> t

  (* [get t i] returns the integer stored in the packed array [t] at index [i]. *)

  (* Together, [pack] and [get] satisfy the following property: if the index [i]
     is within bounds, then [get (pack a) i] equals [a.(i)]. *)

  val get: t -> int -> int

  (* [get1 t i] returns the integer stored in the packed array [t] at index [i].
     It assumes (and does not check) that the array's bit width is [1]. The
     parameter [t] is just a string. *)

  val get1: string -> int -> int


end = struct
  (* A packed integer array is represented as a pair of an integer [k] and
     a string [s]. The integer [k] is the number of bits per integer that we
     use. The string [s] is just an array of bits, which is read in 8-bit
     chunks. *)

  (* The ocaml programming language treats string literals and array literals
     in slightly different ways: the former are statically allocated, while
     the latter are dynamically allocated. (This is rather arbitrary.) In the
     context of Menhir's table-based back-end, where compact, immutable
     integer arrays are needed, ocaml strings are preferable to ocaml arrays. *)

  type t =
    int * string

  (* The magnitude [k] of an integer [v] is the number of bits required
     to represent [v]. It is rounded up to the nearest power of two, so
     that [k] divides [Sys.word_size]. *)

  let magnitude (v : int) =
    if v < 0 then
      Sys.word_size
    else
      let rec check k max = (* [max] equals [2^k] *)
        if (max <= 0) || (v < max) then
          k
            (* if [max] just overflew, then [v] requires a full ocaml
               integer, and [k] is the number of bits in an ocaml integer
               plus one, that is, [Sys.word_size]. *)
        else
          check (2 * k) (max * max)
      in
      check 1 2

  (* [pack a] turns an array of integers into a packed integer array. *)

  (* Because the sign bit is the most significant bit, the magnitude of
     any negative number is the word size. In other words, [pack] does
     not achieve any space savings as soon as [a] contains any negative
     numbers, even if they are ``small''. *)

  let pack (a : int array) : t =

    let m = Array.length a in

    (* Compute the maximum magnitude of the array elements. This tells
       us how many bits per element we are going to use. *)

    let k =
      Array.fold_left (fun k v ->
        max k (magnitude v)
      ) 1 a
    in

    (* Because access to ocaml strings is performed on an 8-bit basis,
       two cases arise. If [k] is less than 8, then we can pack multiple
       array entries into a single character. If [k] is greater than 8,
       then we must use multiple characters to represent a single array
       entry. *)

    if k <= 8 then begin

      (* [w] is the number of array entries that we pack in a character. *)

      assert (8 mod k = 0);
      let w = 8 / k in

      (* [n] is the length of the string that we allocate. *)

      let n =
        if m mod w = 0 then
          m / w
        else
          m / w + 1
      in

      let s =
        String.create n
      in

      (* Define a reader for the source array. The reader might run off
         the end if [w] does not divide [m]. *)

      let i = ref 0 in
      let next () =
        let ii = !i in
        if ii = m then
          0 (* ran off the end, pad with zeroes *)
        else
          let v = a.(ii) in
          i := ii + 1;
          v
      in

      (* Fill up the string. *)

      for j = 0 to n - 1 do
        let c = ref 0 in
        for x = 1 to w do
          c := (!c lsl k) lor next()
        done;
        s.[j] <- Char.chr !c
      done;

      (* Done. *)

      k, s

    end
    else begin (* k > 8 *)

      (* [w] is the number of characters that we use to encode an array entry. *)

      assert (k mod 8 = 0);
      let w = k / 8 in

      (* [n] is the length of the string that we allocate. *)

      let n =
        m * w
      in

      let s =
        String.create n
      in

      (* Fill up the string. *)

      for i = 0 to m - 1 do
        let v = ref a.(i) in
        for x = 1 to w do
          s.[(i + 1) * w - x] <- Char.chr (!v land 255);
          v := !v lsr 8
        done
      done;

      (* Done. *)

      k, s

    end

  (* Access to a string. *)

  let read (s : string) (i : int) : int =
    Char.code (String.unsafe_get s i)

  (* [get1 t i] returns the integer stored in the packed array [t] at index [i].
     It assumes (and does not check) that the array's bit width is [1]. The
     parameter [t] is just a string. *)

  let get1 (s : string) (i : int) : int =
    let c = read s (i lsr 3) in
    let c = c lsr ((lnot i) land 0b111) in
    let c = c land 0b1 in
    c

  (* [get t i] returns the integer stored in the packed array [t] at index [i]. *)

  (* Together, [pack] and [get] satisfy the following property: if the index [i]
     is within bounds, then [get (pack a) i] equals [a.(i)]. *)

  let get ((k, s) : t) (i : int) : int =
    match k with
    | 1 ->
        get1 s i
    | 2 ->
        let c = read s (i lsr 2) in
        let c = c lsr (2 * ((lnot i) land 0b11)) in
        let c = c land 0b11 in
        c
    | 4 ->
        let c = read s (i lsr 1) in
        let c = c lsr (4 * ((lnot i) land 0b1)) in
        let c = c land 0b1111 in
        c
    | 8 ->
        read s i
    | 16 ->
        let j = 2 * i in
        (read s j) lsl 8 + read s (j + 1)
    | _ ->
        assert (k = 32); (* 64 bits unlikely, not supported *)
        let j = 4 * i in
        (((read s j lsl 8) + read s (j + 1)) lsl 8 + read s (j + 2)) lsl 8 + read s (j + 3)


end
module RowDisplacement : sig
  (* This module compresses a two-dimensional table, where some values
     are considered insignificant, via row displacement. *)

  (* A compressed table is represented as a pair of arrays. The
     displacement array is an array of offsets into the data array. *)

  type 'a table =
      int array * (* displacement *)
       'a array   (* data *)

  (* [compress equal insignificant dummy m n t] turns the two-dimensional table
     [t] into a compressed table. The parameter [equal] is equality of data
     values. The parameter [wildcard] tells which data values are insignificant,
     and can thus be overwritten with other values. The parameter [dummy] is
     used to fill holes in the data array. [m] and [n] are the integer
     dimensions of the table [t]. *)

  val compress:
    ('a -> 'a -> bool) ->
    ('a -> bool) ->
    'a ->
    int -> int ->
    'a array array ->
    'a table

  (* [get ct i j] returns the value found at indices [i] and [j] in the
     compressed table [ct]. This function call is permitted only if the
     value found at indices [i] and [j] in the original table is
     significant -- otherwise, it could fail abruptly. *)

  (* Together, [compress] and [get] have the property that, if the value
     found at indices [i] and [j] in an uncompressed table [t] is
     significant, then [get (compress t) i j] is equal to that value. *)

  val get:
    'a table ->
    int -> int ->
    'a

  (* [getget] is a variant of [get] which only requires read access,
     via accessors, to the two components of the table. *)

  val getget:
    ('displacement -> int -> int) ->
    ('data -> int -> 'a) ->
    'displacement * 'data ->
    int -> int ->
    'a


end = struct
  (* This module compresses a two-dimensional table, where some values
     are considered insignificant, via row displacement. *)

  (* This idea reportedly appears in Aho and Ullman's ``Principles
     of Compiler Design'' (1977). It is evaluated in Tarjan and Yao's
     ``Storing a Sparse Table'' (1979) and in Dencker, Dürre, and Heuft's
     ``Optimization of Parser Tables for Portable Compilers'' (1984). *)

  (* A compressed table is represented as a pair of arrays. The
     displacement array is an array of offsets into the data array. *)

  type 'a table =
      int array * (* displacement *)
       'a array   (* data *)

  (* In a natural version of this algorithm, displacements would be greater
     than (or equal to) [-n]. However, in the particular setting of Menhir,
     both arrays are intended to be compressed with [PackedIntArray], which
     does not efficiently support negative numbers. For this reason, we are
     careful not to produce negative displacements. *)

  (* In order to avoid producing negative displacements, we simply use the
     least significant bit as the sign bit. This is implemented by [encode]
     and [decode] below. *)

  (* One could also think, say, of adding [n] to every displacement, so as
     to ensure that all displacements are nonnegative. This would work, but
     would require [n] to be published, for use by the decoder. *)

  let encode (displacement : int) : int =
    if displacement >= 0 then
      displacement lsl 1
    else
      (-displacement) lsl 1 + 1

  let decode (displacement : int) : int =
    if displacement land 1 = 0 then
      displacement lsr 1
    else
      -(displacement lsr 1)

  (* It is reasonable to assume that, as matrices grow large, their
     density becomes low, i.e., they have many insignificant entries.
     As a result, it is important to work with a sparse data structure
     for rows. We internally represent a row as a list of its
     significant entries, where each entry is a pair of a [j] index and
     an element. *)

  type 'a row =
      (int * 'a) list

  (* [compress equal insignificant dummy m n t] turns the two-dimensional table
     [t] into a compressed table. The parameter [equal] is equality of data
     values. The parameter [wildcard] tells which data values are insignificant,
     and can thus be overwritten with other values. The parameter [dummy] is
     used to fill holes in the data array. [m] and [n] are the integer
     dimensions of the table [t]. *)

  let compress
      (equal : 'a -> 'a -> bool)
      (insignificant : 'a -> bool)
      (dummy : 'a)
      (m : int) (n : int)
      (t : 'a array array)
      : 'a table =

    (* Be defensive. *)

    assert (Array.length t = m);
    assert begin
      for i = 0 to m - 1 do
        assert (Array.length t.(i) = n)
      done;
      true
    end;

    (* This turns a row-as-array into a row-as-sparse-list. The row is
       accompanied by its index [i] and by its rank (the number of its
       significant entries, that is, the length of the row-as-a-list. *)

    let sparse (i : int) (line : 'a array) : int * int * 'a row (* index, rank, row *) =

      let rec loop (j : int) (rank : int) (row : 'a row) =
        if j < 0 then
          i, rank, row
        else
          let x = line.(j) in
          if insignificant x then
            loop (j - 1) rank row
          else
            loop (j - 1) (1 + rank) ((j, x) :: row)
      in

      loop (n - 1) 0 []

    in

    (* Construct an array of all rows, together with their index and rank. *)

    let rows : (int * int * 'a row) array = (* index, rank, row *)
      Array.mapi sparse t
    in

    (* Sort this array by decreasing rank. This does not have any impact
       on correctness, but reportedly improves compression. The
       intuitive idea is that rows with few significant elements are
       easy to fit, so they should be inserted last, after the problem
       has become quite constrained by fitting the heavier rows. This
       heuristic is attributed to Ziegler. *)

    Array.fast_sort (fun (_, rank1, _) (_, rank2, _) ->
      compare rank2 rank1
    ) rows;

    (* Allocate a one-dimensional array of displacements. *)

    let displacement : int array =
      Array.make m 0
    in

    (* Allocate a one-dimensional, infinite array of values. Indices
       into this array are written [k]. *)

    let data : 'a InfiniteArray.t =
      InfiniteArray.make dummy
    in

    (* Determine whether [row] fits at offset [k] within the current [data]
       array, up to extension of this array. *)

    (* Note that this check always succeeds when [k] equals the length of
       the [data] array. Indeed, the loop is then skipped. This property
       guarantees the termination of the recursive function [fit] below. *)

    let fits k (row : 'a row) : bool =

      let d = InfiniteArray.extent data in

      let rec loop = function
        | [] ->
            true
        | (j, x) :: row ->

            (* [x] is a significant element. *)

            (* By hypothesis, [k + j] is nonnegative. If it is greater than or
               equal to the current length of the data array, stop -- the row
               fits. *)

            assert (k + j >= 0);

            if k + j >= d then
              true

            (* We now know that [k + j] is within bounds of the data
               array. Check whether it is compatible with the element [y] found
               there. If it is, continue. If it isn't, stop -- the row does not
               fit. *)

            else
              let y = InfiniteArray.get data (k + j) in
              if insignificant y || equal x y then
                loop row
              else
                false

      in
      loop row

    in

    (* Find the leftmost position where a row fits. *)

    (* If the leftmost significant element in this row is at offset [j],
       then we can hope to fit as far left as [-j] -- so this element
       lands at offset [0] in the data array. *)

    (* Note that displacements may be negative. This means that, for
       insignificant elements, accesses to the data array could fail: they could
       be out of bounds, either towards the left or towards the right. This is
       not a problem, as long as [get] is invoked only at significant
       elements. *)

    let rec fit k row : int =
      if fits k row then
        k
      else
        fit (k + 1) row
    in

    let fit row =
      match row with
      | [] ->
          0 (* irrelevant *)
      | (j, _) :: _ ->
          fit (-j) row
    in

    (* Write [row] at (compatible) offset [k]. *)

    let rec write k = function
      | [] ->
          ()
      | (j, x) :: row ->
          InfiniteArray.set data (k + j) x;
          write k row
    in

    (* Iterate over the sorted array of rows. Fit and write each row at
       the leftmost compatible offset. Update the displacement table. *)

    Array.iter (fun (i, _, row) ->
      let k = fit row in (* if [row] has leading insignificant elements, then [k] can be negative *)
      write k row;
      displacement.(i) <- encode k
    ) rows;

    (* Return the compressed tables. *)

    displacement, InfiniteArray.domain data

  (* [get ct i j] returns the value found at indices [i] and [j] in the
     compressed table [ct]. This function call is permitted only if the
     value found at indices [i] and [j] in the original table is
     significant -- otherwise, it could fail abruptly. *)

  (* Together, [compress] and [get] have the property that, if the value
     found at indices [i] and [j] in an uncompressed table [t] is
     significant, then [get (compress t) i j] is equal to that value. *)

  let get (displacement, data) i j =
    assert (0 <= i && i < Array.length displacement);
    let k = decode displacement.(i) in
    assert (0 <= k + j && k + j < Array.length data);
      (* failure of this assertion indicates an attempt to access an
         insignificant element that happens to be mapped out of the bounds
         of the [data] array. *)
    data.(k + j)

  (* [getget] is a variant of [get] which only requires read access,
     via accessors, to the two components of the table. *)

  let getget get_displacement get_data (displacement, data) i j =
    let k = decode (get_displacement displacement i) in
    get_data data (k + j)


end
module EngineTypes = struct
  (* This file defines several types and module types that are used in the
     specification of module [Engine]. *)

  (* --------------------------------------------------------------------------- *)

  (* It would be nice if we could keep the structure of stacks and environments
     hidden. However, stacks and environments must be accessible to semantic
     actions, so the following data structure definitions must be public. *)

  (* --------------------------------------------------------------------------- *)

  (* A stack is a linked list of cells. A sentinel cell -- which is its own
     successor -- is used to mark the bottom of the stack. The sentinel cell
     itself is not significant -- it contains dummy values. *)

  type ('state, 'semantic_value) stack = {

    (* The state that we should go back to if we pop this stack cell. *)

    (* This convention means that the state contained in the top stack cell is
       not the current state [env.current]. It also means that the state found
       within the sentinel is a dummy -- it is never consulted. This convention
       is the same as that adopted by the code-based back-end. *)

    state: 'state;

    (* The semantic value associated with the chunk of input that this cell
       represents. *)

    semv: 'semantic_value;

    (* The start and end positions of the chunk of input that this cell
       represents. *)

    startp: Lexing.position;
    endp: Lexing.position;

    (* The next cell down in the stack. If this is a self-pointer, then this
       cell is the sentinel, and the stack is conceptually empty. *)

    next: ('state, 'semantic_value) stack;

  }

  (* --------------------------------------------------------------------------- *)

  (* A parsing environment contains basically all of the automaton's state. *)

  type ('state, 'semantic_value, 'token) env = {


    (* The last token that was obtained from the lexer, with its starting and
       ending positions. *)

    token: Lexing.position * 'token * Lexing.position;

    (* A count of how many tokens were shifted since the beginning, or since
       the last [error] token was encountered. By convention, if [shifted]
       is (-1), then the current lookahead token is [error]. *)

    shifted: int;

    (* A copy of the value of [shifted] just before the most recent error
       was detected. This value is not used by the automaton itself, but
       is made accessible to semantic actions. *)

    previouserror: int;

    (* The stack. In [CodeBackend], it is passed around on its own,
       whereas, here, it is accessed via the environment. *)

    stack: ('state, 'semantic_value) stack;

    (* The current state. In [CodeBackend], it is passed around on its
       own, whereas, here, it is accessed via the environment. *)

    current: 'state;

  }

  (* --------------------------------------------------------------------------- *)

  (* This signature describes the parameters that must be supplied to the LR
     engine. *)

  module type TABLE = sig

    (* The type of automaton states. *)

    type state

    (* The type of tokens. These can be thought of as real tokens, that is,
       tokens returned by the lexer. They carry a semantic value. This type
       does not include the [error] pseudo-token. *)

    type token

    (* The type of terminal symbols. These can be thought of as integer codes.
       They do not carry a semantic value. This type does include the [error]
       pseudo-token. *)

    type terminal

    (* The type of semantic values. *)

    type semantic_value

    (* A token is conceptually a pair of a (non-[error]) terminal symbol and
       a semantic value. The following two functions are the pair projections. *)

    val token2terminal: token -> terminal
    val token2value: token -> semantic_value

    (* Even though the [error] pseudo-token is not a real token, it is a
       terminal symbol. Furthermore, for regularity, it must have a semantic
       value. *)

    val error_terminal: terminal
    val error_value: semantic_value

    (* The type of productions. *)

    type production

    (* If a state [s] has a default reduction on production [prod], then, upon
       entering [s], the automaton should reduce [prod] without consulting the
       lookahead token. The following function allows determining which states
       have default reductions. *)

    (* Instead of returning a value of a sum type -- either [DefRed prod], or
       [NoDefRed] -- it accepts two continuations, and invokes just one of
       them. This mechanism allows avoiding a memory allocation. *)

    val default_reduction:
      state ->
      ('env -> production -> 'answer) ->
      ('env -> 'answer) ->
      'env -> 'answer

    (* An LR automaton can normally take three kinds of actions: shift, reduce,
       or fail. (Acceptance is a particular case of reduction: it consists in
       reducing a start production.) *)

    (* There are two variants of the shift action. [shift/discard s] instructs
       the automaton to discard the current token, request a new one from the
       lexer, and move to state [s]. [shift/nodiscard s] instructs it to move to
       state [s] without requesting a new token. This instruction should be used
       when [s] has a default reduction on [#]. See [CodeBackend.gettoken] for
       details. *)

    (* This is the automaton's action table. It maps a pair of a state and a
       terminal symbol to an action. *)

    (* Instead of returning a value of a sum type -- one of shift/discard,
       shift/nodiscard, reduce, or fail -- this function accepts three
       continuations, and invokes just one them. This mechanism allows avoiding
       a memory allocation. *)

    (* In summary, the parameters to [action] are as follows:

       - the first two parameters, a state and a terminal symbol, are used to
         look up the action table;

       - the next parameter is the semantic value associated with the above
         terminal symbol; it is not used, only passed along to the shift
         continuation, as explained below;

       - the shift continuation expects an environment; a flag that tells
         whether to discard the current token; the terminal symbol that
         is being shifted; its semantic value; and the target state of
         the transition;

       - the reduce continuation expects an environment and a production;

       - the fail continuation expects an environment;

       - the last parameter is the environment; it is not used, only passed
         along to the selected continuation. *)

    val action:
      state ->
      terminal ->
      'semantic_value ->
      ('env -> bool -> terminal -> 'semantic_value -> state -> 'answer) ->
      ('env -> production -> 'answer) ->
      ('env -> 'answer) ->
      'env -> 'answer

    (* This is the automaton's goto table. It maps a pair of a state and a
       production to a new state.

       This convention is slightly different from the textbook approach. The
       goto table is usually indexed by a state and a non-terminal symbol. *)

    val goto: state -> production -> state

    (* By convention, a semantic action is responsible for:

       1. fetching whatever semantic values and positions it needs off the stack;

       2. popping an appropriate number of cells off the stack, as dictated
       by the length of the right-hand side of the production; this involves
       updating [env.stack];

       3. computing a new semantic value, as well as new start and end positions;

       4. pushing a new stack cell, which contains the three values
       computed in step 3; this again involves updating [env.stack]
       (only one update is necessary).

       Point 1 is essentially forced upon us: if semantic values were fetched
       off the stack by this interpreter, then the calling convention for
       semantic actions would be variadic: not all semantic actions would have
       the same number of arguments. The rest follows rather naturally. *)

    (* If production [prod] is an accepting production, then the semantic action
       is responsible for raising exception [Accept], instead of returning
       normally. This convention allows us to not distinguish between regular
       productions and accepting productions. All we have to do is catch that
       exception at top level. *)

    (* For introspection purposes, the user can iterate on all states of the
       grammar. *)
    val iter_states: (state -> unit) -> unit

    (* Entry points of grammar are allowed to raise [Accept result]. *)

    exception Accept of semantic_value

    (* Semantic actions are allowed to raise [Error]. *)

    exception Error

    type semantic_action =
        (state, semantic_value, token) env -> (state, semantic_value) stack

    val semantic_action: production -> semantic_action

    (* The LR engine can attempt error recovery. This consists in discarding
       tokens, just after an error has been successfully handled, until a token
       that can be successfully handled is found. This mechanism is optional.
       The following flag enables it. *)

    val recovery: bool

    (* The LR engine requires a number of hooks, which are used for logging. *)

    (* The comments below indicate the conventional messages that correspond
       to these hooks in the code-based back-end; see [CodeBackend]. *)

    module Log : sig

      (* State %d: *)

      val state: state -> unit

      (* Shifting (<terminal>) to state <state> *)

      val shift: terminal -> state -> unit

      (* Reducing a production should be logged either as a reduction
         event (for regular productions) or as an acceptance event (for
         start productions). *)

      (* Reducing production <production> / Accepting *)

      val reduce_or_accept: production -> unit

      (* Lookahead token is now <terminal> (<pos>-<pos>) *)

      val lookahead_token: Lexing.position -> terminal -> Lexing.position -> unit

      (* Initiating error handling *)

      val initiating_error_handling: unit -> unit

      (* Resuming error handling *)

      val resuming_error_handling: unit -> unit

      (* Handling error in state <state> *)

      val handling_error: state -> unit

      (* Discarding last token read (<terminal>) *)

      val discarding_last_token: terminal -> unit

    end

  end

  (* --------------------------------------------------------------------------- *)

  (* Step-by-step execution interface *)

  module type STEP_ENGINE = sig

    type state
    type token
    type semantic_value

    type step = [ `Step_run | `Step_error | `Step_action ]
    type feed = [ `Feed | `Feed_error ]

    type 'a parser = { env: (state, semantic_value, token) env; tag : 'a }

    type result =
      [ `Step   of step parser
      | `Feed   of feed parser
      | `Accept of semantic_value
      | `Reject ]

    val initial: state -> Lexing.position * token * Lexing.position -> step parser

    val step: step parser -> result

    val feed: feed parser -> Lexing.position * token * Lexing.position -> step parser

    val reduce_default: feed parser -> feed parser
  end

  module type QUERY = sig
    type token
    type state

    type terminal
    val index: token -> terminal
    val action: state -> terminal -> [`Shift of [`Discard | `Keep] * state | `Reduce | `Fail]
    val default_reduction: state -> bool
    val iter_states: (state -> unit) -> unit
  end

  (* This signature describes the LR engine. *)

  module type ENGINE = sig

    type state
    type token
    type semantic_value
    type terminal

    (* An entry point to the engine requires a start state, a lexer, and a lexing
       buffer. It either succeeds and produces a semantic value, or fails and
       raises [Error]. *)

    exception Error

    (* Entry points of the grammar leave the parser by raising [Accept]. *)

    exception Accept of semantic_value

    val entry:
      state ->
      (Lexing.lexbuf -> token) ->
      Lexing.lexbuf ->
      semantic_value

    include STEP_ENGINE
      with type state := state
       and type token := token
       and type semantic_value := semantic_value

    module Query : QUERY
      with type state := state
       and type token := token
       and type terminal = terminal

  end

end
module Engine : sig
  open EngineTypes

  (* The LR parsing engine. *)

  module Make (T : TABLE) : ENGINE with type state = T.state
                                    and type token = T.token
                                    and type semantic_value = T.semantic_value
                                    and type terminal := T.terminal

end = struct
  open EngineTypes

  (* The LR parsing engine. *)

  let fst3 (a,_,_) = a
  let snd3 (_,a,_) = a
  let thd3 (_,_,a) = a

  (* This module is used:

     - at compile time, if so requested by the user, via the --interpret options;
     - at run time, in the table-based back-end. *)

  module Make (T : TABLE) = struct

    (* This propagates type and exception definitions. *)

    include T

    let _eRR : exn =
      Error

    (* --------------------------------------------------------------------------- *)

    type feed = [ `Feed | `Feed_error ]
    type step = [ `Step_run | `Step_error | `Step_action ]

    type 'a parser = { env: (state, semantic_value, token) env; tag: 'a }

    type result =
      [ `Step   of step parser
      | `Feed   of feed parser
      | `Accept of semantic_value
      | `Reject ]

    (* --------------------------------------------------------------------------- *)

    (* [discard] takes a token off the input stream, queries the lexer
       for a new one, and stores it into [env.token], overwriting the
       previous token. If [env.shifted] has not yet reached its limit,
       it is incremented. *)

    let discard env token : (state, semantic_value, token) env =
      Log.lookahead_token
        (fst3 token)
        (T.token2terminal (snd3 token))
        (thd3 token);
      let shifted = env.shifted + 1 in
      let shifted =
        if shifted >= 0
        then shifted
        else env.shifted
      in
      { env with token; shifted }

    (* --------------------------------------------------------------------------- *)

    (* In the code-based back-end, the [run] function is sometimes responsible
       for pushing a new cell on the stack. This is motivated by code sharing
       concerns. In this interpreter, there is no such concern; [run]'s caller
       is always responsible for updating the stack. *)

    (* In the code-based back-end, there is a [run] function for each state
       [s]. This function can behave in two slightly different ways, depending
       on when it is invoked, or (equivalently) depending on [s].

       If [run] is invoked after shifting a terminal symbol (or, equivalently,
       if [s] has a terminal incoming symbol), then [run] discards a token,
       unless [s] has a default reduction on [#]. (Indeed, in that case,
       requesting the next token might drive the lexer off the end of the input
       stream.)

       If, on the other hand, [run] is invoked after performing a goto transition,
       or invoked directly by an entry point, then there is nothing to discard.

       These two cases are reflected in [CodeBackend.gettoken].

       Here, the code is structured in a slightly different way. It is up to
       the caller of [run] to indicate whether to discard a token. *)

    let rec run env : result =

      (* Log the fact that we just entered this state. *)

      let s = env.current in
      Log.state s;

      (* Examine what situation we are in. This case analysis is analogous to
         that performed in [CodeBackend.gettoken], in the sub-case where we do
         not have a terminal incoming symbol. *)

      T.default_reduction
        s
        reduce   (* there is a default reduction; perform it *)
        continue (* there is none; continue below *)
        env

    and continue env : result =

      (* There is no default reduction. Consult the current lookahead token
         so as to determine which action should be taken. *)

      (* Peeking at the first input token, without taking it off the input
         stream, is normally done by reading [env.token]. However, we check
         [env.shifted] first: if it is -1, then the lookahead token is the
         [error] token. *)

      (* Note that, if we just called [discard] above, then the lookahead
         token cannot be [error]. *)

      if env.shifted = (-1) then begin
        Log.resuming_error_handling();
        `Step { env; tag = `Step_error }
      end
      else
        action env

    (* --------------------------------------------------------------------------- *)

    (* When [action] is invoked, we know that the current state does not have
       a default reduction. We also know that the current lookahead token is
       not [error]: it is a real token, stored in [env.token]. *)

    and action env : result =

      (* We consult the two-dimensional action table, indexed by the
         current state and the current lookahead token, in order to
         determine which action should be taken. *)

      let token = snd3 env.token in
      T.action
        env.current                    (* determines a row *)
        (T.token2terminal token)       (* determines a column *)
        (T.token2value token)
        shift                          (* shift continuation *)
        reduce                         (* reduce continuation *)
        initiate                       (* failure continuation *)
        env

    (* --------------------------------------------------------------------------- *)

    (* This function takes care of shift transitions along a terminal symbol.
       (Goto transitions are taken care of within [reduce] below.) The symbol
       can be either an actual token or the [error] pseudo-token. *)

    and shift env
        (please_discard : bool)
        (terminal : terminal)
        (value : semantic_value)
        (s' : state)
        : result =

      (* Log the transition. *)

      Log.shift terminal s';

      let env =
        let startp, _, endp = env.token in
        { env with
          (* Push a new cell onto the stack, containing the identity of the
             state that we are leaving. *)
          stack = {
            state = env.current;
            semv = value;
            startp;
            endp;
            next = env.stack;
          };
          (* Switch to state [s']. *)
          current = s';
        }
      in
      if please_discard
      then `Feed { env; tag = `Feed }
      else `Step { env; tag = `Step_run }

    (* --------------------------------------------------------------------------- *)

    (* This function takes care of reductions. *)

    and reduce env (prod : production) : result =

      (* Log a reduction event. *)

      Log.reduce_or_accept prod;

      (* Invoke the semantic action. The semantic action is responsible for
         truncating the stack, updating the current state, producing a cell that
         contains a new semantic value, and raising [Accept] or [Error] if
         appropriate. *)

      (* If the semantic action raises [Error], we catch it immediately and
         initiate error handling. *)

      (* The apparently weird idiom used here is an encoding for a
         [let/unless] construct, which does not exist in ocaml. *)

      let env, success =
        try { env with stack = T.semantic_action prod env }, true
        with Error -> env, false
      in
      if success then begin

        (* By our convention, the semantic action is responsible for updating
           the stack. The state now found in the top stack cell is the return
           state. *)

        (* Perform a goto transition. The target state is determined
           by consulting the goto table at the return state and at
           production [prod]. *)

        let current = T.goto env.stack.state prod in
        `Step { env = { env with current }; tag = `Step_run }

      end
      else
        errorbookkeeping env


    (* --------------------------------------------------------------------------- *)

    (* The following functions deal with errors. *)

    (* [initiate] and [errorbookkeeping] initiate error handling. See the functions
       by the same names in [CodeBackend]. *)

    and initiate env : result =
      assert (env.shifted >= 0);
      if T.recovery && env.shifted = 0 then begin
        Log.discarding_last_token (T.token2terminal (snd3 env.token));
        `Feed { env; tag = `Feed_error }
      end
      else
        errorbookkeeping env

    and errorbookkeeping env =
      Log.initiating_error_handling();
      let env = { env with previouserror = env.shifted; shifted = -1 } in
      `Step { env; tag = `Step_error }

    (* [error] handles errors. *)

    and error env : result =

      (* Consult the column associated with the [error] pseudo-token in the
         action table. *)

      T.action
        env.current                    (* determines a row *)
        T.error_terminal               (* determines a column *)
        T.error_value
        error_shift                    (* shift continuation *)
        error_reduce                   (* reduce continuation *)
        error_fail                     (* failure continuation *)
        env

    and error_shift env please_discard terminal value s' =

      (* Here, [terminal] is [T.error_terminal], and [value] is [T.error_value]. *)

      assert (terminal = T.error_terminal && value = T.error_value);

      (* This state is capable of shifting the [error] token. *)

      Log.handling_error env.current;
      shift env please_discard terminal value s'

    and error_reduce env prod =

      (* This state is capable of performing a reduction on [error]. *)

      Log.handling_error env.current;
      reduce env prod

    and error_fail env =

      (* This state is unable to handle errors. Attempt to pop a stack
         cell. *)

      let cell = env.stack in
      let next = cell.next in
      if next == cell then

        (* The stack is empty. Die. *)

        `Reject

      else begin

        (* The stack is nonempty. Pop a cell, updating the current state
           with that found in the popped cell, and try again. *)

        let env = { env with stack = next; current = cell.state } in
        error env

      end

    (* --------------------------------------------------------------------------- *)

    (* Step-by-step execution interface *)

    let step s =
      try
        match s.tag with
        | `Step_run    -> run s.env
        | `Step_action -> action s.env
        | `Step_error  -> error s.env
      with
      | T.Accept v -> `Accept v
      | Error      -> `Reject

    let feed s token =
      match s.tag with
      | `Feed ->
        { env = discard s.env token; tag = `Step_run }
      | `Feed_error ->
        { env = { (discard s.env token) with shifted = 0 }; tag = `Step_action }

    let reduce_default s =
      let rec aux env =
        T.default_reduction env.current
          begin fun env prod ->
             let env', success =
               try { env with stack = T.semantic_action prod env }, true
               with Error -> env, false
             in
             if success then begin

               let current = T.goto env'.stack.state prod in
               aux { env' with current }

             end
             else
               env
          end
          (fun env -> env)
          env
      in
      {s with env = aux s.env}


    let initial s (start_p,t,curr_p as token) =
      let rec empty = {
        state = s;                 (* dummy *)
        semv = T.error_value;      (* dummy *)
        startp = Lexing.dummy_pos; (* dummy *)
        endp = Lexing.dummy_pos;   (* dummy *)
        next = empty;
      } in

      (* Log our first lookahead token. *)

      Log.lookahead_token start_p (T.token2terminal t) curr_p;

      (* Build an initial environment. *)

      let env = {
        token = token;
        shifted = max_int;
        previouserror = max_int;
        stack = empty;
        current = s;
      } in

      { env; tag = `Step_run }

    (* --------------------------------------------------------------------------- *)

    let entry
        (s : state)
        (lexer : Lexing.lexbuf -> token)
        (lexbuf : Lexing.lexbuf)
        : semantic_value =

      (* Build an empty stack. This is a dummy cell, which is its own
         successor. Its fields other than [next] contain dummy values. *)

      let rec empty = {
        state = s;                          (* dummy *)
        semv = T.error_value;               (* dummy *)
        startp = lexbuf.Lexing.lex_start_p; (* dummy *)
        endp = lexbuf.Lexing.lex_curr_p;    (* dummy *)
        next = empty;
      } in

      (* Perform an initial call to the lexer. *)

      let token : token =
        lexer lexbuf
      in

      let { Lexing.lex_start_p; lex_curr_p } = lexbuf in

      (* Log our first lookahead token. *)

      Log.lookahead_token lex_start_p (T.token2terminal token) lex_curr_p;

      (* Build an initial environment. *)

      let env = {
        token = (lex_start_p, token, lex_curr_p);
        shifted = max_int;
        previouserror = max_int;
        stack = empty;
        current = s;
      } in

      (* Run. Catch [Accept], which represents normal termination. Let [Error]
         escape. *)

      try

        (* If ocaml offered a [match/with] construct with zero branches, this is
           what we would use here, since the type [void] has zero cases. *)

        let rec aux : result -> semantic_value = function
          | `Step p -> aux (step p)
          | `Accept v -> v
          | `Reject -> raise _eRR
          | `Feed p ->
            let token = lexer lexbuf in
            let { Lexing.lex_start_p; lex_curr_p } = lexbuf in
            aux (step (feed p (lex_start_p, token, lex_curr_p)))
        in
        aux (run env)

      with T.Accept v -> v

    (* --------------------------------------------------------------------------- *)

    (* Query interface *)

    module Query = struct
      type terminal = T.terminal
      let index = T.token2terminal

      let action state term =
        T.action state term ()
          (fun () discard _term () state ->
             `Shift ((if discard then `Discard else `Keep), state))
          (fun () _prod -> `Reduce)
          (fun () -> `Fail)
          ()

      let default_reduction state =
        T.default_reduction state
          (fun () _prod -> true)
          (fun () -> false)
          ()

      let iter_states = T.iter_states
    end

    (* --------------------------------------------------------------------------- *)

  end


end
module TableFormat = struct
  (* This signature defines the format of the parse tables. It is used as
     an argument to [TableInterpreter]. *)

  module type TABLES = sig

    (* This is the parser's type of tokens. *)

    type token

    (* This is the type of parser's semantic values. *)

    type semantic_value

    (* This maps a token to its internal (generation-time) integer code. *)

    val token2terminal: token -> int

    (* This is the integer code for the error pseudo-token. *)

    val error_terminal: int

    (* Distinguished semantic_value tagging invalid stack frame *)

    val error_value: semantic_value

    (* This maps a token to its semantic value. *)

    val token2value: token -> semantic_value

    (* Number of states (labelled from 0 to n-1) provided only for information *)

    val number_of_states: int

    (* Traditionally, an LR automaton is described by two tables, namely, an
       action table and a goto table. See, for instance, the Dragon book.

       The action table is a two-dimensional matrix that maps a state and a
       lookahead token to an action. An action is one of: shift to a certain
       state, reduce a certain production, accept, or fail.

       The goto table is a two-dimensional matrix that maps a state and a
       non-terminal symbol to either a state or undefined. By construction, this
       table is sparse: its undefined entries are never looked up. A compression
       technique is free to overlap them with other entries.

       In Menhir, things are slightly different. If a state has a default
       reduction on token [#], then that reduction must be performed without
       consulting the lookahead token. As a result, we must first determine
       whether that is the case, before we can obtain a lookahead token and use it
       as an index in the action table.

       Thus, Menhir's tables are as follows.

       A one-dimensional default reduction table maps a state to either ``no
       default reduction'' (encoded as: 0) or ``by default, reduce prod''
       (encoded as: 1 + prod). The action table is looked up only when there
       is no default reduction. *)

    val default_reduction: PackedIntArray.t

    (* Menhir follows Dencker, Dürre and Heuft, who point out that, although the
       action table is not sparse by nature (i.e., the error entries are
       significant), it can be made sparse by first factoring out a binary error
       matrix, then replacing the error entries in the action table with undefined
       entries. Thus:

       A two-dimensional error bitmap maps a state and a terminal to either
       ``fail'' (encoded as: 0) or ``do not fail'' (encoded as: 1). The action
       table, which is now sparse, is looked up only in the latter case. *)

    (* The error bitmap is flattened into a one-dimensional table; its width is
       recorded so as to allow indexing. The table is then compressed via
       [PackedIntArray]. The bit width of the resulting packed array must be
       [1], so it is not explicitly recorded. *)

    (* The error bitmap does not contain a column for the [#] pseudo-terminal.
       Thus, its width is [Terminal.n - 1]. We exploit the fact that the integer
       code assigned to [#] is greatest: the fact that the right-most column
       in the bitmap is missing does not affect the code for accessing it. *)

    val error: int (* width of the bitmap *) * string (* second component of [PackedIntArray.t] *)

    (* A two-dimensional action table maps a state and a terminal to one of
       ``shift to state s and discard the current token'' (encoded as: s | 10),
       ``shift to state s without discarding the current token'' (encoded as: s |
       11), or ``reduce prod'' (encoded as: prod | 01). *)

    (* The action table is first compressed via [RowDisplacement], then packed
       via [PackedIntArray]. *)

    (* Like the error bitmap, the action table does not contain a column for the
       [#] pseudo-terminal. *)

    val action: PackedIntArray.t * PackedIntArray.t

    (* A one-dimensional lhs table maps a production to its left-hand side (a
       non-terminal symbol). *)

    val lhs: PackedIntArray.t

    (* A two-dimensional goto table maps a state and a non-terminal symbol to
       either undefined (encoded as: 0) or a new state s (encoded as: 1 + s). *)

    (* The goto table is first compressed via [RowDisplacement], then packed
       via [PackedIntArray]. *)

    val goto: PackedIntArray.t * PackedIntArray.t

    (* A one-dimensional semantic action table maps productions to semantic
       actions. The calling convention for semantic actions is described in
       [EngineTypes]. *)

    val semantic_action: ((int, semantic_value, token) EngineTypes.env ->
                          (int, semantic_value) EngineTypes.stack) array


    (* The parser defines its own [Accept] exception. This exception is raised
       and caught by the engine inside entry points to return a final result. *)

    exception Accept of semantic_value

    (* The parser defines its own [Error] exception. This exception can be
       raised by semantic actions and caught by the engine, and raised by the
       engine towards the final user. *)

    exception Error

    (* The parser indicates whether to perform error recovery. *)

    val recovery: bool

    (* The parser indicates whether to generate a trace. Generating a
       trace requires two extra tables, which respectively map a
       terminal symbol and a production to a string. *)

    val trace: (string array * string array) option

  end


end
module TableInterpreter : sig
  (* This module instantiates the generic [Engine] with a thin decoding layer
     for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

  (* The exception [Accept] is no longer pre-declared here: with --typed-values,
     the Table backend now generates an exception parametrized over the type of
     semantic values. *)

  (* This functor is invoked by the generated parser. *)

  module Make (T : TableFormat.TABLES)

  : EngineTypes.ENGINE with type state = int
                        and type token = T.token
                        and type semantic_value = T.semantic_value
                        and type terminal := int


end = struct
  (* This module instantiates the generic [Engine] with a thin decoding layer
     for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

  (* The exception [Accept] is no longer pre-declared here: with --typed-values,
     the Table backend now generates an exception parametrized over the type of
     semantic values. *)

  (* This functor is invoked by the generated parser. *)

  module Make (T : TableFormat.TABLES)

  = Engine.Make (struct

    type state =
        int

    type token =
        T.token

    type terminal =
        int

    type semantic_value =
      T.semantic_value

    let token2terminal =
      T.token2terminal

    let token2value =
      T.token2value

    let error_terminal =
      T.error_terminal

    let error_value =
      T.error_value

    type production =
        int

    let default_reduction state defred nodefred env =
      let code = PackedIntArray.get T.default_reduction state in
      if code = 0 then
        nodefred env
      else
        defred env (code - 1)

    (* This auxiliary function helps access a compressed, two-dimensional
       matrix, like the action and goto tables. *)

    let unmarshal2 table i j =
      RowDisplacement.getget
        PackedIntArray.get
        PackedIntArray.get
        table
        i j

    (* This auxiliary function helps access a flattened, two-dimensional
       matrix, like the error bitmap. *)

    let unflatten (n, data) i j =
      PackedIntArray.get1 data (n * i + j)

    let action state terminal value shift reduce fail env =
      match unflatten T.error state terminal with
      | 1 ->
          let action = unmarshal2 T.action state terminal in
          let opcode = action land 0b11
          and param = action lsr 2 in
          if opcode >= 0b10 then
            (* 0b10 : shift/discard *)
            (* 0b11 : shift/nodiscard *)
            let please_discard = (opcode = 0b10) in
            shift env please_discard terminal value param
          else
            (* 0b01 : reduce *)
            (* 0b00 : cannot happen *)
            reduce env param
      | c ->
          assert (c = 0);
          fail env

    let goto state prod =
      let code = unmarshal2 T.goto state (PackedIntArray.get T.lhs prod) in
      (* code = 1 + state *)
      code - 1

    let iter_states f =
      for i = 0 to T.number_of_states - 1 do
        f i
      done

    exception Accept = T.Accept

    exception Error = T.Error

    type semantic_action =
      (state, semantic_value, token) EngineTypes.env ->
       (state, semantic_value) EngineTypes.stack

    let semantic_action prod =
      T.semantic_action.(prod)

    let recovery =
      T.recovery

    module Log = struct

      open Printf

      let state state =
        match T.trace with
        | Some _ ->
            fprintf stderr "State %d:\n%!" state
        | None ->
            ()

      let shift terminal state =
        match T.trace with
        | Some (terminals, _) ->
            fprintf stderr "Shifting (%s) to state %d\n%!" terminals.(terminal) state
        | None ->
            ()

      let reduce_or_accept prod =
        match T.trace with
        | Some (_, productions) ->
            fprintf stderr "%s\n%!" productions.(prod)
        | None ->
            ()

      let lookahead_token start_p token curr_p =
        match T.trace with
        | Some (terminals, _) ->
            fprintf stderr "Lookahead token is now %s (%d-%d)\n%!"
              terminals.(token)
              start_p.Lexing.pos_cnum
              curr_p.Lexing.pos_cnum
        | None ->
            ()

      let initiating_error_handling () =
        match T.trace with
        | Some _ ->
            fprintf stderr "Initiating error handling\n%!"
        | None ->
            ()

      let resuming_error_handling () =
        match T.trace with
        | Some _ ->
            fprintf stderr "Resuming error handling\n%!"
        | None ->
            ()

      let handling_error state =
        match T.trace with
        | Some _ ->
            fprintf stderr "Handling error in state %d\n%!" state
        | None ->
            ()

      let discarding_last_token token =
        match T.trace with
        | Some (terminals, _) ->
            fprintf stderr "Discarding last token read (%s)\n%!" terminals.(token)
        | None ->
            ()

    end

  end)

end
module Convert : sig
  (* An ocamlyacc-style, or Menhir-style, parser requires access to
     the lexer, which must be parameterized with a lexing buffer, and
     to the lexing buffer itself, where it reads position information. *)

  (* This traditional API is convenient when used with ocamllex, but
     inelegant when used with other lexer generators. *)

  type ('token, 'semantic_value) traditional =
      (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'semantic_value

  (* This revised API is independent of any lexer generator. Here, the
     parser only requires access to the lexer, and the lexer takes no
     parameters. The tokens returned by the lexer may contain position
     information. *)

  type ('token, 'semantic_value) revised =
      (unit -> 'token) -> 'semantic_value

  (* --------------------------------------------------------------------------- *)

  (* Converting a traditional parser, produced by ocamlyacc or Menhir,
     into a revised parser. *)

  (* A token of the revised lexer is essentially a triple of a token
     of the traditional lexer (or raw token), a start position, and
     and end position. The three [get] functions are accessors. *)

  (* We do not require the type ['token] to actually be a triple type.
     This enables complex applications where it is a record type with
     more than three fields. It also enables simple applications where
     positions are of no interest, so ['token] is just ['raw_token]
     and [get_startp] and [get_endp] return dummy positions. *)

  val traditional2revised:
    ('token -> 'raw_token) ->
    ('token -> Lexing.position) ->
    ('token -> Lexing.position) ->
    ('raw_token, 'semantic_value) traditional ->
    ('token, 'semantic_value) revised

  (* --------------------------------------------------------------------------- *)

  (* Converting a revised parser back to a traditional parser. *)

  val revised2traditional:
    ('raw_token -> Lexing.position -> Lexing.position -> 'token) ->
    ('token, 'semantic_value) revised ->
    ('raw_token, 'semantic_value) traditional

  (* --------------------------------------------------------------------------- *)

  (* Simplified versions of the above, where concrete triples are used. *)

  module Simplified : sig

    val traditional2revised:
      ('token, 'semantic_value) traditional ->
      ('token * Lexing.position * Lexing.position, 'semantic_value) revised

    val revised2traditional:
      ('token * Lexing.position * Lexing.position, 'semantic_value) revised ->
      ('token, 'semantic_value) traditional

  end

end = struct
  (* An ocamlyacc-style, or Menhir-style, parser requires access to
     the lexer, which must be parameterized with a lexing buffer, and
     to the lexing buffer itself, where it reads position information. *)

  (* This traditional API is convenient when used with ocamllex, but
     inelegant when used with other lexer generators. *)

  type ('token, 'semantic_value) traditional =
      (Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'semantic_value

  (* This revised API is independent of any lexer generator. Here, the
     parser only requires access to the lexer, and the lexer takes no
     parameters. The tokens returned by the lexer may contain position
     information. *)

  type ('token, 'semantic_value) revised =
      (unit -> 'token) -> 'semantic_value

  (* --------------------------------------------------------------------------- *)

  (* Converting a traditional parser, produced by ocamlyacc or Menhir,
     into a revised parser. *)

  (* A token of the revised lexer is essentially a triple of a token
     of the traditional lexer (or raw token), a start position, and
     and end position. The three [get] functions are accessors. *)

  (* We do not require the type ['token] to actually be a triple type.
     This enables complex applications where it is a record type with
     more than three fields. It also enables simple applications where
     positions are of no interest, so ['token] is just ['raw_token]
     and [get_startp] and [get_endp] return dummy positions. *)

  let traditional2revised
    (get_raw_token : 'token -> 'raw_token)
    (get_startp    : 'token -> Lexing.position)
    (get_endp      : 'token -> Lexing.position)
    (parser : ('raw_token, 'semantic_value) traditional)
  : ('token, 'semantic_value) revised =

    (* Accept a revised lexer. *)

    fun (lexer : unit -> 'token) ->

      (* Create a dummy lexing buffer. *)

      let lexbuf : Lexing.lexbuf =
        Lexing.from_string ""
      in

      (* Wrap the revised lexer as a traditional lexer. A traditional
         lexer returns a raw token and updates the fields of the lexing
         buffer with new positions, which will be read by the parser. *)

      let lexer (lexbuf : Lexing.lexbuf) : 'raw_token =
        let token : 'token = lexer() in
        lexbuf.Lexing.lex_start_p <- get_startp token;
        lexbuf.Lexing.lex_curr_p <- get_endp token;
        get_raw_token token
      in

      (* Invoke the traditional parser. *)

      parser lexer lexbuf

  (* --------------------------------------------------------------------------- *)

  (* Converting a revised parser back to a traditional parser. *)

  let revised2traditional
    (make_token : 'raw_token -> Lexing.position -> Lexing.position -> 'token)
    (parser : ('token, 'semantic_value) revised)
  : ('raw_token, 'semantic_value) traditional =

    (* Accept a traditional lexer and a lexing buffer. *)

    fun (lexer : Lexing.lexbuf -> 'raw_token) (lexbuf : Lexing.lexbuf) ->

      (* Wrap the traditional lexer as a revised lexer. *)

      let lexer () : 'token =
        let token : 'raw_token = lexer lexbuf in
        make_token token lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p
      in

      (* Invoke the revised parser. *)

      parser lexer

  (* --------------------------------------------------------------------------- *)

  (* Simplified versions of the above, where concrete triples are used. *)

  module Simplified = struct

    let traditional2revised parser =
      traditional2revised
        (fun (token, _, _)  -> token)
        (fun (_, startp, _) -> startp)
        (fun (_, _, endp)   -> endp)
        parser

    let revised2traditional parser =
      revised2traditional
        (fun token startp endp -> (token, startp, endp))
        parser

  end

end
