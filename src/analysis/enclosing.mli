(** This module contains the "enclosing" implementation.

    The enclosing of a range, is the set of parent ranges that would be logical
    to expand to. In LSP, it is called "selection range".

    Contrary to "type enclosings", which need to be typable nodes from the
    parsetree, "enclosings" are not necessarily nodes: They can split the
    inclusion of a node in multiple ranges. For instance, with square brackets
    denoting ranges, we want to include a range for each [let ... in]:

    {[
    [[[[let x1 = 1 in]
        let x2 = 1 in]
        let x3 = 1 in]
        x1 + x2 + x3]
    ]}

    Note that the implementation would be strengthened by using a non-ppxed
    version of [Mbrowser.t].
*)

(** Generate the list of ranges from:
    - A range as "starting point",
    - A list of parents for the current node
 *)
val locs : Location.t -> Mbrowse.t -> Location.t list
