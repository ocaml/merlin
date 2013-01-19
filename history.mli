(** {1 Historique}
  * Une sorte de zipper : maintient et synchronise des listes des différentes
  * version d'un objet.
  * où curseur
  *)
type 'a t

(* Un historique vide *)
val empty : 'a t
(** Construit un historique à partir d'une liste.
  * Le curseur est placé au début : les éléments de la liste représente le
  * futur.
  *)
val of_list : 'a list -> 'a t


(** Découpe [--o--] en [--o] [o--] *)
val split : 'a t -> 'a t * 'a t
(** Supprime le future : [--o--] en [--o] *)
val cutoff : 'a t -> 'a t

(** Élément à gauche du curseur
  * (si la dernière opération est une insertion, la valeur insérée est retournée)
  *)
val prev : 'a t -> 'a option
(** Élément à droite du curseur
  * (None le plus souvent)
  *)
val next : 'a t -> 'a option
(** Renvoie le passé *)
val prevs : 'a t -> 'a list
(** Renvoie le futur *)
val nexts : 'a t -> 'a list

(** offset : "date", c'est-à-dire nombre d'éléments dans le passé *)
val offset : 'a t -> int
val seek_offset : int -> 'a t -> 'a t

(** Se déplace d'une étape en avant, renvoie l'élément et l'historique décalé
  * s'il existe *)
val forward  : 'a t -> ('a * 'a t) option 
(** Se déplace d'une étape en arrière, renvoie l'élément et l'historique décalé
  * s'il existe *)
val backward : 'a t -> ('a * 'a t) option 
(** Se déplace d'un nombre arbitraire d'étape *)
val move : int -> 'a t -> 'a t

(** Se déplace jusqu'à un élément particulier; le premier argument est une
  * fonction de comparaison sur les éléments de l'historique. *)
val seek : ('a -> int) -> 'a t -> 'a t

(** Ajoute un élément à gauche du curseur :
  * insert w [..zyx|abc..] = [..zyxw|abc..] *)
val insert : 'a -> 'a t -> 'a t
(** Supprime l'élément à gauche du curseur, si possible *)
val remove : 'a t -> ('a * 'a t) option
(** Modifie l'élément à gauche du curseur *)
val modify : ('a -> 'a) -> 'a t -> 'a t

type 'a sync

(** {1 Synchronisation} *)
module Sync :
sig
  val origin : 'a sync

  val at : 'a t -> 'a sync
  val item : 'a sync -> 'a option

  (* rewind : prj a b
   * Remonte les historiques [a] et [b] jusqu'à trouver un point commun,
   * l'origine éventuellement (offset 0)
   *) 
  val rewind : ('b -> 'a sync) -> 'a t -> 'b t -> 'a t * 'b t
  (* nearest : prj a b 
   * Trouve le point de [a] le plus du point actuel [b]
   *)
  val nearest : ('b -> 'a sync) -> 'a t -> 'b t -> 'a t * 'b t

  val left : ('b -> 'a sync) -> 'a t -> 'b t -> 'a t
  val right : ('b -> 'a sync) -> 'a t -> 'b t -> 'b t
end

(** {1 Misc: intégration au lexer} *)
type pos = Lexing.position
type 'a loc = 'a * pos * pos

val wrap_lexer : ?filter:('a -> bool) -> ?bufpos:Lexing.position ref ->
  'a loc t ref -> (Lexing.lexbuf -> 'a) -> (Lexing.lexbuf -> 'a)

val current_pos : ?default:Lexing.position -> 'a loc t -> pos
val seek_pos : pos -> 'a loc t -> 'a loc t

