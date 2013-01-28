(** {0 Outline parser}
  * Définititions auxiliaires utilisées par le parser d'outline *)

(** Les constructions du code source sont découpées en "chunk" de différentes
  * kinds. *)
type kind =
  | Enter_module (* module _ = struct *)
  | Leave_module (* end *)
  | Definition   (* let / type / … *)
  | Rollback     (* and … : nous devons retourner à la définition précédente
                  *         pour retrouver le /genre/ de définition *)
  | Done         (* EOF rencontré après une construction syntaxiquement correcte *)
  | Unterminated (* La construction syntaxique n'est pas terminée *)
  | Exception of exn (* Une exception est survenue dans le parser, à traiter en amont *)

(** Le parser d'outline fonctionne par effet de bord :
  * - en général, les productions n'ont aucune action sémantique
  * - quand le parser rencontre une construction syntaxique qu'il sait délimiter
  *   (ci-dessus: module ou définition), il lève l'exception Chunk.
  * La position renvoyée est celle du dernier token du chunk, afin de distinguer
  * l'éventuel token lookahead consommé par le parser et de le rajouter en début
  * du lexing buffer.
  * EX: let a = 5 type t = ...
  *                  ^ |CHUNK: let a = 5|
  * Le parser ne lève Chunk qu'après type, il faut réajouter le token type en
  * début de flux.
  *)
exception Chunk of kind * Lexing.position

(** Si !filter_first > 0, le parser ne lève pas d'exception mais décrémente
  * filter_first. Cela permet d'implanter le rollback quand le code source est
  * donné en plusieurs fois.
  * Ainsi "let x = 5" puis "and y = 6" est analysée en :
  * - "let x = 5" : raise Chunk (Definition,_)
  * - "and …" : raise Chunk (Rollback,_)
  * - filter_first := 1
  * - "let x = 5 and y = 6" : raise Chunk(Definition,_)
  * -            ^ une exception Chunk(Definition) est ignorée ici
  *                et filter_first décrémenté
  *)
val filter_first : int ref

(** Utilisé pour ignorer les modules de première classe :
  * la construction :
  * "let module = … in " permet de définir un module à l'intérieur d'une
  * définition, mais le parser d'outline ne sait pas interrompre une définition
  * (elle est soit complètement correcte, soit complètement fausse).
  * À chaque entrée dans ce genre de définition nesting est incrémenté, à la
  * sortie nesting est décrémenté. Une définition de module est reportée
  * uniquement si !nesting = 0.
  *) 
val nesting : int ref

(** Appelé pour initialiser le parser avant chaque parser.
  * filter_first := rollback; nesting := 0
  *)
val reset : rollback:int -> unit -> unit

(** Augmente nesting *)
val enter : unit -> unit
(** Décrémente nesting *)
val leave : unit -> unit
(** Lève l'exception si !nesting = 0 et !filter_first = 0 *)
val emit_top : kind -> Lexing.position -> unit

(** {0 Routines auxiliaires d'entrée/sortie } *)

val pos_to_json : Lexing.position -> Json.json
val pos_of_json : Json.json -> [ `Line of (int * int) ]

(** Simplifie la construction d'un formatter vers une chaîne *)
val ppf_to_string : unit -> Format.formatter * (unit -> string)
