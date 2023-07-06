(** An all-or-nothing cache mechanism that can be used for any phase *)

module type S = sig
  type t
  (** Phase input *)

  type output
  (** Phase output *)

  val f : t -> output
  (** Phase computation *)

  val title : string
  (** Phase title for logging *)

  module Fingerprint : sig
    type input

    type t
    (** Fingerprint used to determine whether the cache should be invalidated *)

    val make : input -> (t, string) result
    (** Creates a fingerprint from the phase input *)

    val equal : t -> t -> bool
    (** Determines whether two fingerprints are the same *)
  end
  with type input := t
end

module With_cache (Phase : S) : sig
  type t = { output : Phase.output; cache_was_hit : bool }

  val apply :
    ?cache_disabling:string option -> ?force_invalidation:bool -> Phase.t -> t
  (** [apply ~cache_disabling ~force_invalidation phase_input] runs the phase
      computation [Phase.f phase_input], if there's some [cache_disabling].
      Otherwise, the phase computation is run with a cache mechanism. Whether
      the cache is invalidated depends on the outcome of a [Phase.Fingerprint]
      comparison between the current fingerprint and the last one. Additionally,
      the invalidation of the cache can be forced by setting the
      force_invalidation parameter to true.*)
end
