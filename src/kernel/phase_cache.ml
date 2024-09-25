let { Logger.log } = Logger.for_section "Phase cache"

module type S = sig
  type t
  type output

  val f : t -> output

  val title : string

  module Fingerprint : sig
    type input
    type t

    val make : input -> (t, string) result
    val equal : t -> t -> bool
  end
  with type input := t
end

module With_cache (Phase : S) = struct
  type t = { output : Phase.output; cache_was_hit : bool }
  type cache = { fingerprint : Phase.Fingerprint.t; output : Phase.output }

  let cache = ref None

  let apply ?(cache_disabling = None) ?(force_invalidation = false) input =
    let title = Phase.title in
    match cache_disabling with
    | Some reason ->
      log ~title "Cache is disabled: %s" reason;
      cache := None;
      let output = Phase.f input in
      { output; cache_was_hit = false }
    | None -> (
      let new_fingerprint = Phase.Fingerprint.make input in
      match (!cache, new_fingerprint) with
      | None, Ok new_fingerprint ->
        log ~title "Cache wasn't populated\n";
        let output = Phase.f input in
        cache := Some { fingerprint = new_fingerprint; output };
        { output; cache_was_hit = false }
      | Some { fingerprint; output }, Ok new_fingerprint ->
        if
          (not force_invalidation)
          && Phase.Fingerprint.equal fingerprint new_fingerprint
        then (
          log ~title "Cache hit";
          { output; cache_was_hit = true })
        else (
          log ~title "Cache invalidation";
          let output = Phase.f input in
          cache := Some { fingerprint = new_fingerprint; output };
          { output; cache_was_hit = false })
      | (None | Some _), Error err ->
        log ~title "Cache workflow is incomplete: %s" err;
        cache := None;
        let output = Phase.f input in
        { output; cache_was_hit = false })
end
