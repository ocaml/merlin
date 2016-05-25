open Std

val make : on_read:(Unix.file_descr -> 'a) ->
           input:Unix.file_descr ->
           output:Unix.file_descr ->
           (unit -> Json.json option) * (Json.json -> unit)
