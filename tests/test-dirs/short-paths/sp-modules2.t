  $ cat > bar.mli <<EOF
  > module A : sig
  >   type t
  >   module B : sig
  >     module C : sig
  >       type s
  >       module D : sig
  >         val foo : t -> s
  >       end
  >     end
  >   end
  > end

  $ cat >bar.mli <<EOF
  > module A : sig
  >   type t
  >   module B : sig
  >     module C : sig
  >       type s
  >     end
  >     module D : sig
  >       val foo : t -> C.s
  >     end
  >   end
  > end
  > EOF

  $ $OCAMLC -c bar.mli -o bar.cmi

  $ cat > foo.ml <<EOF
  > open Bar
  > open A.B
  > let f x = D.foo x
  > EOF

> -log-file - -log-section discourse,type-enclosing,short-paths 
  $ $MERLIN single type-enclosing -short-paths -position 3:13 \
  > -filename foo.ml < foo.ml | jq '.value[0]'
  {
    "start": {
      "line": 3,
      "col": 10
    },
    "end": {
      "line": 3,
      "col": 15
    },
    "type": "A.t -> C.s",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -short-paths -log-file - -log-section discourse-verbose -position 3:16 \
  > -filename foo.ml < foo.ml | jq '.value[0]'
  # 0.01 discourse-verbose - U1
  U1: path module used in file: Stdlib! (File "command line", line 1)
  # 0.01 discourse-verbose - U3
  U3: open module Stdlib!
  # 0.01 discourse-verbose - U3
  U3: value raise/284 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value raise/284 [Stdlib!.raise] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value raise_notrace/285 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value raise_notrace/285 [Stdlib!.raise_notrace] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value invalid_arg/286 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value invalid_arg/286 [Stdlib!.invalid_arg] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value failwith/287 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value failwith/287 [Stdlib!.failwith] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value =/301 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value =/301 [Stdlib!.=] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value <>/302 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value <>/302 [Stdlib!.<>] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value </303 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value </303 [Stdlib!.<] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value >/304 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value >/304 [Stdlib!.>] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value <=/305 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value <=/305 [Stdlib!.<=] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value >=/306 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value >=/306 [Stdlib!.>=] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value compare/307 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value compare/307 [Stdlib!.compare] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value min/308 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value min/308 [Stdlib!.min] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value max/309 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value max/309 [Stdlib!.max] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ==/310 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ==/310 [Stdlib!.==] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value !=/311 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value !=/311 [Stdlib!.!=] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value not/312 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value not/312 [Stdlib!.not] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value &&/313 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value &&/313 [Stdlib!.&&] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ||/314 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ||/314 [Stdlib!.||] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __LOC__/315 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __LOC__/315 [Stdlib!.__LOC__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __FILE__/316 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __FILE__/316 [Stdlib!.__FILE__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __LINE__/317 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __LINE__/317 [Stdlib!.__LINE__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __MODULE__/318 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __MODULE__/318 [Stdlib!.__MODULE__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __POS__/319 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __POS__/319 [Stdlib!.__POS__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __FUNCTION__/320 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __FUNCTION__/320 [Stdlib!.__FUNCTION__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __LOC_OF__/321 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __LOC_OF__/321 [Stdlib!.__LOC_OF__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __LINE_OF__/322 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __LINE_OF__/322 [Stdlib!.__LINE_OF__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value __POS_OF__/323 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value __POS_OF__/323 [Stdlib!.__POS_OF__] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value |>/324 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value |>/324 [Stdlib!.|>] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value @@/325 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value @@/325 [Stdlib!.@@] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ~-/326 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ~-/326 [Stdlib!.~-] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ~+/327 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ~+/327 [Stdlib!.~+] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value succ/328 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value succ/328 [Stdlib!.succ] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value pred/329 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value pred/329 [Stdlib!.pred] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value +/330 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value +/330 [Stdlib!.+] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value -/331 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value -/331 [Stdlib!.-] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value */332 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value */332 [Stdlib!.*] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value //333 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value //333 [Stdlib!./] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value mod/334 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value mod/334 [Stdlib!.mod] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value abs/335 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value abs/335 [Stdlib!.abs] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value max_int/336 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value max_int/336 [Stdlib!.max_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value min_int/337 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value min_int/337 [Stdlib!.min_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value land/338 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value land/338 [Stdlib!.land] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value lor/339 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value lor/339 [Stdlib!.lor] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value lxor/340 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value lxor/340 [Stdlib!.lxor] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value lnot/341 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value lnot/341 [Stdlib!.lnot] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value lsl/342 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value lsl/342 [Stdlib!.lsl] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value lsr/343 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value lsr/343 [Stdlib!.lsr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value asr/344 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value asr/344 [Stdlib!.asr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ~-./345 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ~-./345 [Stdlib!.~-.] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ~+./346 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ~+./346 [Stdlib!.~+.] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value +./347 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value +./347 [Stdlib!.+.] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value -./348 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value -./348 [Stdlib!.-.] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value *./349 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value *./349 [Stdlib!.*.] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value /./350 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value /./350 [Stdlib!./.] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value **/351 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value **/351 [Stdlib!.**] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value sqrt/352 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value sqrt/352 [Stdlib!.sqrt] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value exp/353 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value exp/353 [Stdlib!.exp] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value log/354 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value log/354 [Stdlib!.log] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value log10/355 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value log10/355 [Stdlib!.log10] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value expm1/356 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value expm1/356 [Stdlib!.expm1] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value log1p/357 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value log1p/357 [Stdlib!.log1p] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value cos/358 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value cos/358 [Stdlib!.cos] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value sin/359 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value sin/359 [Stdlib!.sin] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value tan/360 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value tan/360 [Stdlib!.tan] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value acos/361 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value acos/361 [Stdlib!.acos] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value asin/362 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value asin/362 [Stdlib!.asin] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value atan/363 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value atan/363 [Stdlib!.atan] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value atan2/364 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value atan2/364 [Stdlib!.atan2] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value hypot/365 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value hypot/365 [Stdlib!.hypot] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value cosh/366 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value cosh/366 [Stdlib!.cosh] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value sinh/367 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value sinh/367 [Stdlib!.sinh] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value tanh/368 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value tanh/368 [Stdlib!.tanh] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value acosh/369 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value acosh/369 [Stdlib!.acosh] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value asinh/370 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value asinh/370 [Stdlib!.asinh] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value atanh/371 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value atanh/371 [Stdlib!.atanh] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ceil/372 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ceil/372 [Stdlib!.ceil] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value floor/373 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value floor/373 [Stdlib!.floor] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value abs_float/374 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value abs_float/374 [Stdlib!.abs_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value copysign/375 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value copysign/375 [Stdlib!.copysign] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value mod_float/376 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value mod_float/376 [Stdlib!.mod_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value frexp/377 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value frexp/377 [Stdlib!.frexp] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ldexp/378 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ldexp/378 [Stdlib!.ldexp] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value modf/379 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value modf/379 [Stdlib!.modf] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value float/380 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value float/380 [Stdlib!.float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value float_of_int/381 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value float_of_int/381 [Stdlib!.float_of_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value truncate/382 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value truncate/382 [Stdlib!.truncate] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value int_of_float/383 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value int_of_float/383 [Stdlib!.int_of_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value infinity/384 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value infinity/384 [Stdlib!.infinity] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value neg_infinity/385 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value neg_infinity/385 [Stdlib!.neg_infinity] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value nan/386 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value nan/386 [Stdlib!.nan] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value max_float/387 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value max_float/387 [Stdlib!.max_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value min_float/388 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value min_float/388 [Stdlib!.min_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value epsilon_float/389 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value epsilon_float/389 [Stdlib!.epsilon_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type fpclass/390 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type fpclass/390 [Stdlib!.fpclass] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value classify_float/391 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value classify_float/391 [Stdlib!.classify_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ^/392 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ^/392 [Stdlib!.^] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value int_of_char/393 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value int_of_char/393 [Stdlib!.int_of_char] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value char_of_int/394 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value char_of_int/394 [Stdlib!.char_of_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ignore/395 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ignore/395 [Stdlib!.ignore] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value string_of_bool/396 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value string_of_bool/396 [Stdlib!.string_of_bool] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value bool_of_string_opt/397 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value bool_of_string_opt/397 [Stdlib!.bool_of_string_opt] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value bool_of_string/398 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value bool_of_string/398 [Stdlib!.bool_of_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value string_of_int/399 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value string_of_int/399 [Stdlib!.string_of_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value int_of_string_opt/400 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value int_of_string_opt/400 [Stdlib!.int_of_string_opt] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value int_of_string/401 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value int_of_string/401 [Stdlib!.int_of_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value string_of_float/402 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value string_of_float/402 [Stdlib!.string_of_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value float_of_string_opt/403 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value float_of_string_opt/403 [Stdlib!.float_of_string_opt] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value float_of_string/404 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value float_of_string/404 [Stdlib!.float_of_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value fst/405 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value fst/405 [Stdlib!.fst] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value snd/406 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value snd/406 [Stdlib!.snd] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value @/407 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value @/407 [Stdlib!.@] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type in_channel/408 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type in_channel/408 [Stdlib!.in_channel] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type out_channel/409 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type out_channel/409 [Stdlib!.out_channel] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value stdin/410 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value stdin/410 [Stdlib!.stdin] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value stdout/411 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value stdout/411 [Stdlib!.stdout] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value stderr/412 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value stderr/412 [Stdlib!.stderr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_char/413 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_char/413 [Stdlib!.print_char] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_string/414 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_string/414 [Stdlib!.print_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_bytes/415 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_bytes/415 [Stdlib!.print_bytes] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_int/416 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_int/416 [Stdlib!.print_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_float/417 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_float/417 [Stdlib!.print_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_endline/418 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_endline/418 [Stdlib!.print_endline] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value print_newline/419 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value print_newline/419 [Stdlib!.print_newline] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_char/420 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_char/420 [Stdlib!.prerr_char] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_string/421 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_string/421 [Stdlib!.prerr_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_bytes/422 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_bytes/422 [Stdlib!.prerr_bytes] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_int/423 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_int/423 [Stdlib!.prerr_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_float/424 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_float/424 [Stdlib!.prerr_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_endline/425 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_endline/425 [Stdlib!.prerr_endline] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value prerr_newline/426 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value prerr_newline/426 [Stdlib!.prerr_newline] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value read_line/427 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value read_line/427 [Stdlib!.read_line] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value read_int_opt/428 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value read_int_opt/428 [Stdlib!.read_int_opt] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value read_int/429 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value read_int/429 [Stdlib!.read_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value read_float_opt/430 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value read_float_opt/430 [Stdlib!.read_float_opt] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value read_float/431 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value read_float/431 [Stdlib!.read_float] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type open_flag/432 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type open_flag/432 [Stdlib!.open_flag] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value open_out/433 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value open_out/433 [Stdlib!.open_out] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value open_out_bin/434 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value open_out_bin/434 [Stdlib!.open_out_bin] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value open_out_gen/435 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value open_out_gen/435 [Stdlib!.open_out_gen] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value flush/436 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value flush/436 [Stdlib!.flush] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value flush_all/437 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value flush_all/437 [Stdlib!.flush_all] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_char/438 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_char/438 [Stdlib!.output_char] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_string/439 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_string/439 [Stdlib!.output_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_bytes/440 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_bytes/440 [Stdlib!.output_bytes] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output/441 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output/441 [Stdlib!.output] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_substring/442 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_substring/442 [Stdlib!.output_substring] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_byte/443 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_byte/443 [Stdlib!.output_byte] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_binary_int/444 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_binary_int/444 [Stdlib!.output_binary_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value output_value/445 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value output_value/445 [Stdlib!.output_value] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value seek_out/446 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value seek_out/446 [Stdlib!.seek_out] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value pos_out/447 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value pos_out/447 [Stdlib!.pos_out] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value out_channel_length/448 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value out_channel_length/448 [Stdlib!.out_channel_length] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value close_out/449 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value close_out/449 [Stdlib!.close_out] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value close_out_noerr/450 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value close_out_noerr/450 [Stdlib!.close_out_noerr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value set_binary_mode_out/451 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value set_binary_mode_out/451 [Stdlib!.set_binary_mode_out] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value open_in/452 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value open_in/452 [Stdlib!.open_in] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value open_in_bin/453 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value open_in_bin/453 [Stdlib!.open_in_bin] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value open_in_gen/454 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value open_in_gen/454 [Stdlib!.open_in_gen] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value input_char/455 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value input_char/455 [Stdlib!.input_char] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value input_line/456 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value input_line/456 [Stdlib!.input_line] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value input/457 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value input/457 [Stdlib!.input] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value really_input/458 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value really_input/458 [Stdlib!.really_input] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value really_input_string/459 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value really_input_string/459 [Stdlib!.really_input_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value input_byte/460 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value input_byte/460 [Stdlib!.input_byte] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value input_binary_int/461 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value input_binary_int/461 [Stdlib!.input_binary_int] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value input_value/462 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value input_value/462 [Stdlib!.input_value] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value seek_in/463 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value seek_in/463 [Stdlib!.seek_in] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value pos_in/464 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value pos_in/464 [Stdlib!.pos_in] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value in_channel_length/465 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value in_channel_length/465 [Stdlib!.in_channel_length] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value close_in/466 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value close_in/466 [Stdlib!.close_in] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value close_in_noerr/467 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value close_in_noerr/467 [Stdlib!.close_in_noerr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value set_binary_mode_in/468 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value set_binary_mode_in/468 [Stdlib!.set_binary_mode_in] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: module LargeFile/469 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module LargeFile/469 [Stdlib!.LargeFile] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.LargeFile -> LargeFile/469 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: type ref/470 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type ref/470 [Stdlib!.ref] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ref/471 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ref/471 [Stdlib!.ref] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value !/472 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value !/472 [Stdlib!.!] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value :=/473 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value :=/473 [Stdlib!.:=] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value incr/474 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value incr/474 [Stdlib!.incr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value decr/475 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value decr/475 [Stdlib!.decr] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type result/476 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type result/476 [Stdlib!.result] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type format6/477 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type format6/477 [Stdlib!.format6] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type format4/478 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type format4/478 [Stdlib!.format4] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: type format/479 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: type format/479 [Stdlib!.format] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value string_of_format/480 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value string_of_format/480 [Stdlib!.string_of_format] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value format_of_string/481 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value format_of_string/481 [Stdlib!.format_of_string] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value ^^/482 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value ^^/482 [Stdlib!.^^] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value exit/483 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value exit/483 [Stdlib!.exit] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value at_exit/484 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value at_exit/484 [Stdlib!.at_exit] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value valid_float_lexem/485 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value valid_float_lexem/485 [Stdlib!.valid_float_lexem] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value unsafe_really_input/486 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value unsafe_really_input/486 [Stdlib!.unsafe_really_input] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value do_at_exit/487 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value do_at_exit/487 [Stdlib!.do_at_exit] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: value do_domain_local_at_exit/488 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: value do_domain_local_at_exit/488 [Stdlib!.do_domain_local_at_exit] brought in scope by an open
  # 0.01 discourse-verbose - U3
  U3: module Arg/489 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Arg/489 [Stdlib!.Arg] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Arg! -> Arg/489 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Arg -> Arg/489 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Array/490 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Array/490 [Stdlib!.Array] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Array! -> Array/490 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Array -> Array/490 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module ArrayLabels/491 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module ArrayLabels/491 [Stdlib!.ArrayLabels] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__ArrayLabels! -> ArrayLabels/491 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.ArrayLabels -> ArrayLabels/491 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Atomic/492 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Atomic/492 [Stdlib!.Atomic] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Atomic! -> Atomic/492 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Atomic -> Atomic/492 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Bigarray/493 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Bigarray/493 [Stdlib!.Bigarray] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Bigarray! -> Bigarray/493 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Bigarray -> Bigarray/493 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Bool/494 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Bool/494 [Stdlib!.Bool] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Bool! -> Bool/494 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Bool -> Bool/494 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Buffer/495 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Buffer/495 [Stdlib!.Buffer] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Buffer! -> Buffer/495 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Buffer -> Buffer/495 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Bytes/496 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Bytes/496 [Stdlib!.Bytes] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Bytes! -> Bytes/496 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Bytes -> Bytes/496 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module BytesLabels/497 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module BytesLabels/497 [Stdlib!.BytesLabels] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__BytesLabels! -> BytesLabels/497 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.BytesLabels -> BytesLabels/497 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Callback/498 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Callback/498 [Stdlib!.Callback] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Callback! -> Callback/498 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Callback -> Callback/498 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Char/499 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Char/499 [Stdlib!.Char] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Char! -> Char/499 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Char -> Char/499 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Complex/500 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Complex/500 [Stdlib!.Complex] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Complex! -> Complex/500 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Complex -> Complex/500 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Condition/501 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Condition/501 [Stdlib!.Condition] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Condition! -> Condition/501 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Condition -> Condition/501 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Digest/502 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Digest/502 [Stdlib!.Digest] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Digest! -> Digest/502 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Digest -> Digest/502 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Domain/503 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Domain/503 [Stdlib!.Domain] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Domain! -> Domain/503 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Domain -> Domain/503 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Dynarray/504 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Dynarray/504 [Stdlib!.Dynarray] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Dynarray! -> Dynarray/504 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Dynarray -> Dynarray/504 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Pqueue/505 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Pqueue/505 [Stdlib!.Pqueue] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Pqueue! -> Pqueue/505 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Pqueue -> Pqueue/505 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Effect/506 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Effect/506 [Stdlib!.Effect] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Effect! -> Effect/506 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Effect -> Effect/506 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Either/507 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Either/507 [Stdlib!.Either] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Either! -> Either/507 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Either -> Either/507 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Ephemeron/508 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Ephemeron/508 [Stdlib!.Ephemeron] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Ephemeron! -> Ephemeron/508 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Ephemeron -> Ephemeron/508 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Filename/509 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Filename/509 [Stdlib!.Filename] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Filename! -> Filename/509 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Filename -> Filename/509 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Float/510 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Float/510 [Stdlib!.Float] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Float! -> Float/510 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Float -> Float/510 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Format/511 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Format/511 [Stdlib!.Format] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Format! -> Format/511 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Format -> Format/511 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Fun/512 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Fun/512 [Stdlib!.Fun] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Fun! -> Fun/512 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Fun -> Fun/512 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Gc/513 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Gc/513 [Stdlib!.Gc] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Gc! -> Gc/513 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Gc -> Gc/513 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Hashtbl/514 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Hashtbl/514 [Stdlib!.Hashtbl] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Hashtbl! -> Hashtbl/514 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Hashtbl -> Hashtbl/514 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Iarray/515 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Iarray/515 [Stdlib!.Iarray] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Iarray! -> Iarray/515 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Iarray -> Iarray/515 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module In_channel/516 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module In_channel/516 [Stdlib!.In_channel] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__In_channel! -> In_channel/516 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.In_channel -> In_channel/516 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Int/517 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Int/517 [Stdlib!.Int] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Int! -> Int/517 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Int -> Int/517 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Int32/518 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Int32/518 [Stdlib!.Int32] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Int32! -> Int32/518 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Int32 -> Int32/518 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Int64/519 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Int64/519 [Stdlib!.Int64] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Int64! -> Int64/519 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Int64 -> Int64/519 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Lazy/520 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Lazy/520 [Stdlib!.Lazy] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Lazy! -> Lazy/520 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Lazy -> Lazy/520 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Lexing/521 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Lexing/521 [Stdlib!.Lexing] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Lexing! -> Lexing/521 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Lexing -> Lexing/521 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module List/522 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module List/522 [Stdlib!.List] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__List! -> List/522 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.List -> List/522 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module ListLabels/523 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module ListLabels/523 [Stdlib!.ListLabels] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__ListLabels! -> ListLabels/523 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.ListLabels -> ListLabels/523 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Map/524 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Map/524 [Stdlib!.Map] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Map! -> Map/524 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Map -> Map/524 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Marshal/525 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Marshal/525 [Stdlib!.Marshal] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Marshal! -> Marshal/525 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Marshal -> Marshal/525 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module MoreLabels/526 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module MoreLabels/526 [Stdlib!.MoreLabels] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__MoreLabels! -> MoreLabels/526 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.MoreLabels -> MoreLabels/526 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Mutex/527 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Mutex/527 [Stdlib!.Mutex] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Mutex! -> Mutex/527 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Mutex -> Mutex/527 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Nativeint/528 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Nativeint/528 [Stdlib!.Nativeint] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Nativeint! -> Nativeint/528 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Nativeint -> Nativeint/528 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Obj/529 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Obj/529 [Stdlib!.Obj] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Obj! -> Obj/529 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Obj -> Obj/529 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Oo/530 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Oo/530 [Stdlib!.Oo] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Oo! -> Oo/530 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Oo -> Oo/530 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Option/531 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Option/531 [Stdlib!.Option] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Option! -> Option/531 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Option -> Option/531 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Out_channel/532 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Out_channel/532 [Stdlib!.Out_channel] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Out_channel! -> Out_channel/532 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Out_channel -> Out_channel/532 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Pair/533 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Pair/533 [Stdlib!.Pair] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Pair! -> Pair/533 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Pair -> Pair/533 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Parsing/534 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Parsing/534 [Stdlib!.Parsing] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Parsing! -> Parsing/534 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Parsing -> Parsing/534 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Printexc/535 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Printexc/535 [Stdlib!.Printexc] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Printexc! -> Printexc/535 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Printexc -> Printexc/535 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Printf/536 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Printf/536 [Stdlib!.Printf] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Printf! -> Printf/536 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Printf -> Printf/536 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Queue/537 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Queue/537 [Stdlib!.Queue] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Queue! -> Queue/537 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Queue -> Queue/537 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Random/538 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Random/538 [Stdlib!.Random] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Random! -> Random/538 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Random -> Random/538 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Result/539 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Result/539 [Stdlib!.Result] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Result! -> Result/539 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Result -> Result/539 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Repr/540 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Repr/540 [Stdlib!.Repr] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Repr! -> Repr/540 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Repr -> Repr/540 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Scanf/541 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Scanf/541 [Stdlib!.Scanf] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Scanf! -> Scanf/541 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Scanf -> Scanf/541 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Semaphore/542 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Semaphore/542 [Stdlib!.Semaphore] brought in scope by an open
  # 0.01 discourse-verbose - subst
  subst: Stdlib__Semaphore! -> Semaphore/542 (open/alias defined in file)
  # 0.01 discourse-verbose - subst
  subst: Stdlib!.Semaphore -> Semaphore/542 (open/alias defined in file)
  # 0.01 discourse-verbose - U3
  U3: module Seq/543 brought in scope by open
  # 0.01 discourse-verbose - U3
  U3: module Seq/543 [Stdlib!.Seq] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Seq! -> Seq/543 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Seq -> Seq/543 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Set/544 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Set/544 [Stdlib!.Set] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Set! -> Set/544 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Set -> Set/544 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Stack/545 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Stack/545 [Stdlib!.Stack] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Stack! -> Stack/545 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Stack -> Stack/545 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module StdLabels/546 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module StdLabels/546 [Stdlib!.StdLabels] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__StdLabels! -> StdLabels/546 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.StdLabels -> StdLabels/546 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module String/547 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module String/547 [Stdlib!.String] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__String! -> String/547 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.String -> String/547 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module StringLabels/548 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module StringLabels/548 [Stdlib!.StringLabels] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__StringLabels! -> StringLabels/548 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.StringLabels -> StringLabels/548 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Sys/549 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Sys/549 [Stdlib!.Sys] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Sys! -> Sys/549 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Sys -> Sys/549 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Type/550 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Type/550 [Stdlib!.Type] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Type! -> Type/550 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Type -> Type/550 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Uchar/551 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Uchar/551 [Stdlib!.Uchar] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Uchar! -> Uchar/551 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Uchar -> Uchar/551 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Unit/552 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Unit/552 [Stdlib!.Unit] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Unit! -> Unit/552 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Unit -> Unit/552 (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module Weak/553 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module Weak/553 [Stdlib!.Weak] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Weak! -> Weak/553 (open/alias defined in file)
  # 0.02 discourse-verbose - subst
  subst: Stdlib!.Weak -> Weak/553 (open/alias defined in file)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar! (File "foo.ml", line 1, characters 5-8)
  # 0.02 discourse-verbose - U3
  U3: open module Bar!
  # 0.02 discourse-verbose - U3
  U3: module A/554 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module A/554 [Bar!.A] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Bar!.A -> A/554 (open/alias defined in file)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar!.A.B (File "foo.ml", line 2, characters 5-8)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar!.A (File "foo.ml", line 2, characters 5-8)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar! (File "foo.ml", line 2, characters 5-8)
  # 0.02 discourse-verbose - U3
  U3: open module Bar!.A.B
  # 0.02 discourse-verbose - U3
  U3: module C/555 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module C/555[0] [Bar!.A.B.C] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Bar!.A.B.C -> C/555[0] (open/alias defined in file)
  # 0.02 discourse-verbose - U3
  U3: module D/556 brought in scope by open
  # 0.02 discourse-verbose - U3
  U3: module D/556[0] [Bar!.A.B.D] brought in scope by an open
  # 0.02 discourse-verbose - subst
  subst: Bar!.A.B.D -> D/556[0] (open/alias defined in file)
  # 0.02 discourse-verbose - U1
  U1: path value used in file: Bar!.A.B.D.foo (File "foo.ml", line 3, characters 10-15)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar!.A.B.D (File "foo.ml", line 3, characters 10-15)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar!.A.B (File "foo.ml", line 3, characters 10-15)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar!.A (File "foo.ml", line 3, characters 10-15)
  # 0.02 discourse-verbose - U1
  U1: path module used in file: Bar! (File "foo.ml", line 3, characters 10-15)
  # 0.02 discourse-verbose - U1
  U1: path value used in file: x/281 (File "foo.ml", line 3, characters 16-17)
  # 0.02 discourse-verbose - U1
  U1: path value used in file: x/281 (File "foo.ml", line 1, characters 0-1)
  # 0.02 discourse-verbose - D2
  D2: Bar! in U so in D (kind: module)
  # 0.02 discourse-verbose - D5
  D5: merging discourse of module Bar!
  # 0.02 discourse-verbose - D3
  D3: signature component module A/554 (Bar!.A) under module Bar!
  # 0.02 discourse-verbose - D2
  D2: Stdlib! in U so in D (kind: module)
  # 0.02 discourse-verbose - D5
  D5: merging discourse of module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value raise/284 (Stdlib!.raise) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value raise_notrace/285 (Stdlib!.raise_notrace) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value invalid_arg/286 (Stdlib!.invalid_arg) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value failwith/287 (Stdlib!.failwith) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Exit/288 (Stdlib!.Exit) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Match_failure/289 (Stdlib!.Match_failure) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Assert_failure/290 (Stdlib!.Assert_failure) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Invalid_argument/291 (Stdlib!.Invalid_argument) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Failure/292 (Stdlib!.Failure) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Not_found/293 (Stdlib!.Not_found) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Out_of_memory/294 (Stdlib!.Out_of_memory) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Stack_overflow/295 (Stdlib!.Stack_overflow) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Sys_error/296 (Stdlib!.Sys_error) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor End_of_file/297 (Stdlib!.End_of_file) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Division_by_zero/298 (Stdlib!.Division_by_zero) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Sys_blocked_io/299 (Stdlib!.Sys_blocked_io) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component extension constructor Undefined_recursive_module/300 (Stdlib!.Undefined_recursive_module) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value =/301 (Stdlib!.=) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value <>/302 (Stdlib!.<>) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value </303 (Stdlib!.<) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value >/304 (Stdlib!.>) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value <=/305 (Stdlib!.<=) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value >=/306 (Stdlib!.>=) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value compare/307 (Stdlib!.compare) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value min/308 (Stdlib!.min) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value max/309 (Stdlib!.max) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ==/310 (Stdlib!.==) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value !=/311 (Stdlib!.!=) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value not/312 (Stdlib!.not) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value &&/313 (Stdlib!.&&) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ||/314 (Stdlib!.||) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __LOC__/315 (Stdlib!.__LOC__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __FILE__/316 (Stdlib!.__FILE__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __LINE__/317 (Stdlib!.__LINE__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __MODULE__/318 (Stdlib!.__MODULE__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __POS__/319 (Stdlib!.__POS__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __FUNCTION__/320 (Stdlib!.__FUNCTION__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __LOC_OF__/321 (Stdlib!.__LOC_OF__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __LINE_OF__/322 (Stdlib!.__LINE_OF__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value __POS_OF__/323 (Stdlib!.__POS_OF__) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value |>/324 (Stdlib!.|>) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value @@/325 (Stdlib!.@@) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ~-/326 (Stdlib!.~-) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ~+/327 (Stdlib!.~+) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value succ/328 (Stdlib!.succ) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value pred/329 (Stdlib!.pred) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value +/330 (Stdlib!.+) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value -/331 (Stdlib!.-) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value */332 (Stdlib!.*) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value //333 (Stdlib!./) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value mod/334 (Stdlib!.mod) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value abs/335 (Stdlib!.abs) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value max_int/336 (Stdlib!.max_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value min_int/337 (Stdlib!.min_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value land/338 (Stdlib!.land) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value lor/339 (Stdlib!.lor) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value lxor/340 (Stdlib!.lxor) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value lnot/341 (Stdlib!.lnot) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value lsl/342 (Stdlib!.lsl) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value lsr/343 (Stdlib!.lsr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value asr/344 (Stdlib!.asr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ~-./345 (Stdlib!.~-.) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ~+./346 (Stdlib!.~+.) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value +./347 (Stdlib!.+.) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value -./348 (Stdlib!.-.) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value *./349 (Stdlib!.*.) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value /./350 (Stdlib!./.) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value **/351 (Stdlib!.**) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value sqrt/352 (Stdlib!.sqrt) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value exp/353 (Stdlib!.exp) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value log/354 (Stdlib!.log) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value log10/355 (Stdlib!.log10) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value expm1/356 (Stdlib!.expm1) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value log1p/357 (Stdlib!.log1p) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value cos/358 (Stdlib!.cos) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value sin/359 (Stdlib!.sin) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value tan/360 (Stdlib!.tan) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value acos/361 (Stdlib!.acos) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value asin/362 (Stdlib!.asin) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value atan/363 (Stdlib!.atan) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value atan2/364 (Stdlib!.atan2) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value hypot/365 (Stdlib!.hypot) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value cosh/366 (Stdlib!.cosh) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value sinh/367 (Stdlib!.sinh) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value tanh/368 (Stdlib!.tanh) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value acosh/369 (Stdlib!.acosh) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value asinh/370 (Stdlib!.asinh) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value atanh/371 (Stdlib!.atanh) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ceil/372 (Stdlib!.ceil) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value floor/373 (Stdlib!.floor) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value abs_float/374 (Stdlib!.abs_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value copysign/375 (Stdlib!.copysign) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value mod_float/376 (Stdlib!.mod_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value frexp/377 (Stdlib!.frexp) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ldexp/378 (Stdlib!.ldexp) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value modf/379 (Stdlib!.modf) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value float/380 (Stdlib!.float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value float_of_int/381 (Stdlib!.float_of_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value truncate/382 (Stdlib!.truncate) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value int_of_float/383 (Stdlib!.int_of_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value infinity/384 (Stdlib!.infinity) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value neg_infinity/385 (Stdlib!.neg_infinity) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value nan/386 (Stdlib!.nan) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value max_float/387 (Stdlib!.max_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value min_float/388 (Stdlib!.min_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value epsilon_float/389 (Stdlib!.epsilon_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type fpclass/390 (Stdlib!.fpclass) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value classify_float/391 (Stdlib!.classify_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ^/392 (Stdlib!.^) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value int_of_char/393 (Stdlib!.int_of_char) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value char_of_int/394 (Stdlib!.char_of_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ignore/395 (Stdlib!.ignore) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value string_of_bool/396 (Stdlib!.string_of_bool) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value bool_of_string_opt/397 (Stdlib!.bool_of_string_opt) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value bool_of_string/398 (Stdlib!.bool_of_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value string_of_int/399 (Stdlib!.string_of_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value int_of_string_opt/400 (Stdlib!.int_of_string_opt) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value int_of_string/401 (Stdlib!.int_of_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value string_of_float/402 (Stdlib!.string_of_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value float_of_string_opt/403 (Stdlib!.float_of_string_opt) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value float_of_string/404 (Stdlib!.float_of_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value fst/405 (Stdlib!.fst) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value snd/406 (Stdlib!.snd) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value @/407 (Stdlib!.@) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type in_channel/408 (Stdlib!.in_channel) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type out_channel/409 (Stdlib!.out_channel) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value stdin/410 (Stdlib!.stdin) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value stdout/411 (Stdlib!.stdout) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value stderr/412 (Stdlib!.stderr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_char/413 (Stdlib!.print_char) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_string/414 (Stdlib!.print_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_bytes/415 (Stdlib!.print_bytes) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_int/416 (Stdlib!.print_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_float/417 (Stdlib!.print_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_endline/418 (Stdlib!.print_endline) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value print_newline/419 (Stdlib!.print_newline) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_char/420 (Stdlib!.prerr_char) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_string/421 (Stdlib!.prerr_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_bytes/422 (Stdlib!.prerr_bytes) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_int/423 (Stdlib!.prerr_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_float/424 (Stdlib!.prerr_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_endline/425 (Stdlib!.prerr_endline) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value prerr_newline/426 (Stdlib!.prerr_newline) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value read_line/427 (Stdlib!.read_line) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value read_int_opt/428 (Stdlib!.read_int_opt) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value read_int/429 (Stdlib!.read_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value read_float_opt/430 (Stdlib!.read_float_opt) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value read_float/431 (Stdlib!.read_float) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type open_flag/432 (Stdlib!.open_flag) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value open_out/433 (Stdlib!.open_out) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value open_out_bin/434 (Stdlib!.open_out_bin) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value open_out_gen/435 (Stdlib!.open_out_gen) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value flush/436 (Stdlib!.flush) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value flush_all/437 (Stdlib!.flush_all) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_char/438 (Stdlib!.output_char) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_string/439 (Stdlib!.output_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_bytes/440 (Stdlib!.output_bytes) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output/441 (Stdlib!.output) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_substring/442 (Stdlib!.output_substring) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_byte/443 (Stdlib!.output_byte) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_binary_int/444 (Stdlib!.output_binary_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value output_value/445 (Stdlib!.output_value) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value seek_out/446 (Stdlib!.seek_out) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value pos_out/447 (Stdlib!.pos_out) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value out_channel_length/448 (Stdlib!.out_channel_length) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value close_out/449 (Stdlib!.close_out) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value close_out_noerr/450 (Stdlib!.close_out_noerr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value set_binary_mode_out/451 (Stdlib!.set_binary_mode_out) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value open_in/452 (Stdlib!.open_in) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value open_in_bin/453 (Stdlib!.open_in_bin) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value open_in_gen/454 (Stdlib!.open_in_gen) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value input_char/455 (Stdlib!.input_char) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value input_line/456 (Stdlib!.input_line) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value input/457 (Stdlib!.input) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value really_input/458 (Stdlib!.really_input) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value really_input_string/459 (Stdlib!.really_input_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value input_byte/460 (Stdlib!.input_byte) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value input_binary_int/461 (Stdlib!.input_binary_int) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value input_value/462 (Stdlib!.input_value) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value seek_in/463 (Stdlib!.seek_in) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value pos_in/464 (Stdlib!.pos_in) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value in_channel_length/465 (Stdlib!.in_channel_length) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value close_in/466 (Stdlib!.close_in) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value close_in_noerr/467 (Stdlib!.close_in_noerr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value set_binary_mode_in/468 (Stdlib!.set_binary_mode_in) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component module LargeFile/469 (Stdlib!.LargeFile) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type ref/470 (Stdlib!.ref) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ref/471 (Stdlib!.ref) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value !/472 (Stdlib!.!) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value :=/473 (Stdlib!.:=) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value incr/474 (Stdlib!.incr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value decr/475 (Stdlib!.decr) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type result/476 (Stdlib!.result) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type format6/477 (Stdlib!.format6) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type format4/478 (Stdlib!.format4) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component type format/479 (Stdlib!.format) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value string_of_format/480 (Stdlib!.string_of_format) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value format_of_string/481 (Stdlib!.format_of_string) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value ^^/482 (Stdlib!.^^) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value exit/483 (Stdlib!.exit) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value at_exit/484 (Stdlib!.at_exit) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value valid_float_lexem/485 (Stdlib!.valid_float_lexem) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value unsafe_really_input/486 (Stdlib!.unsafe_really_input) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value do_at_exit/487 (Stdlib!.do_at_exit) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component value do_domain_local_at_exit/488 (Stdlib!.do_domain_local_at_exit) under module Stdlib!
  # 0.02 discourse-verbose - D3
  D3: signature component module Arg/489 (Stdlib!.Arg) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Arg! -> Stdlib!.Arg (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Arg! -> Stdlib!.Arg (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Array/490 (Stdlib!.Array) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Array! -> Stdlib!.Array (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Array! -> Stdlib!.Array (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module ArrayLabels/491 (Stdlib!.ArrayLabels) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__ArrayLabels! -> Stdlib!.ArrayLabels (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__ArrayLabels! -> Stdlib!.ArrayLabels (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Atomic/492 (Stdlib!.Atomic) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Atomic! -> Stdlib!.Atomic (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Atomic! -> Stdlib!.Atomic (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Bigarray/493 (Stdlib!.Bigarray) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Bigarray! -> Stdlib!.Bigarray (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Bigarray! -> Stdlib!.Bigarray (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Bool/494 (Stdlib!.Bool) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Bool! -> Stdlib!.Bool (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Bool! -> Stdlib!.Bool (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Buffer/495 (Stdlib!.Buffer) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Buffer! -> Stdlib!.Buffer (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Buffer! -> Stdlib!.Buffer (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Bytes/496 (Stdlib!.Bytes) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Bytes! -> Stdlib!.Bytes (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Bytes! -> Stdlib!.Bytes (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module BytesLabels/497 (Stdlib!.BytesLabels) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__BytesLabels! -> Stdlib!.BytesLabels (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__BytesLabels! -> Stdlib!.BytesLabels (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Callback/498 (Stdlib!.Callback) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Callback! -> Stdlib!.Callback (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Callback! -> Stdlib!.Callback (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Char/499 (Stdlib!.Char) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Char! -> Stdlib!.Char (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Char! -> Stdlib!.Char (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Complex/500 (Stdlib!.Complex) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Complex! -> Stdlib!.Complex (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Complex! -> Stdlib!.Complex (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Condition/501 (Stdlib!.Condition) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Condition! -> Stdlib!.Condition (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Condition! -> Stdlib!.Condition (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Digest/502 (Stdlib!.Digest) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Digest! -> Stdlib!.Digest (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Digest! -> Stdlib!.Digest (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Domain/503 (Stdlib!.Domain) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Domain! -> Stdlib!.Domain (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Domain! -> Stdlib!.Domain (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Dynarray/504 (Stdlib!.Dynarray) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Dynarray! -> Stdlib!.Dynarray (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Dynarray! -> Stdlib!.Dynarray (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Pqueue/505 (Stdlib!.Pqueue) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Pqueue! -> Stdlib!.Pqueue (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Pqueue! -> Stdlib!.Pqueue (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Effect/506 (Stdlib!.Effect) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Effect! -> Stdlib!.Effect (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Effect! -> Stdlib!.Effect (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Either/507 (Stdlib!.Either) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Either! -> Stdlib!.Either (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Either! -> Stdlib!.Either (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Ephemeron/508 (Stdlib!.Ephemeron) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Ephemeron! -> Stdlib!.Ephemeron (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Ephemeron! -> Stdlib!.Ephemeron (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Filename/509 (Stdlib!.Filename) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Filename! -> Stdlib!.Filename (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Filename! -> Stdlib!.Filename (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Float/510 (Stdlib!.Float) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Float! -> Stdlib!.Float (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Float! -> Stdlib!.Float (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Format/511 (Stdlib!.Format) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Format! -> Stdlib!.Format (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Format! -> Stdlib!.Format (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Fun/512 (Stdlib!.Fun) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Fun! -> Stdlib!.Fun (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Fun! -> Stdlib!.Fun (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Gc/513 (Stdlib!.Gc) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Gc! -> Stdlib!.Gc (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Gc! -> Stdlib!.Gc (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Hashtbl/514 (Stdlib!.Hashtbl) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Hashtbl! -> Stdlib!.Hashtbl (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Hashtbl! -> Stdlib!.Hashtbl (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Iarray/515 (Stdlib!.Iarray) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Iarray! -> Stdlib!.Iarray (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Iarray! -> Stdlib!.Iarray (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module In_channel/516 (Stdlib!.In_channel) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__In_channel! -> Stdlib!.In_channel (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__In_channel! -> Stdlib!.In_channel (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Int/517 (Stdlib!.Int) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Int! -> Stdlib!.Int (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Int! -> Stdlib!.Int (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Int32/518 (Stdlib!.Int32) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Int32! -> Stdlib!.Int32 (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Int32! -> Stdlib!.Int32 (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Int64/519 (Stdlib!.Int64) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Int64! -> Stdlib!.Int64 (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Int64! -> Stdlib!.Int64 (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Lazy/520 (Stdlib!.Lazy) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Lazy! -> Stdlib!.Lazy (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Lazy! -> Stdlib!.Lazy (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Lexing/521 (Stdlib!.Lexing) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Lexing! -> Stdlib!.Lexing (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Lexing! -> Stdlib!.Lexing (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module List/522 (Stdlib!.List) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__List! -> Stdlib!.List (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__List! -> Stdlib!.List (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module ListLabels/523 (Stdlib!.ListLabels) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__ListLabels! -> Stdlib!.ListLabels (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__ListLabels! -> Stdlib!.ListLabels (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Map/524 (Stdlib!.Map) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Map! -> Stdlib!.Map (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Map! -> Stdlib!.Map (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Marshal/525 (Stdlib!.Marshal) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Marshal! -> Stdlib!.Marshal (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Marshal! -> Stdlib!.Marshal (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module MoreLabels/526 (Stdlib!.MoreLabels) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__MoreLabels! -> Stdlib!.MoreLabels (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__MoreLabels! -> Stdlib!.MoreLabels (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Mutex/527 (Stdlib!.Mutex) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Mutex! -> Stdlib!.Mutex (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Mutex! -> Stdlib!.Mutex (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Nativeint/528 (Stdlib!.Nativeint) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Nativeint! -> Stdlib!.Nativeint (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Nativeint! -> Stdlib!.Nativeint (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Obj/529 (Stdlib!.Obj) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Obj! -> Stdlib!.Obj (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Obj! -> Stdlib!.Obj (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Oo/530 (Stdlib!.Oo) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Oo! -> Stdlib!.Oo (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Oo! -> Stdlib!.Oo (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Option/531 (Stdlib!.Option) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Option! -> Stdlib!.Option (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Option! -> Stdlib!.Option (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Out_channel/532 (Stdlib!.Out_channel) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Out_channel! -> Stdlib!.Out_channel (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Out_channel! -> Stdlib!.Out_channel (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Pair/533 (Stdlib!.Pair) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Pair! -> Stdlib!.Pair (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Pair! -> Stdlib!.Pair (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Parsing/534 (Stdlib!.Parsing) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Parsing! -> Stdlib!.Parsing (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Parsing! -> Stdlib!.Parsing (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Printexc/535 (Stdlib!.Printexc) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Printexc! -> Stdlib!.Printexc (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Printexc! -> Stdlib!.Printexc (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Printf/536 (Stdlib!.Printf) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Printf! -> Stdlib!.Printf (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Printf! -> Stdlib!.Printf (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Queue/537 (Stdlib!.Queue) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Queue! -> Stdlib!.Queue (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Queue! -> Stdlib!.Queue (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Random/538 (Stdlib!.Random) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Random! -> Stdlib!.Random (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Random! -> Stdlib!.Random (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Result/539 (Stdlib!.Result) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Result! -> Stdlib!.Result (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Result! -> Stdlib!.Result (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Repr/540 (Stdlib!.Repr) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Repr! -> Stdlib!.Repr (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Repr! -> Stdlib!.Repr (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Scanf/541 (Stdlib!.Scanf) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Scanf! -> Stdlib!.Scanf (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Scanf! -> Stdlib!.Scanf (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Semaphore/542 (Stdlib!.Semaphore) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Semaphore! -> Stdlib!.Semaphore (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Semaphore! -> Stdlib!.Semaphore (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Seq/543 (Stdlib!.Seq) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Seq! -> Stdlib!.Seq (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Seq! -> Stdlib!.Seq (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Set/544 (Stdlib!.Set) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Set! -> Stdlib!.Set (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Set! -> Stdlib!.Set (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Stack/545 (Stdlib!.Stack) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Stack! -> Stdlib!.Stack (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Stack! -> Stdlib!.Stack (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module StdLabels/546 (Stdlib!.StdLabels) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__StdLabels! -> Stdlib!.StdLabels (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__StdLabels! -> Stdlib!.StdLabels (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module String/547 (Stdlib!.String) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__String! -> Stdlib!.String (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__String! -> Stdlib!.String (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module StringLabels/548 (Stdlib!.StringLabels) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__StringLabels! -> Stdlib!.StringLabels (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__StringLabels! -> Stdlib!.StringLabels (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Sys/549 (Stdlib!.Sys) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Sys! -> Stdlib!.Sys (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Sys! -> Stdlib!.Sys (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Type/550 (Stdlib!.Type) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Type! -> Stdlib!.Type (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Type! -> Stdlib!.Type (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Uchar/551 (Stdlib!.Uchar) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Uchar! -> Stdlib!.Uchar (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Uchar! -> Stdlib!.Uchar (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Unit/552 (Stdlib!.Unit) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Unit! -> Stdlib!.Unit (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Unit! -> Stdlib!.Unit (open/alias defined in file)
  # 0.02 discourse-verbose - D3
  D3: signature component module Weak/553 (Stdlib!.Weak) under module Stdlib!
  # 0.02 discourse-verbose - D12
  D12: subst Stdlib__Weak! -> Stdlib!.Weak (sub-component is a module alias)
  # 0.02 discourse-verbose - subst
  subst: Stdlib__Weak! -> Stdlib!.Weak (open/alias defined in file)
  # 0.02 discourse-verbose - D2
  D2: x/281 in U so in D (kind: value)
  # 0.02 discourse-verbose - D4
  D4: merging discourse of value x/281
  # 0.02 discourse-verbose - D2
  D2: Bar!.A in U so in D (kind: module)
  # 0.02 discourse-verbose - D5
  D5: merging discourse of module Bar!.A
  # 0.02 discourse-verbose - D3
  D3: signature component type t/557 (Bar!.A.t) under module Bar!.A
  # 0.02 discourse-verbose - D3
  D3: signature component module B/558 (Bar!.A.B) under module Bar!.A
  # 0.02 discourse-verbose - D2
  D2: Bar!.A.B in U so in D (kind: module)
  # 0.02 discourse-verbose - D5
  D5: merging discourse of module Bar!.A.B
  # 0.02 discourse-verbose - D3
  D3: signature component module C/555 (Bar!.A.B.C) under module Bar!.A.B
  # 0.02 discourse-verbose - D3
  D3: signature component module D/556 (Bar!.A.B.D) under module Bar!.A.B
  # 0.02 discourse-verbose - D2
  D2: Bar!.A.B.D in U so in D (kind: module)
  # 0.02 discourse-verbose - D5
  D5: merging discourse of module Bar!.A.B.D
  # 0.02 discourse-verbose - D3
  D3: signature component value foo/559 (Bar!.A.B.D.foo) under module Bar!.A.B.D
  # 0.02 discourse-verbose - D2
  D2: Bar!.A.B.D.foo in U so in D (kind: value)
  # 0.02 discourse-verbose - D4
  D4: merging discourse of value Bar!.A.B.D.foo
  {
    "start": {
      "line": 3,
      "col": 16
    },
    "end": {
      "line": 3,
      "col": 17
    },
    "type": "A.t",
    "tail": "no"
  }
