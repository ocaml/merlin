
  $ build-irmin >> /dev/null 2>&1

  $ merl-an error-regression -s 1 --data=test-data 2> /dev/null

  $ cat test-data/results.json
  {"sample_id":3113,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/ppx_irmin/test_logs.ml < ./irmin/test/ppx_irmin/test_logs.ml","success":true}
  {"sample_id":3112,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1:10' -index 0 -filename ./irmin/test/ppx_irmin/test_logs.ml < ./irmin/test/ppx_irmin/test_logs.ml","success":true}
  {"sample_id":3109,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1:10' -filename ./irmin/test/ppx_irmin/test_logs.ml < ./irmin/test/ppx_irmin/test_logs.ml","success":true}
  {"sample_id":3108,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '42:16' -filename ./irmin/test/ppx_irmin/test_logs.ml < ./irmin/test/ppx_irmin/test_logs.ml","success":true}
  {"sample_id":3107,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '40:4' -end '40:21' -filename ./irmin/test/ppx_irmin/test_logs.ml < ./irmin/test/ppx_irmin/test_logs.ml","success":true}
  {"sample_id":3106,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_tree.mli < ./irmin/test/irmin/test_tree.mli","success":true}
  {"sample_id":3105,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin/test_tree.mli < ./irmin/test/irmin/test_tree.mli","success":true}
  {"sample_id":3104,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin/test_tree.mli < ./irmin/test/irmin/test_tree.mli","success":true}
  {"sample_id":3103,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin/test_tree.mli < ./irmin/test/irmin/test_tree.mli","success":true}
  {"sample_id":3102,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin/test_tree.mli < ./irmin/test/irmin/test_tree.mli","success":true}
  {"sample_id":3099,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3098,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '484:23' -index 0 -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3097,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix gche -position '484:23' -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3096,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix gche -position '484:23' -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3095,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '484:23' -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3094,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '558:33' -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3093,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '548:11' -end '548:22' -filename ./irmin/test/irmin/test_tree.ml < ./irmin/test/irmin/test_tree.ml","success":true}
  {"sample_id":3092,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3091,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '37:7' -index 0 -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3090,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ite -position '37:7' -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3089,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ite -position '37:7' -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3088,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '37:7' -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3087,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '28:43' -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3086,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '28:30' -end '28:43' -filename ./irmin/test/irmin/test_lru.ml < ./irmin/test/irmin/test_lru.ml","success":true}
  {"sample_id":3085,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_hash.mli < ./irmin/test/irmin/test_hash.mli","success":true}
  {"sample_id":3084,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:15' -index 0 -filename ./irmin/test/irmin/test_hash.mli < ./irmin/test/irmin/test_hash.mli","success":true}
  {"sample_id":3083,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin/test_hash.mli < ./irmin/test/irmin/test_hash.mli","success":true}
  {"sample_id":3082,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin/test_hash.mli < ./irmin/test/irmin/test_hash.mli","success":true}
  {"sample_id":3081,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:15' -filename ./irmin/test/irmin/test_hash.mli < ./irmin/test/irmin/test_hash.mli","success":true}
  {"sample_id":3078,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3077,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '33:25' -index 0 -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3076,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix in -position '33:25' -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3075,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix in -position '33:25' -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3074,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '33:25' -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3073,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '28:16' -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3072,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '28:19' -end '28:28' -filename ./irmin/test/irmin/test_hash.ml < ./irmin/test/irmin/test_hash.ml","success":true}
  {"sample_id":3071,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_conf.mli < ./irmin/test/irmin/test_conf.mli","success":true}
  {"sample_id":3070,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin/test_conf.mli < ./irmin/test/irmin/test_conf.mli","success":true}
  {"sample_id":3069,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin/test_conf.mli < ./irmin/test/irmin/test_conf.mli","success":true}
  {"sample_id":3068,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin/test_conf.mli < ./irmin/test/irmin/test_conf.mli","success":true}
  {"sample_id":3067,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin/test_conf.mli < ./irmin/test/irmin/test_conf.mli","success":true}
  {"sample_id":3064,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3063,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '39:34' -index 0 -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3062,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix k -position '39:34' -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3061,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix k -position '39:34' -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3060,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '39:34' -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3059,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '29:32' -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3058,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '29:37' -end '29:37' -filename ./irmin/test/irmin/test_conf.ml < ./irmin/test/irmin/test_conf.ml","success":true}
  {"sample_id":3057,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test.mli < ./irmin/test/irmin/test.mli","success":true}
  {"sample_id":3050,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3049,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '35:17' -index 0 -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3048,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Random.se -position '35:17' -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3047,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Random.se -position '35:17' -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3046,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '35:17' -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3045,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '35:20' -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3044,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '35:2' -end '36:46' -filename ./irmin/test/irmin/test.ml < ./irmin/test/irmin/test.ml","success":true}
  {"sample_id":3043,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3042,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '36:20' -index 0 -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3041,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Alcotes -position '36:20' -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3040,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Alcotes -position '36:20' -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3039,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '36:20' -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3038,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '116:43' -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3037,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '114:6' -end '117:20' -filename ./irmin/test/irmin/generic-key/test_store_offset.ml < ./irmin/test/irmin/generic-key/test_store_offset.ml","success":true}
  {"sample_id":3036,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3035,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '39:57' -index 0 -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3034,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Som -position '39:57' -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3033,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Som -position '39:57' -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3032,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '39:57' -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3031,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '91:52' -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3030,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '39:6' -end '39:64' -filename ./irmin/test/irmin/generic-key/test_inlined_contents.ml < ./irmin/test/irmin/generic-key/test_inlined_contents.ml","success":true}
  {"sample_id":3029,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/generic-key/test.ml < ./irmin/test/irmin/generic-key/test.ml","success":true}
  {"sample_id":3028,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:7' -index 0 -filename ./irmin/test/irmin/generic-key/test.ml < ./irmin/test/irmin/generic-key/test.ml","success":true}
  {"sample_id":3025,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:7' -filename ./irmin/test/irmin/generic-key/test.ml < ./irmin/test/irmin/generic-key/test.ml","success":true}
  {"sample_id":3024,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:7' -filename ./irmin/test/irmin/generic-key/test.ml < ./irmin/test/irmin/generic-key/test.ml","success":true}
  {"sample_id":3023,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:9' -end '22:7' -filename ./irmin/test/irmin/generic-key/test.ml < ./irmin/test/irmin/generic-key/test.ml","success":true}
  {"sample_id":3022,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/data/test_fixed_size_string_set.mli < ./irmin/test/irmin/data/test_fixed_size_string_set.mli","success":true}
  {"sample_id":3021,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1:15' -index 0 -filename ./irmin/test/irmin/data/test_fixed_size_string_set.mli < ./irmin/test/irmin/data/test_fixed_size_string_set.mli","success":true}
  {"sample_id":3020,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '1:15' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.mli < ./irmin/test/irmin/data/test_fixed_size_string_set.mli","success":true}
  {"sample_id":3019,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '1:15' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.mli < ./irmin/test/irmin/data/test_fixed_size_string_set.mli","success":true}
  {"sample_id":3018,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1:15' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.mli < ./irmin/test/irmin/data/test_fixed_size_string_set.mli","success":true}
  {"sample_id":3015,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3014,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '31:56' -index 0 -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3013,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix has -position '31:56' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3012,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix has -position '31:56' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3011,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '31:56' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3010,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:60' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3009,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:39' -end '20:45' -filename ./irmin/test/irmin/data/test_fixed_size_string_set.ml < ./irmin/test/irmin/data/test_fixed_size_string_set.ml","success":true}
  {"sample_id":3008,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/data/test.mli < ./irmin/test/irmin/data/test.mli","success":true}
  {"sample_id":3001,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":3000,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '6:63' -index 0 -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":2999,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Test_fixed_size_s -position '6:63' -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":2998,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Test_fixed_size_s -position '6:63' -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":2997,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '6:63' -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":2996,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '6:64' -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":2995,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '6:7' -end '6:29' -filename ./irmin/test/irmin/data/test.ml < ./irmin/test/irmin/data/test.ml","success":true}
  {"sample_id":2994,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2993,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:17' -index 0 -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2992,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Non -position '20:17' -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2991,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Non -position '20:17' -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2990,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:17' -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2989,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '14:17' -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2988,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '12:22' -end '12:24' -filename ./irmin/test/irmin/data/import.ml < ./irmin/test/irmin/data/import.ml","success":true}
  {"sample_id":2987,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2986,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '36:16' -index 0 -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2985,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix store_ -position '36:16' -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2984,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix store_ -position '36:16' -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2983,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '36:16' -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2982,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '28:40' -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2981,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '37:15' -end '37:23' -filename ./irmin/test/irmin-tezos/irmin_fsck.ml < ./irmin/test/irmin-tezos/irmin_fsck.ml","success":true}
  {"sample_id":2980,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-tezos/generate.mli < ./irmin/test/irmin-tezos/generate.mli","success":true}
  {"sample_id":2973,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-tezos/generate.ml < ./irmin/test/irmin-tezos/generate.ml","success":true}
  {"sample_id":2972,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '55:60' -index 0 -filename ./irmin/test/irmin-tezos/generate.ml < ./irmin/test/irmin-tezos/generate.ml","success":true}
  {"sample_id":2969,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '55:60' -filename ./irmin/test/irmin-tezos/generate.ml < ./irmin/test/irmin-tezos/generate.ml","success":true}
  {"sample_id":2968,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '55:60' -filename ./irmin/test/irmin-tezos/generate.ml < ./irmin/test/irmin-tezos/generate.ml","success":true}
  {"sample_id":2967,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '55:49' -end '55:52' -filename ./irmin/test/irmin-tezos/generate.ml < ./irmin/test/irmin-tezos/generate.ml","success":true}
  {"sample_id":2966,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2965,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '133:20' -index 0 -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2964,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '133:20' -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2963,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '133:20' -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2962,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '133:20' -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2961,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '629:28' -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2960,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '590:8' -end '601:20' -filename ./irmin/test/irmin-pack/test_upgrade.ml < ./irmin/test/irmin-pack/test_upgrade.ml","success":true}
  {"sample_id":2959,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2958,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '276:37' -index 0 -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2957,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Fm -position '276:37' -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2956,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Fm -position '276:37' -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2955,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '276:37' -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2954,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '719:57' -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2953,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '666:57' -end '666:58' -filename ./irmin/test/irmin-pack/test_tree.ml < ./irmin/test/irmin-pack/test_tree.ml","success":true}
  {"sample_id":2952,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2951,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '148:70' -index 0 -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2950,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_pack.Indexin -position '148:70' -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2949,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_pack.Indexin -position '148:70' -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2948,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '148:70' -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2947,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '262:44' -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2946,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '245:7' -end '245:17' -filename ./irmin/test/irmin-pack/test_snapshot.ml < ./irmin/test/irmin-pack/test_snapshot.ml","success":true}
  {"sample_id":2945,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_readonly.mli < ./irmin/test/irmin-pack/test_readonly.mli","success":true}
  {"sample_id":2944,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin-pack/test_readonly.mli < ./irmin/test/irmin-pack/test_readonly.mli","success":true}
  {"sample_id":2943,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_readonly.mli < ./irmin/test/irmin-pack/test_readonly.mli","success":true}
  {"sample_id":2942,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_readonly.mli < ./irmin/test/irmin-pack/test_readonly.mli","success":true}
  {"sample_id":2941,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin-pack/test_readonly.mli < ./irmin/test/irmin-pack/test_readonly.mli","success":true}
  {"sample_id":2938,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2937,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '43:19' -index 0 -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2936,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S.Rep -position '43:19' -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2935,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S.Rep -position '43:19' -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2934,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '43:19' -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2933,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '53:20' -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2932,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '121:34' -end '121:52' -filename ./irmin/test/irmin-pack/test_readonly.ml < ./irmin/test/irmin-pack/test_readonly.ml","success":true}
  {"sample_id":2931,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_ranges.ml < ./irmin/test/irmin-pack/test_ranges.ml","success":true}
  {"sample_id":2930,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '33:57' -index 0 -filename ./irmin/test/irmin-pack/test_ranges.ml < ./irmin/test/irmin-pack/test_ranges.ml","success":true}
  {"sample_id":2927,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '33:57' -filename ./irmin/test/irmin-pack/test_ranges.ml < ./irmin/test/irmin-pack/test_ranges.ml","success":true}
  {"sample_id":2926,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '33:31' -filename ./irmin/test/irmin-pack/test_ranges.ml < ./irmin/test/irmin-pack/test_ranges.ml","success":true}
  {"sample_id":2925,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '33:29' -end '33:35' -filename ./irmin/test/irmin-pack/test_ranges.ml < ./irmin/test/irmin-pack/test_ranges.ml","success":true}
  {"sample_id":2924,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_pack_version_bump.mli < ./irmin/test/irmin-pack/test_pack_version_bump.mli","success":true}
  {"sample_id":2923,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1:43' -index 0 -filename ./irmin/test/irmin-pack/test_pack_version_bump.mli < ./irmin/test/irmin-pack/test_pack_version_bump.mli","success":true}
  {"sample_id":2922,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '1:43' -filename ./irmin/test/irmin-pack/test_pack_version_bump.mli < ./irmin/test/irmin-pack/test_pack_version_bump.mli","success":true}
  {"sample_id":2921,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '1:43' -filename ./irmin/test/irmin-pack/test_pack_version_bump.mli < ./irmin/test/irmin-pack/test_pack_version_bump.mli","success":true}
  {"sample_id":2920,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1:43' -filename ./irmin/test/irmin-pack/test_pack_version_bump.mli < ./irmin/test/irmin-pack/test_pack_version_bump.mli","success":true}
  {"sample_id":2917,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_pack_version_bump.ml < ./irmin/test/irmin-pack/test_pack_version_bump.ml","success":true}
  {"sample_id":2916,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '127:26' -index 0 -filename ./irmin/test/irmin-pack/test_pack_version_bump.ml < ./irmin/test/irmin-pack/test_pack_version_bump.ml","success":true}
  {"sample_id":2913,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '127:26' -filename ./irmin/test/irmin-pack/test_pack_version_bump.ml < ./irmin/test/irmin-pack/test_pack_version_bump.ml","success":true}
  {"sample_id":2912,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '98:48' -filename ./irmin/test/irmin-pack/test_pack_version_bump.ml < ./irmin/test/irmin-pack/test_pack_version_bump.ml","success":true}
  {"sample_id":2911,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '111:4' -end '111:60' -filename ./irmin/test/irmin-pack/test_pack_version_bump.ml < ./irmin/test/irmin-pack/test_pack_version_bump.ml","success":true}
  {"sample_id":2910,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_pack.mli < ./irmin/test/irmin-pack/test_pack.mli","success":true}
  {"sample_id":2909,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:52' -index 0 -filename ./irmin/test/irmin-pack/test_pack.mli < ./irmin/test/irmin-pack/test_pack.mli","success":true}
  {"sample_id":2908,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '18:52' -filename ./irmin/test/irmin-pack/test_pack.mli < ./irmin/test/irmin-pack/test_pack.mli","success":true}
  {"sample_id":2907,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '18:52' -filename ./irmin/test/irmin-pack/test_pack.mli < ./irmin/test/irmin-pack/test_pack.mli","success":true}
  {"sample_id":2906,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:52' -filename ./irmin/test/irmin-pack/test_pack.mli < ./irmin/test/irmin-pack/test_pack.mli","success":true}
  {"sample_id":2903,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2902,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '565:48' -index 0 -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2901,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Test_gc.Concur -position '565:48' -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2900,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Test_gc.Concur -position '565:48' -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2899,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '565:48' -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2898,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '324:39' -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2897,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '326:35' -end '326:36' -filename ./irmin/test/irmin-pack/test_pack.ml < ./irmin/test/irmin-pack/test_pack.ml","success":true}
  {"sample_id":2896,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_nearest_leq.mli < ./irmin/test/irmin-pack/test_nearest_leq.mli","success":true}
  {"sample_id":2895,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1:43' -index 0 -filename ./irmin/test/irmin-pack/test_nearest_leq.mli < ./irmin/test/irmin-pack/test_nearest_leq.mli","success":true}
  {"sample_id":2894,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '1:43' -filename ./irmin/test/irmin-pack/test_nearest_leq.mli < ./irmin/test/irmin-pack/test_nearest_leq.mli","success":true}
  {"sample_id":2893,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '1:43' -filename ./irmin/test/irmin-pack/test_nearest_leq.mli < ./irmin/test/irmin-pack/test_nearest_leq.mli","success":true}
  {"sample_id":2892,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1:43' -filename ./irmin/test/irmin-pack/test_nearest_leq.mli < ./irmin/test/irmin-pack/test_nearest_leq.mli","success":true}
  {"sample_id":2889,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2888,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '13:20' -index 0 -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2887,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ge -position '13:20' -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2886,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ge -position '13:20' -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2885,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '13:20' -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2884,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:20' -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2883,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:28' -end '18:38' -filename ./irmin/test/irmin-pack/test_nearest_geq.ml < ./irmin/test/irmin-pack/test_nearest_geq.ml","success":true}
  {"sample_id":2882,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_mapping.mli < ./irmin/test/irmin-pack/test_mapping.mli","success":true}
  {"sample_id":2881,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin-pack/test_mapping.mli < ./irmin/test/irmin-pack/test_mapping.mli","success":true}
  {"sample_id":2880,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_mapping.mli < ./irmin/test/irmin-pack/test_mapping.mli","success":true}
  {"sample_id":2879,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_mapping.mli < ./irmin/test/irmin-pack/test_mapping.mli","success":true}
  {"sample_id":2878,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin-pack/test_mapping.mli < ./irmin/test/irmin-pack/test_mapping.mli","success":true}
  {"sample_id":2875,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_mapping.ml < ./irmin/test/irmin-pack/test_mapping.ml","success":true}
  {"sample_id":2874,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '40:52' -index 0 -filename ./irmin/test/irmin-pack/test_mapping.ml < ./irmin/test/irmin-pack/test_mapping.ml","success":true}
  {"sample_id":2871,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '40:52' -filename ./irmin/test/irmin-pack/test_mapping.ml < ./irmin/test/irmin-pack/test_mapping.ml","success":true}
  {"sample_id":2870,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '33:58' -filename ./irmin/test/irmin-pack/test_mapping.ml < ./irmin/test/irmin-pack/test_mapping.ml","success":true}
  {"sample_id":2869,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '34:12' -end '34:18' -filename ./irmin/test/irmin-pack/test_mapping.ml < ./irmin/test/irmin-pack/test_mapping.ml","success":true}
  {"sample_id":2868,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_lower.mli < ./irmin/test/irmin-pack/test_lower.mli","success":true}
  {"sample_id":2867,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2866,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '409:11' -index 0 -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2865,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Alcot -position '409:11' -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2864,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Alcot -position '409:11' -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2863,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '409:11' -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2862,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '503:48' -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2861,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '488:14' -end '488:26' -filename ./irmin/test/irmin-pack/test_lower.ml < ./irmin/test/irmin-pack/test_lower.ml","success":true}
  {"sample_id":2860,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_inode.mli < ./irmin/test/irmin-pack/test_inode.mli","success":true}
  {"sample_id":2859,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin-pack/test_inode.mli < ./irmin/test/irmin-pack/test_inode.mli","success":true}
  {"sample_id":2858,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_inode.mli < ./irmin/test/irmin-pack/test_inode.mli","success":true}
  {"sample_id":2857,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_inode.mli < ./irmin/test/irmin-pack/test_inode.mli","success":true}
  {"sample_id":2856,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin-pack/test_inode.mli < ./irmin/test/irmin-pack/test_inode.mli","success":true}
  {"sample_id":2853,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2852,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '72:55' -index 0 -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2851,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Dic -position '72:55' -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2850,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Dic -position '72:55' -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2849,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '72:55' -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2848,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '55:36' -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2847,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '120:51' -end '120:52' -filename ./irmin/test/irmin-pack/test_inode.ml < ./irmin/test/irmin-pack/test_inode.ml","success":true}
  {"sample_id":2846,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_indexing_strategy.mli < ./irmin/test/irmin-pack/test_indexing_strategy.mli","success":true}
  {"sample_id":2845,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:15' -index 0 -filename ./irmin/test/irmin-pack/test_indexing_strategy.mli < ./irmin/test/irmin-pack/test_indexing_strategy.mli","success":true}
  {"sample_id":2844,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/test_indexing_strategy.mli < ./irmin/test/irmin-pack/test_indexing_strategy.mli","success":true}
  {"sample_id":2843,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/test_indexing_strategy.mli < ./irmin/test/irmin-pack/test_indexing_strategy.mli","success":true}
  {"sample_id":2842,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:15' -filename ./irmin/test/irmin-pack/test_indexing_strategy.mli < ./irmin/test/irmin-pack/test_indexing_strategy.mli","success":true}
  {"sample_id":2839,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2838,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '54:41' -index 0 -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2837,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ms -position '54:41' -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2836,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ms -position '54:41' -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2835,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '54:41' -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2834,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '38:19' -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2833,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '40:16' -end '40:27' -filename ./irmin/test/irmin-pack/test_indexing_strategy.ml < ./irmin/test/irmin-pack/test_indexing_strategy.ml","success":true}
  {"sample_id":2832,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_hashes.mli < ./irmin/test/irmin-pack/test_hashes.mli","success":true}
  {"sample_id":2831,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:5' -index 0 -filename ./irmin/test/irmin-pack/test_hashes.mli < ./irmin/test/irmin-pack/test_hashes.mli","success":true}
  {"sample_id":2830,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '24:5' -filename ./irmin/test/irmin-pack/test_hashes.mli < ./irmin/test/irmin-pack/test_hashes.mli","success":true}
  {"sample_id":2829,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '24:5' -filename ./irmin/test/irmin-pack/test_hashes.mli < ./irmin/test/irmin-pack/test_hashes.mli","success":true}
  {"sample_id":2828,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:5' -filename ./irmin/test/irmin-pack/test_hashes.mli < ./irmin/test/irmin-pack/test_hashes.mli","success":true}
  {"sample_id":2825,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2824,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '105:44' -index 0 -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2823,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix unst -position '105:44' -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2822,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix unst -position '105:44' -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2821,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '105:44' -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2820,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '260:62' -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2819,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '260:43' -end '260:63' -filename ./irmin/test/irmin-pack/test_hashes.ml < ./irmin/test/irmin-pack/test_hashes.ml","success":true}
  {"sample_id":2818,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_gc.mli < ./irmin/test/irmin-pack/test_gc.mli","success":true}
  {"sample_id":2817,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2816,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1045:32' -index 0 -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2815,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix rep -position '1045:32' -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2814,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix rep -position '1045:32' -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2813,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1045:32' -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2812,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '636:62' -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2811,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '577:8' -end '580:28' -filename ./irmin/test/irmin-pack/test_gc.ml < ./irmin/test/irmin-pack/test_gc.ml","success":true}
  {"sample_id":2810,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_flush_reload.ml < ./irmin/test/irmin-pack/test_flush_reload.ml","success":true}
  {"sample_id":2809,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '240:2' -index 0 -filename ./irmin/test/irmin-pack/test_flush_reload.ml < ./irmin/test/irmin-pack/test_flush_reload.ml","success":true}
  {"sample_id":2806,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '240:2' -filename ./irmin/test/irmin-pack/test_flush_reload.ml < ./irmin/test/irmin-pack/test_flush_reload.ml","success":true}
  {"sample_id":2805,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '170:25' -filename ./irmin/test/irmin-pack/test_flush_reload.ml < ./irmin/test/irmin-pack/test_flush_reload.ml","success":true}
  {"sample_id":2804,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '150:12' -end '150:12' -filename ./irmin/test/irmin-pack/test_flush_reload.ml < ./irmin/test/irmin-pack/test_flush_reload.ml","success":true}
  {"sample_id":2803,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_existing_stores.mli < ./irmin/test/irmin-pack/test_existing_stores.mli","success":true}
  {"sample_id":2802,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:15' -index 0 -filename ./irmin/test/irmin-pack/test_existing_stores.mli < ./irmin/test/irmin-pack/test_existing_stores.mli","success":true}
  {"sample_id":2801,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/test_existing_stores.mli < ./irmin/test/irmin-pack/test_existing_stores.mli","success":true}
  {"sample_id":2800,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/test_existing_stores.mli < ./irmin/test/irmin-pack/test_existing_stores.mli","success":true}
  {"sample_id":2799,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:15' -filename ./irmin/test/irmin-pack/test_existing_stores.mli < ./irmin/test/irmin-pack/test_existing_stores.mli","success":true}
  {"sample_id":2796,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2795,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '194:56' -index 0 -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2794,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix rw -position '194:56' -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2793,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix rw -position '194:56' -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2792,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '194:56' -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2791,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '118:13' -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2790,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '130:14' -end '130:30' -filename ./irmin/test/irmin-pack/test_existing_stores.ml < ./irmin/test/irmin-pack/test_existing_stores.ml","success":true}
  {"sample_id":2789,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_dispatcher.mli < ./irmin/test/irmin-pack/test_dispatcher.mli","success":true}
  {"sample_id":2788,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin-pack/test_dispatcher.mli < ./irmin/test/irmin-pack/test_dispatcher.mli","success":true}
  {"sample_id":2787,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_dispatcher.mli < ./irmin/test/irmin-pack/test_dispatcher.mli","success":true}
  {"sample_id":2786,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_dispatcher.mli < ./irmin/test/irmin-pack/test_dispatcher.mli","success":true}
  {"sample_id":2785,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin-pack/test_dispatcher.mli < ./irmin/test/irmin-pack/test_dispatcher.mli","success":true}
  {"sample_id":2782,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2781,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '80:60' -index 0 -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2780,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Errs.raise -position '80:60' -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2779,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Errs.raise -position '80:60' -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2778,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '80:60' -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2777,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '54:2' -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2776,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '50:10' -end '50:25' -filename ./irmin/test/irmin-pack/test_dispatcher.ml < ./irmin/test/irmin-pack/test_dispatcher.ml","success":true}
  {"sample_id":2775,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_corrupted.mli < ./irmin/test/irmin-pack/test_corrupted.mli","success":true}
  {"sample_id":2774,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:15' -index 0 -filename ./irmin/test/irmin-pack/test_corrupted.mli < ./irmin/test/irmin-pack/test_corrupted.mli","success":true}
  {"sample_id":2773,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/test_corrupted.mli < ./irmin/test/irmin-pack/test_corrupted.mli","success":true}
  {"sample_id":2772,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/test_corrupted.mli < ./irmin/test/irmin-pack/test_corrupted.mli","success":true}
  {"sample_id":2771,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:15' -filename ./irmin/test/irmin-pack/test_corrupted.mli < ./irmin/test/irmin-pack/test_corrupted.mli","success":true}
  {"sample_id":2768,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2767,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '74:54' -index 0 -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2766,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix roo -position '74:54' -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2765,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix roo -position '74:54' -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2764,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '74:54' -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2763,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '75:11' -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2762,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '74:13' -end '74:13' -filename ./irmin/test/irmin-pack/test_corrupted.ml < ./irmin/test/irmin-pack/test_corrupted.ml","success":true}
  {"sample_id":2761,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_async.mli < ./irmin/test/irmin-pack/test_async.mli","success":true}
  {"sample_id":2760,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:43' -index 0 -filename ./irmin/test/irmin-pack/test_async.mli < ./irmin/test/irmin-pack/test_async.mli","success":true}
  {"sample_id":2759,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_async.mli < ./irmin/test/irmin-pack/test_async.mli","success":true}
  {"sample_id":2758,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '17:43' -filename ./irmin/test/irmin-pack/test_async.mli < ./irmin/test/irmin-pack/test_async.mli","success":true}
  {"sample_id":2757,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:43' -filename ./irmin/test/irmin-pack/test_async.mli < ./irmin/test/irmin-pack/test_async.mli","success":true}
  {"sample_id":2754,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test_async.ml < ./irmin/test/irmin-pack/test_async.ml","success":true}
  {"sample_id":2753,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '43:2' -index 0 -filename ./irmin/test/irmin-pack/test_async.ml < ./irmin/test/irmin-pack/test_async.ml","success":true}
  {"sample_id":2750,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '43:2' -filename ./irmin/test/irmin-pack/test_async.ml < ./irmin/test/irmin-pack/test_async.ml","success":true}
  {"sample_id":2749,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:50' -filename ./irmin/test/irmin-pack/test_async.ml < ./irmin/test/irmin-pack/test_async.ml","success":true}
  {"sample_id":2748,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '39:52' -end '40:20' -filename ./irmin/test/irmin-pack/test_async.ml < ./irmin/test/irmin-pack/test_async.ml","success":true}
  {"sample_id":2747,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test.mli < ./irmin/test/irmin-pack/test.mli","success":true}
  {"sample_id":2740,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2739,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:36' -index 0 -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2738,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix s -position '21:36' -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2737,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix s -position '21:36' -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2736,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:36' -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2735,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:38' -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2734,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:17' -end '21:38' -filename ./irmin/test/irmin-pack/test.ml < ./irmin/test/irmin-pack/test.ml","success":true}
  {"sample_id":2733,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/import.ml < ./irmin/test/irmin-pack/import.ml","success":true}
  {"sample_id":2732,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/test/irmin-pack/import.ml < ./irmin/test/irmin-pack/import.ml","success":true}
  {"sample_id":2731,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-pack/import.ml < ./irmin/test/irmin-pack/import.ml","success":true}
  {"sample_id":2730,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-pack/import.ml < ./irmin/test/irmin-pack/import.ml","success":true}
  {"sample_id":2729,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/test/irmin-pack/import.ml < ./irmin/test/irmin-pack/import.ml","success":true}
  {"sample_id":2728,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/test/irmin-pack/import.ml < ./irmin/test/irmin-pack/import.ml","success":true}
  {"sample_id":2726,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/common.mli < ./irmin/test/irmin-pack/common.mli","success":true}
  {"sample_id":2725,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2724,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '210:16' -index 0 -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2723,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ms -position '210:16' -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2722,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ms -position '210:16' -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2721,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '210:16' -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2720,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '344:10' -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2719,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '344:15' -end '344:24' -filename ./irmin/test/irmin-pack/common.ml < ./irmin/test/irmin-pack/common.ml","success":true}
  {"sample_id":2718,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-pack/_layered.mli < ./irmin/test/irmin-pack/_layered.mli","success":true}
  {"sample_id":2717,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:15' -index 0 -filename ./irmin/test/irmin-pack/_layered.mli < ./irmin/test/irmin-pack/_layered.mli","success":true}
  {"sample_id":2716,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/_layered.mli < ./irmin/test/irmin-pack/_layered.mli","success":true}
  {"sample_id":2715,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '17:15' -filename ./irmin/test/irmin-pack/_layered.mli < ./irmin/test/irmin-pack/_layered.mli","success":true}
  {"sample_id":2714,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:15' -filename ./irmin/test/irmin-pack/_layered.mli < ./irmin/test/irmin-pack/_layered.mli","success":true}
  {"sample_id":2711,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2710,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:28' -index 0 -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2709,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_mem -position '18:28' -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2708,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_mem -position '18:28' -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2707,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:28' -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2706,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:67' -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2705,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:48' -end '17:75' -filename ./irmin/test/irmin-mem/test_mem.ml < ./irmin/test/irmin-mem/test_mem.ml","success":true}
  {"sample_id":2704,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-mem/test.mli < ./irmin/test/irmin-mem/test.mli","success":true}
  {"sample_id":2697,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-mem/test.ml < ./irmin/test/irmin-mem/test.ml","success":true}
  {"sample_id":2696,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:34' -index 0 -filename ./irmin/test/irmin-mem/test.ml < ./irmin/test/irmin-mem/test.ml","success":true}
  {"sample_id":2693,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:34' -filename ./irmin/test/irmin-mem/test.ml < ./irmin/test/irmin-mem/test.ml","success":true}
  {"sample_id":2692,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:34' -filename ./irmin/test/irmin-mem/test.ml < ./irmin/test/irmin-mem/test.ml","success":true}
  {"sample_id":2691,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:9' -end '20:34' -filename ./irmin/test/irmin-mem/test.ml < ./irmin/test/irmin-mem/test.ml","success":true}
  {"sample_id":2690,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2689,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '23:25' -index 0 -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2688,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conf -position '23:25' -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2687,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conf -position '23:25' -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2686,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '23:25' -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2685,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '23:25' -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2684,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:20' -end '23:25' -filename ./irmin/test/irmin-mem/bench.ml < ./irmin/test/irmin-mem/bench.ml","success":true}
  {"sample_id":2683,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-http/test_http.mli < ./irmin/test/irmin-http/test_http.mli","success":true}
  {"sample_id":2682,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:34' -index 0 -filename ./irmin/test/irmin-http/test_http.mli < ./irmin/test/irmin-http/test_http.mli","success":true}
  {"sample_id":2681,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '20:34' -filename ./irmin/test/irmin-http/test_http.mli < ./irmin/test/irmin-http/test_http.mli","success":true}
  {"sample_id":2680,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '20:34' -filename ./irmin/test/irmin-http/test_http.mli < ./irmin/test/irmin-http/test_http.mli","success":true}
  {"sample_id":2679,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:34' -filename ./irmin/test/irmin-http/test_http.mli < ./irmin/test/irmin-http/test_http.mli","success":true}
  {"sample_id":2676,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-http/test_http.ml < ./irmin/test/irmin-http/test_http.ml","success":true}
  {"sample_id":2675,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '126:51' -index 0 -filename ./irmin/test/irmin-http/test_http.ml < ./irmin/test/irmin-http/test_http.ml","success":true}
  {"sample_id":2672,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '126:51' -filename ./irmin/test/irmin-http/test_http.ml < ./irmin/test/irmin-http/test_http.ml","success":true}
  {"sample_id":2671,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '80:28' -filename ./irmin/test/irmin-http/test_http.ml < ./irmin/test/irmin-http/test_http.ml","success":true}
  {"sample_id":2670,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '82:6' -end '83:42' -filename ./irmin/test/irmin-http/test_http.ml < ./irmin/test/irmin-http/test_http.ml","success":true}
  {"sample_id":2669,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-http/test.mli < ./irmin/test/irmin-http/test.mli","success":true}
  {"sample_id":2662,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2661,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:32' -index 0 -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2660,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix serv -position '20:32' -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2659,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix serv -position '20:32' -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2658,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:32' -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2657,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '19:47' -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2656,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:56' -end '19:69' -filename ./irmin/test/irmin-http/test.ml < ./irmin/test/irmin-http/test.ml","success":true}
  {"sample_id":2655,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-graphql/test.mli < ./irmin/test/irmin-graphql/test.mli","success":true}
  {"sample_id":2648,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2647,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '330:59' -index 0 -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2646,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix test_get_con -position '330:59' -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2645,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix test_get_con -position '330:59' -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2644,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '330:59' -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2643,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '169:66' -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2642,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '155:52' -end '155:76' -filename ./irmin/test/irmin-graphql/test.ml < ./irmin/test/irmin-graphql/test.ml","success":true}
  {"sample_id":2641,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-graphql/import.ml < ./irmin/test/irmin-graphql/import.ml","success":true}
  {"sample_id":2640,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/test/irmin-graphql/import.ml < ./irmin/test/irmin-graphql/import.ml","success":true}
  {"sample_id":2639,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-graphql/import.ml < ./irmin/test/irmin-graphql/import.ml","success":true}
  {"sample_id":2638,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-graphql/import.ml < ./irmin/test/irmin-graphql/import.ml","success":true}
  {"sample_id":2637,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/test/irmin-graphql/import.ml < ./irmin/test/irmin-graphql/import.ml","success":true}
  {"sample_id":2636,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/test/irmin-graphql/import.ml < ./irmin/test/irmin-graphql/import.ml","success":true}
  {"sample_id":2634,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-graphql/common.mli < ./irmin/test/irmin-graphql/common.mli","success":true}
  {"sample_id":2633,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2632,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '194:42' -index 0 -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2631,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix re -position '194:42' -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2630,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix re -position '194:42' -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2629,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '194:42' -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2628,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '139:55' -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2627,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '131:20' -end '131:20' -filename ./irmin/test/irmin-graphql/common.ml < ./irmin/test/irmin-graphql/common.ml","success":true}
  {"sample_id":2626,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2625,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:47' -index 0 -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2624,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix mis -position '22:47' -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2623,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix mis -position '22:47' -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2622,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:47' -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2621,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:21' -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2620,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:16' -end '18:21' -filename ./irmin/test/irmin-git/test_unix.ml < ./irmin/test/irmin-git/test_unix.ml","success":true}
  {"sample_id":2619,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2618,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '42:62' -index 0 -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2617,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conf -position '42:62' -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2616,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conf -position '42:62' -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2615,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '42:62' -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2614,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '51:34' -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2613,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '51:46' -end '51:52' -filename ./irmin/test/irmin-git/test_git_unix.ml < ./irmin/test/irmin-git/test_git_unix.ml","success":true}
  {"sample_id":2612,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-git/test_git.mli < ./irmin/test/irmin-git/test_git.mli","success":true}
  {"sample_id":2611,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:39' -index 0 -filename ./irmin/test/irmin-git/test_git.mli < ./irmin/test/irmin-git/test_git.mli","success":true}
  {"sample_id":2610,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '24:39' -filename ./irmin/test/irmin-git/test_git.mli < ./irmin/test/irmin-git/test_git.mli","success":true}
  {"sample_id":2609,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '24:39' -filename ./irmin/test/irmin-git/test_git.mli < ./irmin/test/irmin-git/test_git.mli","success":true}
  {"sample_id":2608,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:39' -filename ./irmin/test/irmin-git/test_git.mli < ./irmin/test/irmin-git/test_git.mli","success":true}
  {"sample_id":2607,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '29:25' -filename ./irmin/test/irmin-git/test_git.mli < ./irmin/test/irmin-git/test_git.mli","success":true}
  {"sample_id":2605,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2604,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '91:57' -index 0 -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2603,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix sto -position '91:57' -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2602,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix sto -position '91:57' -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2601,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '91:57' -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2600,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '209:16' -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2599,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '202:4' -end '205:21' -filename ./irmin/test/irmin-git/test_git.ml < ./irmin/test/irmin-git/test_git.ml","success":true}
  {"sample_id":2598,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-git/test.mli < ./irmin/test/irmin-git/test.mli","success":true}
  {"sample_id":2591,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2590,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:53' -index 0 -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2589,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix mis -position '21:53' -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2588,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix mis -position '21:53' -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2587,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:53' -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2586,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:68' -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2585,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:2' -end '21:3' -filename ./irmin/test/irmin-git/test.ml < ./irmin/test/irmin-git/test.ml","success":true}
  {"sample_id":2584,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-fs/test_unix.ml < ./irmin/test/irmin-fs/test_unix.ml","success":true}
  {"sample_id":2583,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:38' -index 0 -filename ./irmin/test/irmin-fs/test_unix.ml < ./irmin/test/irmin-fs/test_unix.ml","success":true}
  {"sample_id":2580,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:38' -filename ./irmin/test/irmin-fs/test_unix.ml < ./irmin/test/irmin-fs/test_unix.ml","success":true}
  {"sample_id":2579,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:36' -filename ./irmin/test/irmin-fs/test_unix.ml < ./irmin/test/irmin-fs/test_unix.ml","success":true}
  {"sample_id":2578,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:9' -end '21:36' -filename ./irmin/test/irmin-fs/test_unix.ml < ./irmin/test/irmin-fs/test_unix.ml","success":true}
  {"sample_id":2577,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-fs/test_fs_unix.ml < ./irmin/test/irmin-fs/test_fs_unix.ml","success":true}
  {"sample_id":2576,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '27:41' -index 0 -filename ./irmin/test/irmin-fs/test_fs_unix.ml < ./irmin/test/irmin-fs/test_fs_unix.ml","success":true}
  {"sample_id":2573,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '27:41' -filename ./irmin/test/irmin-fs/test_fs_unix.ml < ./irmin/test/irmin-fs/test_fs_unix.ml","success":true}
  {"sample_id":2572,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:26' -filename ./irmin/test/irmin-fs/test_fs_unix.ml < ./irmin/test/irmin-fs/test_fs_unix.ml","success":true}
  {"sample_id":2571,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '22:13' -end '22:26' -filename ./irmin/test/irmin-fs/test_fs_unix.ml < ./irmin/test/irmin-fs/test_fs_unix.ml","success":true}
  {"sample_id":2570,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2569,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:17' -index 0 -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2568,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_tes -position '26:17' -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2567,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_tes -position '26:17' -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2566,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:17' -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2565,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:67' -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2564,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:47' -end '21:64' -filename ./irmin/test/irmin-fs/test_fs.ml < ./irmin/test/irmin-fs/test_fs.ml","success":true}
  {"sample_id":2563,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-fs/test.mli < ./irmin/test/irmin-fs/test.mli","success":true}
  {"sample_id":2556,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-fs/test.ml < ./irmin/test/irmin-fs/test.ml","success":true}
  {"sample_id":2555,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:33' -index 0 -filename ./irmin/test/irmin-fs/test.ml < ./irmin/test/irmin-fs/test.ml","success":true}
  {"sample_id":2552,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:33' -filename ./irmin/test/irmin-fs/test.ml < ./irmin/test/irmin-fs/test.ml","success":true}
  {"sample_id":2551,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:33' -filename ./irmin/test/irmin-fs/test.ml < ./irmin/test/irmin-fs/test.ml","success":true}
  {"sample_id":2550,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:9' -end '20:33' -filename ./irmin/test/irmin-fs/test.ml < ./irmin/test/irmin-fs/test.ml","success":true}
  {"sample_id":2549,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/test.mli < ./irmin/test/irmin-containers/test.mli","success":true}
  {"sample_id":2542,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2541,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:29' -index 0 -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2540,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Linked_log. -position '24:29' -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2539,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Linked_log. -position '24:29' -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2538,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:29' -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2537,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '23:7' -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2536,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:7' -end '23:7' -filename ./irmin/test/irmin-containers/test.ml < ./irmin/test/irmin-containers/test.ml","success":true}
  {"sample_id":2535,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/lww_register.ml < ./irmin/test/irmin-containers/lww_register.ml","success":true}
  {"sample_id":2534,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '36:4' -index 0 -filename ./irmin/test/irmin-containers/lww_register.ml < ./irmin/test/irmin-containers/lww_register.ml","success":true}
  {"sample_id":2531,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '36:4' -filename ./irmin/test/irmin-containers/lww_register.ml < ./irmin/test/irmin-containers/lww_register.ml","success":true}
  {"sample_id":2530,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '69:35' -filename ./irmin/test/irmin-containers/lww_register.ml < ./irmin/test/irmin-containers/lww_register.ml","success":true}
  {"sample_id":2529,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '70:30' -end '70:30' -filename ./irmin/test/irmin-containers/lww_register.ml < ./irmin/test/irmin-containers/lww_register.ml","success":true}
  {"sample_id":2528,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/linked_log.ml < ./irmin/test/irmin-containers/linked_log.ml","success":true}
  {"sample_id":2527,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '60:51' -index 0 -filename ./irmin/test/irmin-containers/linked_log.ml < ./irmin/test/irmin-containers/linked_log.ml","success":true}
  {"sample_id":2524,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '60:51' -filename ./irmin/test/irmin-containers/linked_log.ml < ./irmin/test/irmin-containers/linked_log.ml","success":true}
  {"sample_id":2523,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '48:61' -filename ./irmin/test/irmin-containers/linked_log.ml < ./irmin/test/irmin-containers/linked_log.ml","success":true}
  {"sample_id":2522,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '49:16' -end '49:38' -filename ./irmin/test/irmin-containers/linked_log.ml < ./irmin/test/irmin-containers/linked_log.ml","success":true}
  {"sample_id":2521,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/import.ml < ./irmin/test/irmin-containers/import.ml","success":true}
  {"sample_id":2520,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/test/irmin-containers/import.ml < ./irmin/test/irmin-containers/import.ml","success":true}
  {"sample_id":2519,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-containers/import.ml < ./irmin/test/irmin-containers/import.ml","success":true}
  {"sample_id":2518,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-containers/import.ml < ./irmin/test/irmin-containers/import.ml","success":true}
  {"sample_id":2517,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/test/irmin-containers/import.ml < ./irmin/test/irmin-containers/import.ml","success":true}
  {"sample_id":2516,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/test/irmin-containers/import.ml < ./irmin/test/irmin-containers/import.ml","success":true}
  {"sample_id":2514,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/counter.ml < ./irmin/test/irmin-containers/counter.ml","success":true}
  {"sample_id":2513,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '70:38' -index 0 -filename ./irmin/test/irmin-containers/counter.ml < ./irmin/test/irmin-containers/counter.ml","success":true}
  {"sample_id":2510,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '70:38' -filename ./irmin/test/irmin-containers/counter.ml < ./irmin/test/irmin-containers/counter.ml","success":true}
  {"sample_id":2509,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '49:40' -filename ./irmin/test/irmin-containers/counter.ml < ./irmin/test/irmin-containers/counter.ml","success":true}
  {"sample_id":2508,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '50:2' -end '50:6' -filename ./irmin/test/irmin-containers/counter.ml < ./irmin/test/irmin-containers/counter.ml","success":true}
  {"sample_id":2507,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/common.mli < ./irmin/test/irmin-containers/common.mli","success":true}
  {"sample_id":2506,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:59' -index 0 -filename ./irmin/test/irmin-containers/common.mli < ./irmin/test/irmin-containers/common.mli","success":true}
  {"sample_id":2505,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '19:59' -filename ./irmin/test/irmin-containers/common.mli < ./irmin/test/irmin-containers/common.mli","success":true}
  {"sample_id":2504,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '19:59' -filename ./irmin/test/irmin-containers/common.mli < ./irmin/test/irmin-containers/common.mli","success":true}
  {"sample_id":2503,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:59' -filename ./irmin/test/irmin-containers/common.mli < ./irmin/test/irmin-containers/common.mli","success":true}
  {"sample_id":2500,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2499,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:13' -index 0 -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2498,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S.merge -position '21:13' -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2497,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S.merge -position '21:13' -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2496,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:13' -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2495,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:14' -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2494,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:72' -end '20:75' -filename ./irmin/test/irmin-containers/common.ml < ./irmin/test/irmin-containers/common.ml","success":true}
  {"sample_id":2493,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-containers/blob_log.ml < ./irmin/test/irmin-containers/blob_log.ml","success":true}
  {"sample_id":2492,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '40:24' -index 0 -filename ./irmin/test/irmin-containers/blob_log.ml < ./irmin/test/irmin-containers/blob_log.ml","success":true}
  {"sample_id":2489,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '40:24' -filename ./irmin/test/irmin-containers/blob_log.ml < ./irmin/test/irmin-containers/blob_log.ml","success":true}
  {"sample_id":2488,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '76:57' -filename ./irmin/test/irmin-containers/blob_log.ml < ./irmin/test/irmin-containers/blob_log.ml","success":true}
  {"sample_id":2487,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '67:2' -end '76:57' -filename ./irmin/test/irmin-containers/blob_log.ml < ./irmin/test/irmin-containers/blob_log.ml","success":true}
  {"sample_id":2486,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2485,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '42:20' -index 0 -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2484,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Alcotes -position '42:20' -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2483,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Alcotes -position '42:20' -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2482,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '42:20' -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2481,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '35:37' -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2480,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '38:6' -end '39:66' -filename ./irmin/test/irmin-cli/test.ml < ./irmin/test/irmin-cli/test.ml","success":true}
  {"sample_id":2479,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-chunk/test_chunk.ml < ./irmin/test/irmin-chunk/test_chunk.ml","success":true}
  {"sample_id":2478,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '56:16' -index 0 -filename ./irmin/test/irmin-chunk/test_chunk.ml < ./irmin/test/irmin-chunk/test_chunk.ml","success":true}
  {"sample_id":2475,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '56:16' -filename ./irmin/test/irmin-chunk/test_chunk.ml < ./irmin/test/irmin-chunk/test_chunk.ml","success":true}
  {"sample_id":2474,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '57:2' -filename ./irmin/test/irmin-chunk/test_chunk.ml < ./irmin/test/irmin-chunk/test_chunk.ml","success":true}
  {"sample_id":2473,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '74:4' -end '74:31' -filename ./irmin/test/irmin-chunk/test_chunk.ml < ./irmin/test/irmin-chunk/test_chunk.ml","success":true}
  {"sample_id":2472,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-chunk/test.mli < ./irmin/test/irmin-chunk/test.mli","success":true}
  {"sample_id":2465,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-chunk/test.ml < ./irmin/test/irmin-chunk/test.ml","success":true}
  {"sample_id":2464,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '57:8' -index 0 -filename ./irmin/test/irmin-chunk/test.ml < ./irmin/test/irmin-chunk/test.ml","success":true}
  {"sample_id":2461,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '57:8' -filename ./irmin/test/irmin-chunk/test.ml < ./irmin/test/irmin-chunk/test.ml","success":true}
  {"sample_id":2460,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '43:28' -filename ./irmin/test/irmin-chunk/test.ml < ./irmin/test/irmin-chunk/test.ml","success":true}
  {"sample_id":2459,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '43:4' -end '43:54' -filename ./irmin/test/irmin-chunk/test.ml < ./irmin/test/irmin-chunk/test.ml","success":true}
  {"sample_id":2458,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-bench/test.mli < ./irmin/test/irmin-bench/test.mli","success":true}
  {"sample_id":2451,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2450,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:56' -index 0 -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2449,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Replay.te -position '19:56' -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2448,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Replay.te -position '19:56' -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2447,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:56' -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2446,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '19:38' -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2445,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:38' -end '19:38' -filename ./irmin/test/irmin-bench/test.ml < ./irmin/test/irmin-bench/test.ml","success":true}
  {"sample_id":2444,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2443,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '138:59' -index 0 -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2442,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix tru -position '138:59' -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2441,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix tru -position '138:59' -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2440,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '138:59' -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2439,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '103:46' -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2438,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '101:2' -end '126:50' -filename ./irmin/test/irmin-bench/replay.ml < ./irmin/test/irmin-bench/replay.ml","success":true}
  {"sample_id":2437,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2436,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '38:38' -index 0 -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2435,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix f -position '38:38' -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2434,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix f -position '38:38' -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2433,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '38:38' -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2432,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:64' -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2431,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '47:23' -end '47:27' -filename ./irmin/test/irmin-bench/misc.ml < ./irmin/test/irmin-bench/misc.ml","success":true}
  {"sample_id":2430,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-bench/import.ml < ./irmin/test/irmin-bench/import.ml","success":true}
  {"sample_id":2429,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/test/irmin-bench/import.ml < ./irmin/test/irmin-bench/import.ml","success":true}
  {"sample_id":2428,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-bench/import.ml < ./irmin/test/irmin-bench/import.ml","success":true}
  {"sample_id":2427,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/test/irmin-bench/import.ml < ./irmin/test/irmin-bench/import.ml","success":true}
  {"sample_id":2426,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/test/irmin-bench/import.ml < ./irmin/test/irmin-bench/import.ml","success":true}
  {"sample_id":2425,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/test/irmin-bench/import.ml < ./irmin/test/irmin-bench/import.ml","success":true}
  {"sample_id":2423,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2422,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '57:23' -index 0 -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2421,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix flo -position '57:23' -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2420,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix flo -position '57:23' -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2419,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '57:23' -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2418,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '42:32' -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2417,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '41:2' -end '48:69' -filename ./irmin/test/irmin-bench/ema.ml < ./irmin/test/irmin-bench/ema.ml","success":true}
  {"sample_id":2416,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/ppx_irmin/ppx_irmin.mli < ./irmin/src/ppx_irmin/ppx_irmin.mli","success":true}
  {"sample_id":2409,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2408,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:27' -index 0 -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2407,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Plugins.regist -position '25:27' -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2406,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Plugins.regist -position '25:27' -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2405,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:27' -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2404,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '25:27' -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2403,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '24:2' -end '24:25' -filename ./irmin/src/ppx_irmin/ppx_irmin.ml < ./irmin/src/ppx_irmin/ppx_irmin.ml","success":true}
  {"sample_id":2402,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2401,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '27:12' -index 0 -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2400,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '27:12' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2399,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '27:12' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2398,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '27:12' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2397,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '25:22' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2396,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '24:23' -end '24:58' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal_lib.ml","success":true}
  {"sample_id":2395,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.mli < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.mli","success":true}
  {"sample_id":2388,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2387,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '90:28' -index 0 -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2386,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Source -position '90:28' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2385,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Source -position '90:28' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2384,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '90:28' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2383,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '80:28' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2382,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '78:12' -end '78:33' -filename ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml < ./irmin/src/ppx_irmin/internal/ppx_irmin_internal.ml","success":true}
  {"sample_id":2381,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/value.ml < ./irmin/src/libirmin/value.ml","success":true}
  {"sample_id":2380,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:28' -index 0 -filename ./irmin/src/libirmin/value.ml < ./irmin/src/libirmin/value.ml","success":true}
  {"sample_id":2377,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:28' -filename ./irmin/src/libirmin/value.ml < ./irmin/src/libirmin/value.ml","success":true}
  {"sample_id":2376,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '218:44' -filename ./irmin/src/libirmin/value.ml < ./irmin/src/libirmin/value.ml","success":true}
  {"sample_id":2375,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '206:29' -end '206:30' -filename ./irmin/src/libirmin/value.ml < ./irmin/src/libirmin/value.ml","success":true}
  {"sample_id":2374,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2373,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '220:31' -index 0 -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2372,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Array.o -position '220:31' -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2371,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Array.o -position '220:31' -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2370,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '220:31' -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2369,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '186:39' -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2368,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '269:44' -end '281:13' -filename ./irmin/src/libirmin/util.ml < ./irmin/src/libirmin/util.ml","success":true}
  {"sample_id":2367,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/types_intf.ml < ./irmin/src/libirmin/types_intf.ml","success":true}
  {"sample_id":2366,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '81:27' -index 0 -filename ./irmin/src/libirmin/types_intf.ml < ./irmin/src/libirmin/types_intf.ml","success":true}
  {"sample_id":2365,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Struct. -position '81:27' -filename ./irmin/src/libirmin/types_intf.ml < ./irmin/src/libirmin/types_intf.ml","success":true}
  {"sample_id":2364,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Struct. -position '81:27' -filename ./irmin/src/libirmin/types_intf.ml < ./irmin/src/libirmin/types_intf.ml","success":true}
  {"sample_id":2363,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '81:27' -filename ./irmin/src/libirmin/types_intf.ml < ./irmin/src/libirmin/types_intf.ml","success":true}
  {"sample_id":2362,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '57:18' -filename ./irmin/src/libirmin/types_intf.ml < ./irmin/src/libirmin/types_intf.ml","success":true}
  {"sample_id":2360,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/types.mli < ./irmin/src/libirmin/types.mli","success":true}
  {"sample_id":2359,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/libirmin/types.mli < ./irmin/src/libirmin/types.mli","success":true}
  {"sample_id":2358,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Types_in -position '17:22' -filename ./irmin/src/libirmin/types.mli < ./irmin/src/libirmin/types.mli","success":true}
  {"sample_id":2357,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Types_in -position '17:22' -filename ./irmin/src/libirmin/types.mli < ./irmin/src/libirmin/types.mli","success":true}
  {"sample_id":2356,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/libirmin/types.mli < ./irmin/src/libirmin/types.mli","success":true}
  {"sample_id":2355,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/libirmin/types.mli < ./irmin/src/libirmin/types.mli","success":true}
  {"sample_id":2353,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2352,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '46:27' -index 0 -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2351,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix pt -position '46:27' -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2350,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix pt -position '46:27' -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2349,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '46:27' -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2348,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '49:61' -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2347,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '49:33' -end '49:62' -filename ./irmin/src/libirmin/types.ml < ./irmin/src/libirmin/types.ml","success":true}
  {"sample_id":2346,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/type.ml < ./irmin/src/libirmin/type.ml","success":true}
  {"sample_id":2345,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '71:7' -index 0 -filename ./irmin/src/libirmin/type.ml < ./irmin/src/libirmin/type.ml","success":true}
  {"sample_id":2342,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '71:7' -filename ./irmin/src/libirmin/type.ml < ./irmin/src/libirmin/type.ml","success":true}
  {"sample_id":2341,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '54:47' -filename ./irmin/src/libirmin/type.ml < ./irmin/src/libirmin/type.ml","success":true}
  {"sample_id":2340,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '55:8' -end '55:21' -filename ./irmin/src/libirmin/type.ml < ./irmin/src/libirmin/type.ml","success":true}
  {"sample_id":2339,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2338,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '125:49' -index 0 -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2337,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Gene -position '125:49' -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2336,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Gene -position '125:49' -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2335,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '125:49' -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2334,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '91:27' -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2333,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '91:8' -end '98:31' -filename ./irmin/src/libirmin/tree.ml < ./irmin/src/libirmin/tree.ml","success":true}
  {"sample_id":2332,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2331,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '91:36' -index 0 -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2330,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store.c -position '91:36' -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2329,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store.c -position '91:36' -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2328,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '91:36' -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2327,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '76:33' -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2326,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '76:17' -end '76:32' -filename ./irmin/src/libirmin/store.ml < ./irmin/src/libirmin/store.ml","success":true}
  {"sample_id":2325,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2324,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '65:15' -index 0 -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2323,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix i -position '65:15' -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2322,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix i -position '65:15' -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2321,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '65:15' -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2320,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '159:39' -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2319,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '155:24' -end '155:27' -filename ./irmin/src/libirmin/repo.ml < ./irmin/src/libirmin/repo.ml","success":true}
  {"sample_id":2318,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2317,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '122:44' -index 0 -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2316,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix retur -position '122:44' -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2315,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix retur -position '122:44' -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2314,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '122:44' -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2313,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '94:42' -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2312,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '85:50' -end '85:58' -filename ./irmin/src/libirmin/path.ml < ./irmin/src/libirmin/path.ml","success":true}
  {"sample_id":2311,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/libirmin_bindings.ml < ./irmin/src/libirmin/libirmin_bindings.ml","success":true}
  {"sample_id":2310,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:21' -index 0 -filename ./irmin/src/libirmin/libirmin_bindings.ml < ./irmin/src/libirmin/libirmin_bindings.ml","success":true}
  {"sample_id":2309,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix I -position '24:21' -filename ./irmin/src/libirmin/libirmin_bindings.ml < ./irmin/src/libirmin/libirmin_bindings.ml","success":true}
  {"sample_id":2308,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix I -position '24:21' -filename ./irmin/src/libirmin/libirmin_bindings.ml < ./irmin/src/libirmin/libirmin_bindings.ml","success":true}
  {"sample_id":2307,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:21' -filename ./irmin/src/libirmin/libirmin_bindings.ml < ./irmin/src/libirmin/libirmin_bindings.ml","success":true}
  {"sample_id":2306,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:21' -filename ./irmin/src/libirmin/libirmin_bindings.ml < ./irmin/src/libirmin/libirmin_bindings.ml","success":true}
  {"sample_id":2304,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/lib/libirmin.ml < ./irmin/src/libirmin/lib/libirmin.ml","success":true}
  {"sample_id":2303,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:30' -index 0 -filename ./irmin/src/libirmin/lib/libirmin.ml < ./irmin/src/libirmin/lib/libirmin.ml","success":true}
  {"sample_id":2302,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Libirmin_bin -position '17:30' -filename ./irmin/src/libirmin/lib/libirmin.ml < ./irmin/src/libirmin/lib/libirmin.ml","success":true}
  {"sample_id":2301,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Libirmin_bin -position '17:30' -filename ./irmin/src/libirmin/lib/libirmin.ml < ./irmin/src/libirmin/lib/libirmin.ml","success":true}
  {"sample_id":2300,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:30' -filename ./irmin/src/libirmin/lib/libirmin.ml < ./irmin/src/libirmin/lib/libirmin.ml","success":true}
  {"sample_id":2299,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:47' -filename ./irmin/src/libirmin/lib/libirmin.ml < ./irmin/src/libirmin/lib/libirmin.ml","success":true}
  {"sample_id":2297,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2296,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '41:33' -index 0 -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2295,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix retur -position '41:33' -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2294,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix retur -position '41:33' -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2293,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '41:33' -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2292,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:59' -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2291,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '33:28' -end '37:78' -filename ./irmin/src/libirmin/info.ml < ./irmin/src/libirmin/info.ml","success":true}
  {"sample_id":2290,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/gen/generate.ml < ./irmin/src/libirmin/gen/generate.ml","success":true}
  {"sample_id":2289,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '79:4' -index 0 -filename ./irmin/src/libirmin/gen/generate.ml < ./irmin/src/libirmin/gen/generate.ml","success":true}
  {"sample_id":2286,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '79:4' -filename ./irmin/src/libirmin/gen/generate.ml < ./irmin/src/libirmin/gen/generate.ml","success":true}
  {"sample_id":2285,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '79:4' -filename ./irmin/src/libirmin/gen/generate.ml < ./irmin/src/libirmin/gen/generate.ml","success":true}
  {"sample_id":2284,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '69:6' -end '79:4' -filename ./irmin/src/libirmin/gen/generate.ml < ./irmin/src/libirmin/gen/generate.ml","success":true}
  {"sample_id":2283,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2282,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '46:71' -index 0 -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2281,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conte -position '46:71' -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2280,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conte -position '46:71' -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2279,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '46:71' -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2278,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '37:22' -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2277,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '40:4' -end '40:5' -filename ./irmin/src/libirmin/config.ml < ./irmin/src/libirmin/config.ml","success":true}
  {"sample_id":2276,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2275,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '117:21' -index 0 -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2274,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix comm -position '117:21' -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2273,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix comm -position '117:21' -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2272,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '117:21' -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2271,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '155:45' -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2270,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '154:22' -end '154:43' -filename ./irmin/src/libirmin/commit.ml < ./irmin/src/libirmin/commit.ml","success":true}
  {"sample_id":2269,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2268,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '54:13' -index 0 -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2267,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ke -position '54:13' -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2266,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ke -position '54:13' -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2265,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '54:13' -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2264,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '35:25' -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2263,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '38:2' -end '40:16' -filename ./irmin/src/irmin/watch_intf.ml < ./irmin/src/irmin/watch_intf.ml","success":true}
  {"sample_id":2262,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2261,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:22' -index 0 -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2260,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Watch_in -position '20:22' -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2259,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Watch_in -position '20:22' -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2258,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:22' -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2257,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:13' -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2256,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '18:13' -filename ./irmin/src/irmin/watch.mli < ./irmin/src/irmin/watch.mli","success":true}
  {"sample_id":2255,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2254,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '134:13' -index 0 -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2253,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '134:13' -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2252,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '134:13' -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2251,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '134:13' -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2250,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '102:37' -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2249,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '102:54' -end '102:56' -filename ./irmin/src/irmin/watch.ml < ./irmin/src/irmin/watch.ml","success":true}
  {"sample_id":2248,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/version.ml < ./irmin/src/irmin/version.ml","success":true}
  {"sample_id":2243,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:26' -filename ./irmin/src/irmin/version.ml < ./irmin/src/irmin/version.ml","success":true}
  {"sample_id":2242,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:14' -end '17:26' -filename ./irmin/src/irmin/version.ml < ./irmin/src/irmin/version.ml","success":true}
  {"sample_id":2241,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2240,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:34' -index 0 -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2239,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '25:34' -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2238,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '25:34' -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2237,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:34' -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2236,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '19:59' -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2235,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '19:59' -filename ./irmin/src/irmin/unix/irmin_unix.mli < ./irmin/src/irmin/unix/irmin_unix.mli","success":true}
  {"sample_id":2234,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2233,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:13' -index 0 -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2232,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix I. -position '20:13' -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2231,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix I. -position '20:13' -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2230,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:13' -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2229,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:13' -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2228,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:11' -end '20:13' -filename ./irmin/src/irmin/unix/irmin_unix.ml < ./irmin/src/irmin/unix/irmin_unix.ml","success":true}
  {"sample_id":2227,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/unix/info.mli < ./irmin/src/irmin/unix/info.mli","success":true}
  {"sample_id":2226,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2225,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '31:16' -index 0 -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2224,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix auth -position '31:16' -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2223,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix auth -position '31:16' -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2222,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '31:16' -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2221,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '31:30' -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2220,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '29:29' -end '29:30' -filename ./irmin/src/irmin/unix/info.ml < ./irmin/src/irmin/unix/info.ml","success":true}
  {"sample_id":2219,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/type.mli < ./irmin/src/irmin/type.mli","success":true}
  {"sample_id":2218,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/type.ml < ./irmin/src/irmin/type.ml","success":true}
  {"sample_id":2217,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:16' -index 0 -filename ./irmin/src/irmin/type.ml < ./irmin/src/irmin/type.ml","success":true}
  {"sample_id":2216,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '22:16' -filename ./irmin/src/irmin/type.ml < ./irmin/src/irmin/type.ml","success":true}
  {"sample_id":2215,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '22:16' -filename ./irmin/src/irmin/type.ml < ./irmin/src/irmin/type.ml","success":true}
  {"sample_id":2214,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:16' -filename ./irmin/src/irmin/type.ml < ./irmin/src/irmin/type.ml","success":true}
  {"sample_id":2213,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:11' -filename ./irmin/src/irmin/type.ml < ./irmin/src/irmin/type.ml","success":true}
  {"sample_id":2211,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2210,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '448:45' -index 0 -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2209,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix B.Node.Me -position '448:45' -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2208,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix B.Node.Me -position '448:45' -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2207,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '448:45' -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2206,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '246:17' -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2205,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '255:2' -end '255:57' -filename ./irmin/src/irmin/tree_intf.ml < ./irmin/src/irmin/tree_intf.ml","success":true}
  {"sample_id":2204,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2203,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:21' -index 0 -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2202,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Tree_int -position '18:21' -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2201,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Tree_int -position '18:21' -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2200,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:21' -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2199,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:21' -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2198,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:0' -end '19:13' -filename ./irmin/src/irmin/tree.mli < ./irmin/src/irmin/tree.mli","success":true}
  {"sample_id":2197,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/tree.ml < ./irmin/src/irmin/tree.ml","success":true}
  {"sample_id":2196,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '2794:26' -index 0 -filename ./irmin/src/irmin/tree.ml < ./irmin/src/irmin/tree.ml","success":true}
  {"sample_id":2193,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '2794:26' -filename ./irmin/src/irmin/tree.ml < ./irmin/src/irmin/tree.ml","success":true}
  {"sample_id":2192,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '2318:68' -filename ./irmin/src/irmin/tree.ml < ./irmin/src/irmin/tree.ml","success":true}
  {"sample_id":2191,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '1941:30' -end '1941:33' -filename ./irmin/src/irmin/tree.ml < ./irmin/src/irmin/tree.ml","success":true}
  {"sample_id":2190,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2189,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '60:36' -index 0 -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2188,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lwt -position '60:36' -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2187,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lwt -position '60:36' -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2186,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '60:36' -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2185,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '30:33' -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2184,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '33:2' -end '33:69' -filename ./irmin/src/irmin/sync_intf.ml < ./irmin/src/irmin/sync_intf.ml","success":true}
  {"sample_id":2183,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2182,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:21' -index 0 -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2181,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Sync_int -position '19:21' -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2180,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Sync_int -position '19:21' -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2179,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:21' -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2178,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:13' -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2177,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:28' -filename ./irmin/src/irmin/sync.mli < ./irmin/src/irmin/sync.mli","success":true}
  {"sample_id":2176,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2175,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '168:25' -index 0 -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2174,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix x -position '168:25' -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2173,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix x -position '168:25' -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2172,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '168:25' -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2171,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '118:72' -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2170,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '111:22' -end '111:24' -filename ./irmin/src/irmin/sync.ml < ./irmin/src/irmin/sync.ml","success":true}
  {"sample_id":2169,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2168,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:30' -index 0 -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2167,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix rea -position '22:30' -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2166,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix rea -position '22:30' -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2165,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:30' -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2164,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:51' -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2163,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '55:4' -end '55:17' -filename ./irmin/src/irmin/store_properties_intf.ml < ./irmin/src/irmin/store_properties_intf.ml","success":true}
  {"sample_id":2162,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2161,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:33' -index 0 -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2160,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store_properti -position '17:33' -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2159,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store_properti -position '17:33' -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2158,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:33' -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2157,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:33' -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2156,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/store_properties.mli < ./irmin/src/irmin/store_properties.mli","success":true}
  {"sample_id":2155,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/store_properties.ml < ./irmin/src/irmin/store_properties.ml","success":true}
  {"sample_id":2154,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:28' -index 0 -filename ./irmin/src/irmin/store_properties.ml < ./irmin/src/irmin/store_properties.ml","success":true}
  {"sample_id":2153,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store_prope -position '17:28' -filename ./irmin/src/irmin/store_properties.ml < ./irmin/src/irmin/store_properties.ml","success":true}
  {"sample_id":2152,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store_prope -position '17:28' -filename ./irmin/src/irmin/store_properties.ml < ./irmin/src/irmin/store_properties.ml","success":true}
  {"sample_id":2151,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:28' -filename ./irmin/src/irmin/store_properties.ml < ./irmin/src/irmin/store_properties.ml","success":true}
  {"sample_id":2150,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:28' -filename ./irmin/src/irmin/store_properties.ml < ./irmin/src/irmin/store_properties.ml","success":true}
  {"sample_id":2148,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2147,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1110:31' -index 0 -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2146,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Backend.Com -position '1110:31' -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2145,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Backend.Com -position '1110:31' -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2144,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1110:31' -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2143,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '1077:62' -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2142,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '1187:2' -end '1187:61' -filename ./irmin/src/irmin/store_intf.ml < ./irmin/src/irmin/store_intf.ml","success":true}
  {"sample_id":2141,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2140,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:22' -index 0 -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2139,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store_in -position '20:22' -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2138,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store_in -position '20:22' -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2137,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:22' -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2136,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:13' -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2135,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '18:17' -filename ./irmin/src/irmin/store.mli < ./irmin/src/irmin/store.mli","success":true}
  {"sample_id":2134,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2133,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '852:10' -index 0 -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2132,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix k -position '852:10' -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2131,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix k -position '852:10' -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2130,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '852:10' -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2129,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '1234:58' -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2128,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '1057:4' -end '1057:13' -filename ./irmin/src/irmin/store.ml < ./irmin/src/irmin/store.ml","success":true}
  {"sample_id":2127,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2126,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '35:19' -index 0 -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2125,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ke -position '35:19' -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2124,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ke -position '35:19' -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2123,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '35:19' -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2122,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '62:2' -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2121,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '39:2' -end '39:48' -filename ./irmin/src/irmin/storage_intf.ml < ./irmin/src/irmin/storage_intf.ml","success":true}
  {"sample_id":2120,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/storage.mli < ./irmin/src/irmin/storage.mli","success":true}
  {"sample_id":2119,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2118,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '58:21' -index 0 -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2117,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ad -position '58:21' -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2116,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ad -position '58:21' -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2115,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '58:21' -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2114,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '133:28' -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2113,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '135:20' -end '135:20' -filename ./irmin/src/irmin/storage.ml < ./irmin/src/irmin/storage.ml","success":true}
  {"sample_id":2112,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2111,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '36:17' -index 0 -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2110,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '36:17' -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2109,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '36:17' -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2108,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '36:17' -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2107,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '29:30' -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2106,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '30:2' -end '30:38' -filename ./irmin/src/irmin/slice_intf.ml < ./irmin/src/irmin/slice_intf.ml","success":true}
  {"sample_id":2105,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2104,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2103,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Slice_in -position '17:22' -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2102,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Slice_in -position '17:22' -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2101,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2100,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2099,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/slice.mli < ./irmin/src/irmin/slice.mli","success":true}
  {"sample_id":2098,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2097,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '54:60' -index 0 -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2096,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conte -position '54:60' -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2095,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conte -position '54:60' -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2094,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '54:60' -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2093,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '54:60' -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2092,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '57:6' -end '57:6' -filename ./irmin/src/irmin/slice.ml < ./irmin/src/irmin/slice.ml","success":true}
  {"sample_id":2091,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/schema.ml < ./irmin/src/irmin/schema.ml","success":true}
  {"sample_id":2090,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '38:23' -index 0 -filename ./irmin/src/irmin/schema.ml < ./irmin/src/irmin/schema.ml","success":true}
  {"sample_id":2089,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix node_ -position '38:23' -filename ./irmin/src/irmin/schema.ml < ./irmin/src/irmin/schema.ml","success":true}
  {"sample_id":2088,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix node_ -position '38:23' -filename ./irmin/src/irmin/schema.ml < ./irmin/src/irmin/schema.ml","success":true}
  {"sample_id":2087,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '38:23' -filename ./irmin/src/irmin/schema.ml < ./irmin/src/irmin/schema.ml","success":true}
  {"sample_id":2086,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '69:32' -filename ./irmin/src/irmin/schema.ml < ./irmin/src/irmin/schema.ml","success":true}
  {"sample_id":2084,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2083,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:24' -index 0 -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2082,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '25:24' -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2081,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '25:24' -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2080,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:24' -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2079,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:54' -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2078,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '21:54' -filename ./irmin/src/irmin/reversed_list.mli < ./irmin/src/irmin/reversed_list.mli","success":true}
  {"sample_id":2077,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2076,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:17' -index 0 -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2075,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix List. -position '20:17' -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2074,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix List. -position '20:17' -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2073,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:17' -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2072,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:17' -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2071,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:22' -end '19:24' -filename ./irmin/src/irmin/reversed_list.ml < ./irmin/src/irmin/reversed_list.ml","success":true}
  {"sample_id":2070,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2069,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '64:29' -index 0 -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2068,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix H. -position '64:29' -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2067,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix H. -position '64:29' -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2066,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '64:29' -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2065,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '26:33' -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2064,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '29:2' -end '29:32' -filename ./irmin/src/irmin/remote_intf.ml < ./irmin/src/irmin/remote_intf.ml","success":true}
  {"sample_id":2063,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2062,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:23' -index 0 -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2061,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Remote_in -position '19:23' -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2060,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Remote_in -position '19:23' -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2059,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:23' -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2058,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:20' -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2057,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:20' -filename ./irmin/src/irmin/remote.mli < ./irmin/src/irmin/remote.mli","success":true}
  {"sample_id":2056,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2055,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:18' -index 0 -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2054,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix H. -position '25:18' -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2053,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix H. -position '25:18' -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2052,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:18' -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2051,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '29:63' -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2050,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '28:26' -end '28:28' -filename ./irmin/src/irmin/remote.ml < ./irmin/src/irmin/remote.ml","success":true}
  {"sample_id":2049,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2048,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '39:39' -index 0 -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2047,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix val -position '39:39' -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2046,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix val -position '39:39' -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2045,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '39:39' -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2044,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '57:2' -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2043,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '44:2' -end '44:15' -filename ./irmin/src/irmin/read_only_intf.ml < ./irmin/src/irmin/read_only_intf.ml","success":true}
  {"sample_id":2042,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2041,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:26' -index 0 -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2040,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Read_only_ -position '17:26' -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2039,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Read_only_ -position '17:26' -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2038,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:26' -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2037,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:26' -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2036,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/read_only.mli < ./irmin/src/irmin/read_only.mli","success":true}
  {"sample_id":2035,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/read_only.ml < ./irmin/src/irmin/read_only.ml","success":true}
  {"sample_id":2034,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:21' -index 0 -filename ./irmin/src/irmin/read_only.ml < ./irmin/src/irmin/read_only.ml","success":true}
  {"sample_id":2033,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Read_onl -position '17:21' -filename ./irmin/src/irmin/read_only.ml < ./irmin/src/irmin/read_only.ml","success":true}
  {"sample_id":2032,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Read_onl -position '17:21' -filename ./irmin/src/irmin/read_only.ml < ./irmin/src/irmin/read_only.ml","success":true}
  {"sample_id":2031,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:21' -filename ./irmin/src/irmin/read_only.ml < ./irmin/src/irmin/read_only.ml","success":true}
  {"sample_id":2030,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:21' -filename ./irmin/src/irmin/read_only.ml < ./irmin/src/irmin/read_only.ml","success":true}
  {"sample_id":2028,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2027,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '166:28' -index 0 -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2026,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix irm -position '166:28' -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2025,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix irm -position '166:28' -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2024,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '166:28' -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2023,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '333:34' -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2022,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '167:2' -end '171:59' -filename ./irmin/src/irmin/proof_intf.ml < ./irmin/src/irmin/proof_intf.ml","success":true}
  {"sample_id":2021,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/proof.mli < ./irmin/src/irmin/proof.mli","success":true}
  {"sample_id":2020,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:23' -index 0 -filename ./irmin/src/irmin/proof.mli < ./irmin/src/irmin/proof.mli","success":true}
  {"sample_id":2019,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Proof_int -position '17:23' -filename ./irmin/src/irmin/proof.mli < ./irmin/src/irmin/proof.mli","success":true}
  {"sample_id":2018,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Proof_int -position '17:23' -filename ./irmin/src/irmin/proof.mli < ./irmin/src/irmin/proof.mli","success":true}
  {"sample_id":2017,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:23' -filename ./irmin/src/irmin/proof.mli < ./irmin/src/irmin/proof.mli","success":true}
  {"sample_id":2016,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:23' -filename ./irmin/src/irmin/proof.mli < ./irmin/src/irmin/proof.mli","success":true}
  {"sample_id":2014,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2013,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '253:11' -index 0 -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2012,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '253:11' -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2011,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '253:11' -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2010,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '253:11' -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2009,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '250:16' -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2008,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '237:9' -end '237:9' -filename ./irmin/src/irmin/proof.ml < ./irmin/src/irmin/proof.ml","success":true}
  {"sample_id":2007,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2006,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '65:29' -index 0 -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2005,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Read_wr -position '65:29' -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2004,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Read_wr -position '65:29' -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2003,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '65:29' -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2002,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '60:61' -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2001,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '66:0' -end '66:80' -filename ./irmin/src/irmin/perms.ml < ./irmin/src/irmin/perms.ml","success":true}
  {"sample_id":2000,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1999,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '41:24' -index 0 -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1998,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ste -position '41:24' -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1997,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ste -position '41:24' -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1996,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '41:24' -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1995,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '58:47' -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1994,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '63:2' -end '63:42' -filename ./irmin/src/irmin/path_intf.ml < ./irmin/src/irmin/path_intf.ml","success":true}
  {"sample_id":1993,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1992,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:21' -index 0 -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1991,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Path_int -position '19:21' -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1990,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Path_int -position '19:21' -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1989,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:21' -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1988,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:25' -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1987,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:25' -filename ./irmin/src/irmin/path.mli < ./irmin/src/irmin/path.mli","success":true}
  {"sample_id":1986,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1985,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '28:53' -index 0 -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1984,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix h -position '28:53' -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1983,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix h -position '28:53' -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1982,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '28:53' -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1981,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '27:26' -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1980,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '27:18' -end '27:18' -filename ./irmin/src/irmin/path.ml < ./irmin/src/irmin/path.ml","success":true}
  {"sample_id":1979,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1978,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:21' -index 0 -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1977,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix vert -position '26:21' -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1976,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix vert -position '26:21' -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1975,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:21' -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1974,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:29' -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1973,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '33:2' -end '33:30' -filename ./irmin/src/irmin/object_graph_intf.ml < ./irmin/src/irmin/object_graph_intf.ml","success":true}
  {"sample_id":1972,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1971,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:29' -index 0 -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1970,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Object_graph -position '19:29' -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1969,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Object_graph -position '19:29' -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1968,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:29' -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1967,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:13' -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1966,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:13' -filename ./irmin/src/irmin/object_graph.mli < ./irmin/src/irmin/object_graph.mli","success":true}
  {"sample_id":1965,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1964,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '187:40' -index 0 -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1963,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix X. -position '187:40' -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1962,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix X. -position '187:40' -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1961,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '187:40' -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1960,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '225:16' -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1959,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '214:9' -end '214:32' -filename ./irmin/src/irmin/object_graph.ml < ./irmin/src/irmin/object_graph.ml","success":true}
  {"sample_id":1958,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1957,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '456:28' -index 0 -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1956,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix C. -position '456:28' -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1955,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix C. -position '456:28' -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1954,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '456:28' -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1953,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '128:34' -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1952,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '140:2' -end '143:52' -filename ./irmin/src/irmin/node_intf.ml < ./irmin/src/irmin/node_intf.ml","success":true}
  {"sample_id":1951,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1950,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:21' -index 0 -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1949,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Node_int -position '26:21' -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1948,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Node_int -position '26:21' -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1947,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:21' -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1946,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:74' -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1945,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '24:74' -filename ./irmin/src/irmin/node.mli < ./irmin/src/irmin/node.mli","success":true}
  {"sample_id":1944,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1943,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '741:30' -index 0 -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1942,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Typ -position '741:30' -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1941,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Typ -position '741:30' -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1940,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '741:30' -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1939,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '585:13' -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1938,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '568:19' -end '568:24' -filename ./irmin/src/irmin/node.ml < ./irmin/src/irmin/node.ml","success":true}
  {"sample_id":1937,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1936,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:15' -index 0 -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1935,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '29:15' -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1934,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '29:15' -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1933,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:15' -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1932,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:80' -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1931,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '20:80' -filename ./irmin/src/irmin/metrics.mli < ./irmin/src/irmin/metrics.mli","success":true}
  {"sample_id":1930,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1929,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '42:6' -index 0 -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1928,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ui -position '42:6' -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1927,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ui -position '42:6' -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1926,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '42:6' -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1925,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '42:60' -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1924,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '34:20' -end '34:20' -filename ./irmin/src/irmin/metrics.ml < ./irmin/src/irmin/metrics.ml","success":true}
  {"sample_id":1923,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1922,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:35' -index 0 -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1921,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '29:35' -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1920,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '29:35' -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1919,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:35' -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1918,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '30:66' -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1917,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '30:2' -end '30:66' -filename ./irmin/src/irmin/metadata_intf.ml < ./irmin/src/irmin/metadata_intf.ml","success":true}
  {"sample_id":1916,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1915,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:25' -index 0 -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1914,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Metadata_i -position '17:25' -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1913,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Metadata_i -position '17:25' -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1912,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:25' -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1911,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:25' -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1910,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/metadata.mli < ./irmin/src/irmin/metadata.mli","success":true}
  {"sample_id":1909,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1908,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '23:52' -index 0 -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1907,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Merge -position '23:52' -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1906,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Merge -position '23:52' -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1905,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '23:52' -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1904,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '23:52' -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1903,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:45' -end '23:55' -filename ./irmin/src/irmin/metadata.ml < ./irmin/src/irmin/metadata.ml","success":true}
  {"sample_id":1902,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/merge.mli < ./irmin/src/irmin/merge.mli","success":true}
  {"sample_id":1901,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1900,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '131:46' -index 0 -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1899,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix x -position '131:46' -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1898,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix x -position '131:46' -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1897,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '131:46' -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1896,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '96:40' -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1895,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '90:39' -end '90:40' -filename ./irmin/src/irmin/merge.ml < ./irmin/src/irmin/merge.ml","success":true}
  {"sample_id":1894,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/mem/irmin_mem.mli < ./irmin/src/irmin/mem/irmin_mem.mli","success":true}
  {"sample_id":1893,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1892,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '120:36' -index 0 -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1891,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ke -position '120:36' -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1890,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ke -position '120:36' -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1889,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '120:36' -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1888,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '91:39' -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1887,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '103:17' -end '103:17' -filename ./irmin/src/irmin/mem/irmin_mem.ml < ./irmin/src/irmin/mem/irmin_mem.ml","success":true}
  {"sample_id":1886,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/mem/import.ml < ./irmin/src/irmin/mem/import.ml","success":true}
  {"sample_id":1885,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin/mem/import.ml < ./irmin/src/irmin/mem/import.ml","success":true}
  {"sample_id":1884,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin/mem/import.ml < ./irmin/src/irmin/mem/import.ml","success":true}
  {"sample_id":1883,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin/mem/import.ml < ./irmin/src/irmin/mem/import.ml","success":true}
  {"sample_id":1882,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin/mem/import.ml < ./irmin/src/irmin/mem/import.ml","success":true}
  {"sample_id":1881,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin/mem/import.ml < ./irmin/src/irmin/mem/import.ml","success":true}
  {"sample_id":1879,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/lru.mli < ./irmin/src/irmin/lru.mli","success":true}
  {"sample_id":1878,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1877,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '39:15' -index 0 -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1876,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix nex -position '39:15' -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1875,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix nex -position '39:15' -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1874,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '39:15' -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1873,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:21' -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1872,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '40:20' -end '40:23' -filename ./irmin/src/irmin/lru.ml < ./irmin/src/irmin/lru.ml","success":true}
  {"sample_id":1871,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1870,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '31:30' -index 0 -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1869,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix in -position '31:30' -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1868,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix in -position '31:30' -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1867,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '31:30' -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1866,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '40:26' -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1865,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '47:2' -end '47:76' -filename ./irmin/src/irmin/logging_intf.ml < ./irmin/src/irmin/logging_intf.ml","success":true}
  {"sample_id":1864,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1863,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:24' -index 0 -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1862,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Logging_i -position '17:24' -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1861,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Logging_i -position '17:24' -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1860,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:24' -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1859,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:24' -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1858,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/logging.mli < ./irmin/src/irmin/logging.mli","success":true}
  {"sample_id":1857,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/logging.ml < ./irmin/src/irmin/logging.ml","success":true}
  {"sample_id":1856,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:23' -index 0 -filename ./irmin/src/irmin/logging.ml < ./irmin/src/irmin/logging.ml","success":true}
  {"sample_id":1853,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:23' -filename ./irmin/src/irmin/logging.ml < ./irmin/src/irmin/logging.ml","success":true}
  {"sample_id":1852,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '52:40' -filename ./irmin/src/irmin/logging.ml < ./irmin/src/irmin/logging.ml","success":true}
  {"sample_id":1851,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '47:21' -end '47:46' -filename ./irmin/src/irmin/logging.ml < ./irmin/src/irmin/logging.ml","success":true}
  {"sample_id":1850,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/lock.mli < ./irmin/src/irmin/lock.mli","success":true}
  {"sample_id":1849,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1848,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '63:49' -index 0 -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1847,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix loc -position '63:49' -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1846,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix loc -position '63:49' -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1845,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '63:49' -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1844,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '57:33' -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1843,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '58:11' -end '58:28' -filename ./irmin/src/irmin/lock.ml < ./irmin/src/irmin/lock.ml","success":true}
  {"sample_id":1842,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1841,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:24' -index 0 -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1840,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '29:24' -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1839,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '29:24' -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1838,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:24' -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1837,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '37:4' -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1836,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '52:2' -end '53:78' -filename ./irmin/src/irmin/key_intf.ml < ./irmin/src/irmin/key_intf.ml","success":true}
  {"sample_id":1835,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1834,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '91:20' -index 0 -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1833,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Key_int -position '91:20' -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1832,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Key_int -position '91:20' -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1831,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '91:20' -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1830,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '92:13' -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1829,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '89:37' -filename ./irmin/src/irmin/key.mli < ./irmin/src/irmin/key.mli","success":true}
  {"sample_id":1828,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1827,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:34' -index 0 -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1826,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix irm -position '20:34' -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1825,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix irm -position '20:34' -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1824,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:34' -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1823,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:34' -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1822,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:18' -end '23:18' -filename ./irmin/src/irmin/key.ml < ./irmin/src/irmin/key.ml","success":true}
  {"sample_id":1821,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/irmin.mli < ./irmin/src/irmin/irmin.mli","success":true}
  {"sample_id":1820,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1819,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '219:21' -index 0 -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1818,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Remot -position '219:21' -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1817,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Remot -position '219:21' -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1816,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '219:21' -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1815,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '223:31' -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1814,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '129:38' -end '129:43' -filename ./irmin/src/irmin/irmin.ml < ./irmin/src/irmin/irmin.ml","success":true}
  {"sample_id":1813,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1812,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '64:19' -index 0 -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1811,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S -position '64:19' -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1810,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S -position '64:19' -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1809,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '64:19' -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1808,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:21' -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1807,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:21' -filename ./irmin/src/irmin/info_intf.ml < ./irmin/src/irmin/info_intf.ml","success":true}
  {"sample_id":1806,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1805,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:21' -index 0 -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1804,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Info_int -position '17:21' -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1803,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Info_int -position '17:21' -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1802,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:21' -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1801,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:21' -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1800,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/info.mli < ./irmin/src/irmin/info.mli","success":true}
  {"sample_id":1799,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1798,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '35:20' -index 0 -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1797,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix dat -position '35:20' -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1796,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix dat -position '35:20' -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1795,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '35:20' -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1794,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '33:27' -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1793,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '33:16' -end '33:16' -filename ./irmin/src/irmin/info.ml < ./irmin/src/irmin/info.ml","success":true}
  {"sample_id":1792,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1791,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '139:39' -index 0 -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1790,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '139:39' -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1789,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '139:39' -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1788,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '139:39' -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1787,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '114:2' -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1786,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '76:0' -end '77:66' -filename ./irmin/src/irmin/indexable_intf.ml < ./irmin/src/irmin/indexable_intf.ml","success":true}
  {"sample_id":1785,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1784,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:26' -index 0 -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1783,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Indexable_ -position '18:26' -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1782,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Indexable_ -position '18:26' -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1781,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:26' -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1780,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:26' -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1779,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:0' -end '19:13' -filename ./irmin/src/irmin/indexable.mli < ./irmin/src/irmin/indexable.mli","success":true}
  {"sample_id":1778,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1777,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '44:36' -index 0 -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1776,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '44:36' -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1775,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '44:36' -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1774,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '44:36' -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1773,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '43:32' -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1772,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '44:40' -end '44:40' -filename ./irmin/src/irmin/indexable.ml < ./irmin/src/irmin/indexable.ml","success":true}
  {"sample_id":1771,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1770,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '45:42' -index 0 -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1769,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix a -position '45:42' -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1768,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix a -position '45:42' -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1767,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '45:42' -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1766,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '48:73' -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1765,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '48:28' -end '48:32' -filename ./irmin/src/irmin/import.ml < ./irmin/src/irmin/import.ml","success":true}
  {"sample_id":1764,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1763,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '60:10' -index 0 -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1762,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '60:10' -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1761,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '60:10' -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1760,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '60:10' -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1759,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '78:23' -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1758,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '95:2' -end '95:34' -filename ./irmin/src/irmin/hash_intf.ml < ./irmin/src/irmin/hash_intf.ml","success":true}
  {"sample_id":1757,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1756,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:21' -index 0 -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1755,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Hash_int -position '17:21' -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1754,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Hash_int -position '17:21' -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1753,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:21' -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1752,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:21' -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1751,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/hash.mli < ./irmin/src/irmin/hash.mli","success":true}
  {"sample_id":1750,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1749,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '100:18' -index 0 -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1748,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Type. -position '100:18' -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1747,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Type. -position '100:18' -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1746,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '100:18' -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1745,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '74:45' -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1744,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '91:17' -end '93:17' -filename ./irmin/src/irmin/hash.ml < ./irmin/src/irmin/hash.ml","success":true}
  {"sample_id":1743,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/export_for_backends.ml < ./irmin/src/irmin/export_for_backends.ml","success":true}
  {"sample_id":1742,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:35' -index 0 -filename ./irmin/src/irmin/export_for_backends.ml < ./irmin/src/irmin/export_for_backends.ml","success":true}
  {"sample_id":1741,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Reverse -position '20:35' -filename ./irmin/src/irmin/export_for_backends.ml < ./irmin/src/irmin/export_for_backends.ml","success":true}
  {"sample_id":1740,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Reverse -position '20:35' -filename ./irmin/src/irmin/export_for_backends.ml < ./irmin/src/irmin/export_for_backends.ml","success":true}
  {"sample_id":1739,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:35' -filename ./irmin/src/irmin/export_for_backends.ml < ./irmin/src/irmin/export_for_backends.ml","success":true}
  {"sample_id":1738,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:35' -filename ./irmin/src/irmin/export_for_backends.ml < ./irmin/src/irmin/export_for_backends.ml","success":true}
  {"sample_id":1736,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/dot.mli < ./irmin/src/irmin/dot.mli","success":true}
  {"sample_id":1735,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1734,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '147:60' -index 0 -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1733,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Type.to_ -position '147:60' -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1732,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Type.to_ -position '147:60' -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1731,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '147:60' -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1730,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '85:23' -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1729,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '85:38' -end '85:38' -filename ./irmin/src/irmin/dot.ml < ./irmin/src/irmin/dot.ml","success":true}
  {"sample_id":1728,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1727,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:16' -index 0 -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1726,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix irm -position '18:16' -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1725,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix irm -position '18:16' -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1724,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:16' -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1723,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:16' -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1722,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:12' -end '18:16' -filename ./irmin/src/irmin/diff.mli < ./irmin/src/irmin/diff.mli","success":true}
  {"sample_id":1721,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1720,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:16' -index 0 -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1719,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix irm -position '18:16' -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1718,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix irm -position '18:16' -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1717,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:16' -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1716,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:16' -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1715,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:12' -end '18:16' -filename ./irmin/src/irmin/diff.ml < ./irmin/src/irmin/diff.ml","success":true}
  {"sample_id":1714,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1713,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:51' -index 0 -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1712,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Fixed_size_ -position '21:51' -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1711,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Fixed_size_ -position '21:51' -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1710,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:51' -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1709,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '19:27' -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1708,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '19:27' -filename ./irmin/src/irmin/data/irmin_data.ml < ./irmin/src/irmin/data/irmin_data.ml","success":true}
  {"sample_id":1707,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1706,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:23' -index 0 -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1705,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix el -position '26:23' -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1704,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix el -position '26:23' -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1703,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:23' -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1702,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:70' -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1701,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '22:2' -end '24:26' -filename ./irmin/src/irmin/data/hashset.ml < ./irmin/src/irmin/data/hashset.ml","success":true}
  {"sample_id":1700,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/data/fixed_size_string_set.mli < ./irmin/src/irmin/data/fixed_size_string_set.mli","success":true}
  {"sample_id":1699,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1698,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '46:24' -index 0 -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1697,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '46:24' -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1696,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '46:24' -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1695,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '46:24' -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1694,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '88:34' -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1693,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '79:49' -end '79:60' -filename ./irmin/src/irmin/data/fixed_size_string_set.ml < ./irmin/src/irmin/data/fixed_size_string_set.ml","success":true}
  {"sample_id":1692,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1691,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '78:32' -index 0 -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1690,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '78:32' -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1689,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '78:32' -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1688,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '78:32' -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1687,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '43:60' -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1686,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '70:2' -end '72:66' -filename ./irmin/src/irmin/contents_intf.ml < ./irmin/src/irmin/contents_intf.ml","success":true}
  {"sample_id":1685,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1684,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:25' -index 0 -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1683,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Contents_i -position '19:25' -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1682,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Contents_i -position '19:25' -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1681,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:25' -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1680,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:13' -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1679,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:13' -filename ./irmin/src/irmin/contents.mli < ./irmin/src/irmin/contents.mli","success":true}
  {"sample_id":1678,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1677,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '110:21' -index 0 -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1676,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix List. -position '110:21' -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1675,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix List. -position '110:21' -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1674,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '110:21' -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1673,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '154:17' -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1672,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '142:4' -end '142:6' -filename ./irmin/src/irmin/contents.ml < ./irmin/src/irmin/contents.ml","success":true}
  {"sample_id":1671,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1670,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '43:41' -index 0 -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1669,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Hash -position '43:41' -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1668,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Hash -position '43:41' -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1667,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '43:41' -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1666,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:10' -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1665,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:2' -end '22:50' -filename ./irmin/src/irmin/content_addressable_intf.ml < ./irmin/src/irmin/content_addressable_intf.ml","success":true}
  {"sample_id":1664,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1663,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:36' -index 0 -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1662,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Content_address -position '17:36' -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1661,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Content_address -position '17:36' -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1660,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:36' -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1659,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:36' -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1658,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/content_addressable.mli < ./irmin/src/irmin/content_addressable.mli","success":true}
  {"sample_id":1657,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1656,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:15' -index 0 -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1655,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lwt.I -position '22:15' -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1654,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lwt.I -position '22:15' -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1653,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:15' -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1652,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:11' -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1651,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '27:34' -end '27:38' -filename ./irmin/src/irmin/content_addressable.ml < ./irmin/src/irmin/content_addressable.ml","success":true}
  {"sample_id":1650,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/conf.mli < ./irmin/src/irmin/conf.mli","success":true}
  {"sample_id":1649,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1648,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '103:50' -index 0 -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1647,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix of_u -position '103:50' -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1646,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix of_u -position '103:50' -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1645,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '103:50' -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1644,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '176:25' -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1643,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '159:18' -end '159:19' -filename ./irmin/src/irmin/conf.ml < ./irmin/src/irmin/conf.ml","success":true}
  {"sample_id":1642,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1641,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '160:52' -index 0 -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1640,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix inf -position '160:52' -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1639,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix inf -position '160:52' -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1638,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '160:52' -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1637,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '318:41' -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1636,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '121:2' -end '121:44' -filename ./irmin/src/irmin/commit_intf.ml < ./irmin/src/irmin/commit_intf.ml","success":true}
  {"sample_id":1635,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1634,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:23' -index 0 -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1633,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Commit_in -position '26:23' -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1632,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Commit_in -position '26:23' -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1631,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:23' -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1630,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:48' -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1629,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '24:48' -filename ./irmin/src/irmin/commit.mli < ./irmin/src/irmin/commit.mli","success":true}
  {"sample_id":1628,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1627,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '331:50' -index 0 -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1626,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix k -position '331:50' -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1625,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix k -position '331:50' -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1624,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '331:50' -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1623,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '264:32' -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1622,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '272:26' -end '272:29' -filename ./irmin/src/irmin/commit.ml < ./irmin/src/irmin/commit.ml","success":true}
  {"sample_id":1621,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1620,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '37:15' -index 0 -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1619,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S -position '37:15' -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1618,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S -position '37:15' -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1617,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '37:15' -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1616,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '30:21' -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1615,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '38:2' -end '38:31' -filename ./irmin/src/irmin/branch_intf.ml < ./irmin/src/irmin/branch_intf.ml","success":true}
  {"sample_id":1614,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1613,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:23' -index 0 -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1612,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Branch_in -position '19:23' -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1611,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Branch_in -position '19:23' -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1610,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:23' -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1609,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:28' -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1608,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:28' -filename ./irmin/src/irmin/branch.mli < ./irmin/src/irmin/branch.mli","success":true}
  {"sample_id":1607,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1606,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:20' -index 0 -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1605,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix tru -position '26:20' -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1604,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix tru -position '26:20' -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1603,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:20' -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1602,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '35:6' -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1601,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '26:4' -end '35:6' -filename ./irmin/src/irmin/branch.ml < ./irmin/src/irmin/branch.ml","success":true}
  {"sample_id":1600,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1599,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '36:41' -index 0 -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1598,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Hash -position '36:41' -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1597,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Hash -position '36:41' -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1596,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '36:41' -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1595,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '36:76' -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1594,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '88:4' -end '88:17' -filename ./irmin/src/irmin/backend.ml < ./irmin/src/irmin/backend.ml","success":true}
  {"sample_id":1593,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1592,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '57:4' -index 0 -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1591,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '57:4' -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1590,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '57:4' -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1589,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '57:4' -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1588,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '73:60' -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1587,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '79:2' -end '79:15' -filename ./irmin/src/irmin/atomic_write_intf.ml < ./irmin/src/irmin/atomic_write_intf.ml","success":true}
  {"sample_id":1586,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1585,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:29' -index 0 -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1584,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Atomic_write -position '17:29' -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1583,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Atomic_write -position '17:29' -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1582,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:29' -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1581,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:29' -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1580,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/atomic_write.mli < ./irmin/src/irmin/atomic_write.mli","success":true}
  {"sample_id":1579,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1578,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '47:21' -index 0 -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1577,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix tru -position '47:21' -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1576,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix tru -position '47:21' -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1575,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '47:21' -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1574,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:42' -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1573,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '38:15' -end '38:52' -filename ./irmin/src/irmin/atomic_write.ml < ./irmin/src/irmin/atomic_write.ml","success":true}
  {"sample_id":1572,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1571,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '49:18' -index 0 -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1570,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S -position '49:18' -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1569,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S -position '49:18' -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1568,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '49:18' -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1567,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:58' -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1566,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:2' -end '24:15' -filename ./irmin/src/irmin/append_only_intf.ml < ./irmin/src/irmin/append_only_intf.ml","success":true}
  {"sample_id":1565,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1564,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:28' -index 0 -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1563,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Append_only -position '17:28' -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1562,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Append_only -position '17:28' -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1561,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:28' -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1560,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:28' -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1559,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin/append_only.mli < ./irmin/src/irmin/append_only.mli","success":true}
  {"sample_id":1558,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin/append_only.ml < ./irmin/src/irmin/append_only.ml","success":true}
  {"sample_id":1557,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:23' -index 0 -filename ./irmin/src/irmin/append_only.ml < ./irmin/src/irmin/append_only.ml","success":true}
  {"sample_id":1556,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Append_on -position '17:23' -filename ./irmin/src/irmin/append_only.ml < ./irmin/src/irmin/append_only.ml","success":true}
  {"sample_id":1555,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Append_on -position '17:23' -filename ./irmin/src/irmin/append_only.ml < ./irmin/src/irmin/append_only.ml","success":true}
  {"sample_id":1554,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:23' -filename ./irmin/src/irmin/append_only.ml < ./irmin/src/irmin/append_only.ml","success":true}
  {"sample_id":1553,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:23' -filename ./irmin/src/irmin/append_only.ml < ./irmin/src/irmin/append_only.ml","success":true}
  {"sample_id":1551,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-tezos/schema.mli < ./irmin/src/irmin-tezos/schema.mli","success":true}
  {"sample_id":1550,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-tezos/schema.ml < ./irmin/src/irmin-tezos/schema.ml","success":true}
  {"sample_id":1549,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '122:7' -index 0 -filename ./irmin/src/irmin-tezos/schema.ml < ./irmin/src/irmin-tezos/schema.ml","success":true}
  {"sample_id":1546,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '122:7' -filename ./irmin/src/irmin-tezos/schema.ml < ./irmin/src/irmin-tezos/schema.ml","success":true}
  {"sample_id":1545,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '108:13' -filename ./irmin/src/irmin-tezos/schema.ml < ./irmin/src/irmin-tezos/schema.ml","success":true}
  {"sample_id":1544,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '104:35' -end '104:51' -filename ./irmin/src/irmin-tezos/schema.ml < ./irmin/src/irmin-tezos/schema.ml","success":true}
  {"sample_id":1543,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-tezos/irmin_tezos.mli < ./irmin/src/irmin-tezos/irmin_tezos.mli","success":true}
  {"sample_id":1542,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1541,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '27:41' -index 0 -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1540,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Con -position '27:41' -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1539,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Con -position '27:41' -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1538,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '27:41' -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1537,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '28:33' -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1536,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:26' -end '23:37' -filename ./irmin/src/irmin-tezos/irmin_tezos.ml < ./irmin/src/irmin-tezos/irmin_tezos.ml","success":true}
  {"sample_id":1535,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/store_watch.mli < ./irmin/src/irmin-test/store_watch.mli","success":true}
  {"sample_id":1534,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/store_watch.ml < ./irmin/src/irmin-test/store_watch.ml","success":true}
  {"sample_id":1533,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '284:48' -index 0 -filename ./irmin/src/irmin-test/store_watch.ml < ./irmin/src/irmin-test/store_watch.ml","success":true}
  {"sample_id":1530,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '284:48' -filename ./irmin/src/irmin-test/store_watch.ml < ./irmin/src/irmin-test/store_watch.ml","success":true}
  {"sample_id":1529,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '177:54' -filename ./irmin/src/irmin-test/store_watch.ml < ./irmin/src/irmin-test/store_watch.ml","success":true}
  {"sample_id":1528,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '173:8' -end '173:36' -filename ./irmin/src/irmin-test/store_watch.ml < ./irmin/src/irmin-test/store_watch.ml","success":true}
  {"sample_id":1527,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/store_graph.mli < ./irmin/src/irmin-test/store_graph.mli","success":true}
  {"sample_id":1526,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/store_graph.ml < ./irmin/src/irmin-test/store_graph.ml","success":true}
  {"sample_id":1525,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '128:17' -index 0 -filename ./irmin/src/irmin-test/store_graph.ml < ./irmin/src/irmin-test/store_graph.ml","success":true}
  {"sample_id":1522,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '128:17' -filename ./irmin/src/irmin-test/store_graph.ml < ./irmin/src/irmin-test/store_graph.ml","success":true}
  {"sample_id":1521,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '93:51' -filename ./irmin/src/irmin-test/store_graph.ml < ./irmin/src/irmin-test/store_graph.ml","success":true}
  {"sample_id":1520,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '92:10' -end '94:59' -filename ./irmin/src/irmin-test/store_graph.ml < ./irmin/src/irmin-test/store_graph.ml","success":true}
  {"sample_id":1519,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/store.mli < ./irmin/src/irmin-test/store.mli","success":true}
  {"sample_id":1518,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:11' -index 0 -filename ./irmin/src/irmin-test/store.mli < ./irmin/src/irmin-test/store.mli","success":true}
  {"sample_id":1517,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lwt -position '24:11' -filename ./irmin/src/irmin-test/store.mli < ./irmin/src/irmin-test/store.mli","success":true}
  {"sample_id":1516,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lwt -position '24:11' -filename ./irmin/src/irmin-test/store.mli < ./irmin/src/irmin-test/store.mli","success":true}
  {"sample_id":1515,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:11' -filename ./irmin/src/irmin-test/store.mli < ./irmin/src/irmin-test/store.mli","success":true}
  {"sample_id":1512,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1511,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1385:30' -index 0 -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1510,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '1385:30' -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1509,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '1385:30' -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1508,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1385:30' -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1507,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '784:56' -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1506,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '736:44' -end '736:55' -filename ./irmin/src/irmin-test/store.ml < ./irmin/src/irmin-test/store.ml","success":true}
  {"sample_id":1505,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/rusage.ml < ./irmin/src/irmin-test/rusage.ml","success":true}
  {"sample_id":1504,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:15' -index 0 -filename ./irmin/src/irmin-test/rusage.ml < ./irmin/src/irmin-test/rusage.ml","success":true}
  {"sample_id":1503,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix int -position '25:15' -filename ./irmin/src/irmin-test/rusage.ml < ./irmin/src/irmin-test/rusage.ml","success":true}
  {"sample_id":1502,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix int -position '25:15' -filename ./irmin/src/irmin-test/rusage.ml < ./irmin/src/irmin-test/rusage.ml","success":true}
  {"sample_id":1501,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:15' -filename ./irmin/src/irmin-test/rusage.ml < ./irmin/src/irmin-test/rusage.ml","success":true}
  {"sample_id":1498,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1497,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '52:39' -index 0 -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1496,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Map.d -position '52:39' -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1495,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Map.d -position '52:39' -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1494,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '52:39' -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1493,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '51:64' -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1492,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '51:10' -end '51:16' -filename ./irmin/src/irmin-test/node.ml < ./irmin/src/irmin-test/node.ml","success":true}
  {"sample_id":1491,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/irmin_test.mli < ./irmin/src/irmin-test/irmin_test.mli","success":true}
  {"sample_id":1490,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/irmin_test.ml < ./irmin/src/irmin-test/irmin_test.ml","success":true}
  {"sample_id":1489,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:21' -index 0 -filename ./irmin/src/irmin-test/irmin_test.ml < ./irmin/src/irmin-test/irmin_test.ml","success":true}
  {"sample_id":1488,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Comm -position '19:21' -filename ./irmin/src/irmin-test/irmin_test.ml < ./irmin/src/irmin-test/irmin_test.ml","success":true}
  {"sample_id":1487,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Comm -position '19:21' -filename ./irmin/src/irmin-test/irmin_test.ml < ./irmin/src/irmin-test/irmin_test.ml","success":true}
  {"sample_id":1486,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:21' -filename ./irmin/src/irmin-test/irmin_test.ml < ./irmin/src/irmin-test/irmin_test.ml","success":true}
  {"sample_id":1485,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '19:21' -filename ./irmin/src/irmin-test/irmin_test.ml < ./irmin/src/irmin-test/irmin_test.ml","success":true}
  {"sample_id":1483,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/irmin_bench.mli < ./irmin/src/irmin-test/irmin_bench.mli","success":true}
  {"sample_id":1482,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1481,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '153:37' -index 0 -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1480,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix tree_ -position '153:37' -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1479,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix tree_ -position '153:37' -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1478,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '153:37' -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1477,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '113:18' -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1476,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '109:21' -end '109:21' -filename ./irmin/src/irmin-test/irmin_bench.ml < ./irmin/src/irmin-test/irmin_bench.ml","success":true}
  {"sample_id":1475,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/import.ml < ./irmin/src/irmin-test/import.ml","success":true}
  {"sample_id":1474,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-test/import.ml < ./irmin/src/irmin-test/import.ml","success":true}
  {"sample_id":1473,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-test/import.ml < ./irmin/src/irmin-test/import.ml","success":true}
  {"sample_id":1472,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-test/import.ml < ./irmin/src/irmin-test/import.ml","success":true}
  {"sample_id":1471,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-test/import.ml < ./irmin/src/irmin-test/import.ml","success":true}
  {"sample_id":1470,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-test/import.ml < ./irmin/src/irmin-test/import.ml","success":true}
  {"sample_id":1468,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1467,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:35' -index 0 -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1466,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Common.r -position '19:35' -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1465,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Common.r -position '19:35' -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1464,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:35' -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1463,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '19:18' -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1462,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:2' -end '19:18' -filename ./irmin/src/irmin-test/helpers.ml < ./irmin/src/irmin-test/helpers.ml","success":true}
  {"sample_id":1461,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1460,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '192:75' -index 0 -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1459,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix kn -position '192:75' -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1458,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix kn -position '192:75' -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1457,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '192:75' -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1456,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '292:8' -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1455,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '274:21' -end '276:23' -filename ./irmin/src/irmin-test/common.ml < ./irmin/src/irmin-test/common.ml","success":true}
  {"sample_id":1454,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1453,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '48:18' -index 0 -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1452,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix stri -position '48:18' -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1451,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix stri -position '48:18' -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1450,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '48:18' -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1449,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '52:50' -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1448,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '52:0' -end '52:50' -filename ./irmin/src/irmin-pack/version.mli < ./irmin/src/irmin-pack/version.mli","success":true}
  {"sample_id":1447,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1446,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '70:68' -index 0 -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1445,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Non -position '70:68' -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1444,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Non -position '70:68' -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1443,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '70:68' -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1442,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '44:33' -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1441,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '44:21' -end '44:33' -filename ./irmin/src/irmin-pack/version.ml < ./irmin/src/irmin-pack/version.ml","success":true}
  {"sample_id":1440,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1439,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '59:19' -index 0 -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1438,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Object_c -position '59:19' -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1437,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Object_c -position '59:19' -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1436,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '59:19' -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1435,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '60:33' -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1434,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '51:4' -end '60:33' -filename ./irmin/src/irmin-pack/unix/utils.ml < ./irmin/src/irmin-pack/unix/utils.ml","success":true}
  {"sample_id":1433,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1432,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '296:54' -index 0 -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1431,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix missing -position '296:54' -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1430,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix missing -position '296:54' -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1429,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '296:54' -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1428,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '218:54' -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1427,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '216:28' -end '216:37' -filename ./irmin/src/irmin-pack/unix/traverse_pack_file.ml < ./irmin/src/irmin-pack/unix/traverse_pack_file.ml","success":true}
  {"sample_id":1426,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1425,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '350:39' -index 0 -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1424,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Schema. -position '350:39' -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1423,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Schema. -position '350:39' -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1422,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '350:39' -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1421,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '217:21' -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1420,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '230:16' -end '230:20' -filename ./irmin/src/irmin-pack/unix/store_intf.ml < ./irmin/src/irmin-pack/unix/store_intf.ml","success":true}
  {"sample_id":1419,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/store.mli < ./irmin/src/irmin-pack/unix/store.mli","success":true}
  {"sample_id":1418,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1417,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '127:44' -index 0 -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1416,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Backend -position '127:44' -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1415,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Backend -position '127:44' -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1414,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '127:44' -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1413,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '701:44' -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1412,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '732:10' -end '734:29' -filename ./irmin/src/irmin-pack/unix/store.ml < ./irmin/src/irmin-pack/unix/store.ml","success":true}
  {"sample_id":1411,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1410,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '228:41' -index 0 -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1409,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix flo -position '228:41' -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1408,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix flo -position '228:41' -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1407,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '228:41' -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1406,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '124:42' -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1405,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '182:2' -end '183:34' -filename ./irmin/src/irmin-pack/unix/stats_intf.ml < ./irmin/src/irmin-pack/unix/stats_intf.ml","success":true}
  {"sample_id":1404,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1403,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1402,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Stats_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1401,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Stats_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1400,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1399,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1398,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/stats.mli < ./irmin/src/irmin-pack/unix/stats.mli","success":true}
  {"sample_id":1397,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1396,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '64:38' -index 0 -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1395,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix from_ -position '64:38' -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1394,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix from_ -position '64:38' -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1393,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '64:38' -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1392,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '248:53' -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1391,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '237:16' -end '237:16' -filename ./irmin/src/irmin-pack/unix/stats.ml < ./irmin/src/irmin-pack/unix/stats.ml","success":true}
  {"sample_id":1390,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1389,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '55:64' -index 0 -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1388,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Errs -position '55:64' -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1387,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Errs -position '55:64' -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1386,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '55:64' -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1385,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '150:4' -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1384,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '146:4' -end '146:57' -filename ./irmin/src/irmin-pack/unix/sparse_file_intf.ml < ./irmin/src/irmin-pack/unix/sparse_file_intf.ml","success":true}
  {"sample_id":1383,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1382,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:28' -index 0 -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1381,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Sparse_file -position '17:28' -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1380,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Sparse_file -position '17:28' -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1379,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:28' -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1378,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:28' -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1377,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/sparse_file.mli < ./irmin/src/irmin-pack/unix/sparse_file.mli","success":true}
  {"sample_id":1376,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1375,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '104:29' -index 0 -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1374,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix entry_ -position '104:29' -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1373,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix entry_ -position '104:29' -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1372,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '104:29' -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1371,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '78:33' -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1370,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '84:23' -end '94:51' -filename ./irmin/src/irmin-pack/unix/sparse_file.ml < ./irmin/src/irmin-pack/unix/sparse_file.ml","success":true}
  {"sample_id":1369,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/snapshot_intf.ml < ./irmin/src/irmin-pack/unix/snapshot_intf.ml","success":true}
  {"sample_id":1368,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '77:48' -index 0 -filename ./irmin/src/irmin-pack/unix/snapshot_intf.ml < ./irmin/src/irmin-pack/unix/snapshot_intf.ml","success":true}
  {"sample_id":1367,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Inode.Snaps -position '77:48' -filename ./irmin/src/irmin-pack/unix/snapshot_intf.ml < ./irmin/src/irmin-pack/unix/snapshot_intf.ml","success":true}
  {"sample_id":1366,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Inode.Snaps -position '77:48' -filename ./irmin/src/irmin-pack/unix/snapshot_intf.ml < ./irmin/src/irmin-pack/unix/snapshot_intf.ml","success":true}
  {"sample_id":1365,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '77:48' -filename ./irmin/src/irmin-pack/unix/snapshot_intf.ml < ./irmin/src/irmin-pack/unix/snapshot_intf.ml","success":true}
  {"sample_id":1364,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:53' -filename ./irmin/src/irmin-pack/unix/snapshot_intf.ml < ./irmin/src/irmin-pack/unix/snapshot_intf.ml","success":true}
  {"sample_id":1362,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1361,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:25' -index 0 -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1360,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Snapshot_i -position '17:25' -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1359,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Snapshot_i -position '17:25' -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1358,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:25' -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1357,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:25' -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1356,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/snapshot.mli < ./irmin/src/irmin-pack/unix/snapshot.mli","success":true}
  {"sample_id":1355,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1354,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '206:31' -index 0 -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1353,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ind -position '206:31' -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1352,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ind -position '206:31' -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1351,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '206:31' -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1350,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '260:42' -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1349,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '240:18' -end '240:61' -filename ./irmin/src/irmin-pack/unix/snapshot.ml < ./irmin/src/irmin-pack/unix/snapshot.ml","success":true}
  {"sample_id":1348,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1347,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:42' -index 0 -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1346,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix uni -position '25:42' -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1345,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix uni -position '25:42' -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1344,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:42' -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1343,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '35:42' -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1342,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '38:0' -end '38:62' -filename ./irmin/src/irmin-pack/unix/ranges.mli < ./irmin/src/irmin-pack/unix/ranges.mli","success":true}
  {"sample_id":1341,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1340,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '41:10' -index 0 -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1339,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Sta -position '41:10' -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1338,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Sta -position '41:10' -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1337,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '41:10' -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1336,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '110:45' -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1335,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '107:16' -end '107:18' -filename ./irmin/src/irmin-pack/unix/ranges.ml < ./irmin/src/irmin-pack/unix/ranges.ml","success":true}
  {"sample_id":1334,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_value.ml < ./irmin/src/irmin-pack/unix/pack_value.ml","success":true}
  {"sample_id":1333,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:53' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_value.ml < ./irmin/src/irmin-pack/unix/pack_value.ml","success":true}
  {"sample_id":1332,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix has -position '22:53' -filename ./irmin/src/irmin-pack/unix/pack_value.ml < ./irmin/src/irmin-pack/unix/pack_value.ml","success":true}
  {"sample_id":1331,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix has -position '22:53' -filename ./irmin/src/irmin-pack/unix/pack_value.ml < ./irmin/src/irmin-pack/unix/pack_value.ml","success":true}
  {"sample_id":1330,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:53' -filename ./irmin/src/irmin-pack/unix/pack_value.ml < ./irmin/src/irmin-pack/unix/pack_value.ml","success":true}
  {"sample_id":1329,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:28' -filename ./irmin/src/irmin-pack/unix/pack_value.ml < ./irmin/src/irmin-pack/unix/pack_value.ml","success":true}
  {"sample_id":1327,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1326,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '98:48' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1325,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Pack_k -position '98:48' -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1324,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Pack_k -position '98:48' -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1323,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '98:48' -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1322,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '106:28' -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1321,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '68:2' -end '68:51' -filename ./irmin/src/irmin-pack/unix/pack_store_intf.ml < ./irmin/src/irmin-pack/unix/pack_store_intf.ml","success":true}
  {"sample_id":1320,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1319,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:27' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1318,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Pack_store_ -position '17:27' -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1317,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Pack_store_ -position '17:27' -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1316,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:27' -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1315,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:27' -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1314,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/pack_store.mli < ./irmin/src/irmin-pack/unix/pack_store.mli","success":true}
  {"sample_id":1313,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1312,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '489:27' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1311,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '489:27' -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1310,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '489:27' -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1309,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '489:27' -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1308,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '361:51' -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1307,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '334:41' -end '334:41' -filename ./irmin/src/irmin-pack/unix/pack_store.ml < ./irmin/src/irmin-pack/unix/pack_store.ml","success":true}
  {"sample_id":1306,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1305,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '38:22' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1304,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix saf -position '38:22' -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1303,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix saf -position '38:22' -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1302,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '38:22' -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1301,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '30:81' -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1300,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '115:4' -end '115:17' -filename ./irmin/src/irmin-pack/unix/pack_key_intf.ml < ./irmin/src/irmin-pack/unix/pack_key_intf.ml","success":true}
  {"sample_id":1299,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1298,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:25' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1297,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Pack_key_i -position '17:25' -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1296,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Pack_key_i -position '17:25' -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1295,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:25' -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1294,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:25' -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1293,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/pack_key.mli < ./irmin/src/irmin-pack/unix/pack_key.mli","success":true}
  {"sample_id":1292,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1291,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '95:38' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1290,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin. -position '95:38' -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1289,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin. -position '95:38' -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1288,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '95:38' -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1287,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '96:69' -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1286,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '87:34' -end '87:73' -filename ./irmin/src/irmin-pack/unix/pack_key.ml < ./irmin/src/irmin-pack/unix/pack_key.ml","success":true}
  {"sample_id":1285,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1284,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '74:30' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1283,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.H -position '74:30' -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1282,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.H -position '74:30' -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1281,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '74:30' -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1280,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '72:18' -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1279,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:2' -end '21:37' -filename ./irmin/src/irmin-pack/unix/pack_index_intf.ml < ./irmin/src/irmin-pack/unix/pack_index_intf.ml","success":true}
  {"sample_id":1278,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1277,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:27' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1276,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Pack_index_ -position '17:27' -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1275,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Pack_index_ -position '17:27' -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1274,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:27' -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1273,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:27' -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1272,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/pack_index.mli < ./irmin/src/irmin-pack/unix/pack_index.mli","success":true}
  {"sample_id":1271,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1270,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '26:35' -index 0 -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1269,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Non -position '26:35' -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1268,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Non -position '26:35' -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1267,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '26:35' -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1266,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:66' -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1265,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '28:17' -end '28:29' -filename ./irmin/src/irmin-pack/unix/pack_index.ml < ./irmin/src/irmin-pack/unix/pack_index.ml","success":true}
  {"sample_id":1264,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1263,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '125:17' -index 0 -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1262,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix in -position '125:17' -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1261,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix in -position '125:17' -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1260,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '125:17' -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1259,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '36:57' -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1258,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '59:64' -end '59:68' -filename ./irmin/src/irmin-pack/unix/lower_intf.ml < ./irmin/src/irmin-pack/unix/lower_intf.ml","success":true}
  {"sample_id":1257,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1256,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1255,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lower_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1254,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lower_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1253,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1252,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1251,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/lower.mli < ./irmin/src/irmin-pack/unix/lower.mli","success":true}
  {"sample_id":1250,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/lower.ml < ./irmin/src/irmin-pack/unix/lower.ml","success":true}
  {"sample_id":1249,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '368:12' -index 0 -filename ./irmin/src/irmin-pack/unix/lower.ml < ./irmin/src/irmin-pack/unix/lower.ml","success":true}
  {"sample_id":1246,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '368:12' -filename ./irmin/src/irmin-pack/unix/lower.ml < ./irmin/src/irmin-pack/unix/lower.ml","success":true}
  {"sample_id":1245,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '301:23' -filename ./irmin/src/irmin-pack/unix/lower.ml < ./irmin/src/irmin-pack/unix/lower.ml","success":true}
  {"sample_id":1244,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '279:4' -end '284:28' -filename ./irmin/src/irmin-pack/unix/lower.ml < ./irmin/src/irmin-pack/unix/lower.ml","success":true}
  {"sample_id":1243,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.mli < ./irmin/src/irmin-pack/unix/irmin_pack_unix.mli","success":true}
  {"sample_id":1242,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1241,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '45:29' -index 0 -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1240,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Pack_s -position '45:29' -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1239,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Pack_s -position '45:29' -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1238,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '45:29' -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1237,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '40:18' -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1236,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '40:0' -end '40:18' -filename ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml < ./irmin/src/irmin-pack/unix/irmin_pack_unix.ml","success":true}
  {"sample_id":1235,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io_legacy_intf.ml < ./irmin/src/irmin-pack/unix/io_legacy_intf.ml","success":true}
  {"sample_id":1234,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '35:13' -index 0 -filename ./irmin/src/irmin-pack/unix/io_legacy_intf.ml < ./irmin/src/irmin-pack/unix/io_legacy_intf.ml","success":true}
  {"sample_id":1233,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '35:13' -filename ./irmin/src/irmin-pack/unix/io_legacy_intf.ml < ./irmin/src/irmin-pack/unix/io_legacy_intf.ml","success":true}
  {"sample_id":1232,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '35:13' -filename ./irmin/src/irmin-pack/unix/io_legacy_intf.ml < ./irmin/src/irmin-pack/unix/io_legacy_intf.ml","success":true}
  {"sample_id":1231,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '35:13' -filename ./irmin/src/irmin-pack/unix/io_legacy_intf.ml < ./irmin/src/irmin-pack/unix/io_legacy_intf.ml","success":true}
  {"sample_id":1230,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '48:2' -filename ./irmin/src/irmin-pack/unix/io_legacy_intf.ml < ./irmin/src/irmin-pack/unix/io_legacy_intf.ml","success":true}
  {"sample_id":1228,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io_legacy.mli < ./irmin/src/irmin-pack/unix/io_legacy.mli","success":true}
  {"sample_id":1227,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:26' -index 0 -filename ./irmin/src/irmin-pack/unix/io_legacy.mli < ./irmin/src/irmin-pack/unix/io_legacy.mli","success":true}
  {"sample_id":1226,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Io_legacy_ -position '17:26' -filename ./irmin/src/irmin-pack/unix/io_legacy.mli < ./irmin/src/irmin-pack/unix/io_legacy.mli","success":true}
  {"sample_id":1225,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Io_legacy_ -position '17:26' -filename ./irmin/src/irmin-pack/unix/io_legacy.mli < ./irmin/src/irmin-pack/unix/io_legacy.mli","success":true}
  {"sample_id":1224,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:26' -filename ./irmin/src/irmin-pack/unix/io_legacy.mli < ./irmin/src/irmin-pack/unix/io_legacy.mli","success":true}
  {"sample_id":1223,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:26' -filename ./irmin/src/irmin-pack/unix/io_legacy.mli < ./irmin/src/irmin-pack/unix/io_legacy.mli","success":true}
  {"sample_id":1221,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1220,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '90:20' -index 0 -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1219,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '90:20' -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1218,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '90:20' -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1217,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '90:20' -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1216,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '77:22' -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1215,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '68:17' -end '68:19' -filename ./irmin/src/irmin-pack/unix/io_legacy.ml < ./irmin/src/irmin-pack/unix/io_legacy.ml","success":true}
  {"sample_id":1214,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1213,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '87:64' -index 0 -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1212,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix resu -position '87:64' -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1211,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix resu -position '87:64' -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1210,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '87:64' -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1209,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '93:63' -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1208,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '99:2' -end '99:73' -filename ./irmin/src/irmin-pack/unix/io_intf.ml < ./irmin/src/irmin-pack/unix/io_intf.ml","success":true}
  {"sample_id":1207,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1206,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '104:67' -index 0 -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1205,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix e -position '104:67' -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1204,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix e -position '104:67' -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1203,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '104:67' -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1202,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '104:67' -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1201,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '104:55' -end '104:65' -filename ./irmin/src/irmin-pack/unix/io_errors.ml < ./irmin/src/irmin-pack/unix/io_errors.ml","success":true}
  {"sample_id":1200,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1199,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:19' -index 0 -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1198,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Io_intf -position '17:19' -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1197,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Io_intf -position '17:19' -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1196,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:19' -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1195,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:19' -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1194,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/io.mli < ./irmin/src/irmin-pack/unix/io.mli","success":true}
  {"sample_id":1193,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1192,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '239:28' -index 0 -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1191,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix bu -position '239:28' -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1190,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix bu -position '239:28' -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1189,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '239:28' -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1188,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '235:66' -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1187,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '217:43' -end '217:64' -filename ./irmin/src/irmin-pack/unix/io.ml < ./irmin/src/irmin-pack/unix/io.ml","success":true}
  {"sample_id":1186,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/inode_intf.ml < ./irmin/src/irmin-pack/unix/inode_intf.ml","success":true}
  {"sample_id":1185,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '77:36' -index 0 -filename ./irmin/src/irmin-pack/unix/inode_intf.ml < ./irmin/src/irmin-pack/unix/inode_intf.ml","success":true}
  {"sample_id":1184,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix H. -position '77:36' -filename ./irmin/src/irmin-pack/unix/inode_intf.ml < ./irmin/src/irmin-pack/unix/inode_intf.ml","success":true}
  {"sample_id":1183,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix H. -position '77:36' -filename ./irmin/src/irmin-pack/unix/inode_intf.ml < ./irmin/src/irmin-pack/unix/inode_intf.ml","success":true}
  {"sample_id":1182,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '77:36' -filename ./irmin/src/irmin-pack/unix/inode_intf.ml < ./irmin/src/irmin-pack/unix/inode_intf.ml","success":true}
  {"sample_id":1181,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '94:2' -filename ./irmin/src/irmin-pack/unix/inode_intf.ml < ./irmin/src/irmin-pack/unix/inode_intf.ml","success":true}
  {"sample_id":1179,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1178,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1177,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Inode_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1176,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Inode_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1175,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1174,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1173,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/inode.mli < ./irmin/src/irmin-pack/unix/inode.mli","success":true}
  {"sample_id":1172,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1171,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '46:16' -index 0 -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1170,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix H -position '46:16' -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1169,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix H -position '46:16' -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1168,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '46:16' -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1167,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '54:24' -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1166,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '65:17' -end '67:52' -filename ./irmin/src/irmin-pack/unix/inode.ml < ./irmin/src/irmin-pack/unix/inode.ml","success":true}
  {"sample_id":1165,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1164,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '46:21' -index 0 -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1163,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Optint. -position '46:21' -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1162,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Optint. -position '46:21' -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1161,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '46:21' -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1160,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:25' -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1159,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '32:25' -end '32:25' -filename ./irmin/src/irmin-pack/unix/import.ml < ./irmin/src/irmin-pack/unix/import.ml","success":true}
  {"sample_id":1158,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc_worker.mli < ./irmin/src/irmin-pack/unix/gc_worker.mli","success":true}
  {"sample_id":1157,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1156,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '275:31' -index 0 -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1155,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Errs.log_ -position '275:31' -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1154,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Errs.log_ -position '275:31' -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1153,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '275:31' -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1152,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '186:8' -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1151,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '180:19' -end '180:29' -filename ./irmin/src/irmin-pack/unix/gc_worker.ml < ./irmin/src/irmin-pack/unix/gc_worker.ml","success":true}
  {"sample_id":1150,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc_stats.mli < ./irmin/src/irmin-pack/unix/gc_stats.mli","success":true}
  {"sample_id":1149,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1148,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '198:44' -index 0 -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1147,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix cou -position '198:44' -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1146,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix cou -position '198:44' -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1145,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '198:44' -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1144,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '167:33' -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1143,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '164:16' -end '164:24' -filename ./irmin/src/irmin-pack/unix/gc_stats.ml < ./irmin/src/irmin-pack/unix/gc_stats.ml","success":true}
  {"sample_id":1142,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1141,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '45:17' -index 0 -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1140,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix opti -position '45:17' -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1139,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix opti -position '45:17' -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1138,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '45:17' -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1137,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '31:4' -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1136,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '27:41' -end '27:45' -filename ./irmin/src/irmin-pack/unix/gc_args.ml < ./irmin/src/irmin-pack/unix/gc_args.ml","success":true}
  {"sample_id":1135,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc.mli < ./irmin/src/irmin-pack/unix/gc.mli","success":true}
  {"sample_id":1134,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1133,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '120:38' -index 0 -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1132,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Gc_stats.Main.fin -position '120:38' -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1131,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Gc_stats.Main.fin -position '120:38' -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1130,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '120:38' -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1129,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '96:71' -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1128,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '96:6' -end '106:61' -filename ./irmin/src/irmin-pack/unix/gc.ml < ./irmin/src/irmin-pack/unix/gc.ml","success":true}
  {"sample_id":1127,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1126,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '309:15' -index 0 -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1125,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Io. -position '309:15' -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1124,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Io. -position '309:15' -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1123,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '309:15' -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1122,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '306:18' -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1121,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '287:2' -end '288:38' -filename ./irmin/src/irmin-pack/unix/file_manager_intf.ml < ./irmin/src/irmin-pack/unix/file_manager_intf.ml","success":true}
  {"sample_id":1120,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1119,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:29' -index 0 -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1118,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix File_manager -position '17:29' -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1117,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix File_manager -position '17:29' -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1116,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:29' -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1115,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:29' -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1114,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/file_manager.mli < ./irmin/src/irmin-pack/unix/file_manager.mli","success":true}
  {"sample_id":1113,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1112,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '936:42' -index 0 -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1111,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix suff -position '936:42' -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1110,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix suff -position '936:42' -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1109,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '936:42' -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1108,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '1034:28' -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1107,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '979:43' -end '979:43' -filename ./irmin/src/irmin-pack/unix/file_manager.ml < ./irmin/src/irmin-pack/unix/file_manager.ml","success":true}
  {"sample_id":1106,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/errors.ml < ./irmin/src/irmin-pack/unix/errors.ml","success":true}
  {"sample_id":1105,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '128:15' -index 0 -filename ./irmin/src/irmin-pack/unix/errors.ml < ./irmin/src/irmin-pack/unix/errors.ml","success":true}
  {"sample_id":1102,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '128:15' -filename ./irmin/src/irmin-pack/unix/errors.ml < ./irmin/src/irmin-pack/unix/errors.ml","success":true}
  {"sample_id":1101,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '137:33' -filename ./irmin/src/irmin-pack/unix/errors.ml < ./irmin/src/irmin-pack/unix/errors.ml","success":true}
  {"sample_id":1100,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '169:11' -end '169:14' -filename ./irmin/src/irmin-pack/unix/errors.ml < ./irmin/src/irmin-pack/unix/errors.ml","success":true}
  {"sample_id":1099,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1098,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '32:33' -index 0 -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1097,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix opti -position '32:33' -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1096,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix opti -position '32:33' -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1095,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '32:33' -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1094,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '105:2' -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1093,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '76:2' -end '92:62' -filename ./irmin/src/irmin-pack/unix/dispatcher_intf.ml < ./irmin/src/irmin-pack/unix/dispatcher_intf.ml","success":true}
  {"sample_id":1092,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1091,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:27' -index 0 -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1090,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Dispatcher_ -position '17:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1089,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Dispatcher_ -position '17:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1088,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1087,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1086,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/dispatcher.mli < ./irmin/src/irmin-pack/unix/dispatcher.mli","success":true}
  {"sample_id":1085,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1084,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '101:27' -index 0 -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1083,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Errors.Pa -position '101:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1082,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Errors.Pa -position '101:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1081,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '101:27' -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1080,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '113:38' -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1079,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '91:6' -end '91:53' -filename ./irmin/src/irmin-pack/unix/dispatcher.ml < ./irmin/src/irmin-pack/unix/dispatcher.ml","success":true}
  {"sample_id":1078,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/dict_intf.ml < ./irmin/src/irmin-pack/unix/dict_intf.ml","success":true}
  {"sample_id":1077,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:50' -index 0 -filename ./irmin/src/irmin-pack/unix/dict_intf.ml < ./irmin/src/irmin-pack/unix/dict_intf.ml","success":true}
  {"sample_id":1076,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix resu -position '24:50' -filename ./irmin/src/irmin-pack/unix/dict_intf.ml < ./irmin/src/irmin-pack/unix/dict_intf.ml","success":true}
  {"sample_id":1075,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix resu -position '24:50' -filename ./irmin/src/irmin-pack/unix/dict_intf.ml < ./irmin/src/irmin-pack/unix/dict_intf.ml","success":true}
  {"sample_id":1074,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:50' -filename ./irmin/src/irmin-pack/unix/dict_intf.ml < ./irmin/src/irmin-pack/unix/dict_intf.ml","success":true}
  {"sample_id":1073,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '31:58' -filename ./irmin/src/irmin-pack/unix/dict_intf.ml < ./irmin/src/irmin-pack/unix/dict_intf.ml","success":true}
  {"sample_id":1071,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1070,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:21' -index 0 -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1069,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Dict_int -position '17:21' -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1068,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Dict_int -position '17:21' -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1067,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:21' -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1066,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:21' -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1065,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/dict.mli < ./irmin/src/irmin-pack/unix/dict.mli","success":true}
  {"sample_id":1064,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1063,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '90:41' -index 0 -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1062,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ind -position '90:41' -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1061,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ind -position '90:41' -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1060,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '90:41' -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1059,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '63:20' -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1058,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '63:4' -end '63:20' -filename ./irmin/src/irmin-pack/unix/dict.ml < ./irmin/src/irmin-pack/unix/dict.ml","success":true}
  {"sample_id":1057,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1056,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '315:38' -index 0 -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1055,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix stri -position '315:38' -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1054,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix stri -position '315:38' -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1053,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '315:38' -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1052,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '298:47' -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1051,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '225:18' -end '225:22' -filename ./irmin/src/irmin-pack/unix/control_file_intf.ml < ./irmin/src/irmin-pack/unix/control_file_intf.ml","success":true}
  {"sample_id":1050,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1049,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:29' -index 0 -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1048,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Control_file -position '17:29' -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1047,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Control_file -position '17:29' -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1046,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:29' -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1045,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:29' -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1044,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/control_file.mli < ./irmin/src/irmin-pack/unix/control_file.mli","success":true}
  {"sample_id":1043,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1042,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '87:63' -index 0 -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1041,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '87:63' -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1040,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '87:63' -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1039,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '87:63' -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1038,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '66:25' -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1037,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '65:9' -end '65:12' -filename ./irmin/src/irmin-pack/unix/control_file.ml < ./irmin/src/irmin-pack/unix/control_file.ml","success":true}
  {"sample_id":1036,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1035,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '79:36' -index 0 -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1034,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix resu -position '79:36' -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1033,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix resu -position '79:36' -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1032,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '79:36' -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1031,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '140:47' -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1030,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:2' -end '26:49' -filename ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml < ./irmin/src/irmin-pack/unix/chunked_suffix_intf.ml","success":true}
  {"sample_id":1029,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1028,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:31' -index 0 -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1027,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Chunked_suffi -position '17:31' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1026,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Chunked_suffi -position '17:31' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1025,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:31' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1024,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:31' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1023,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.mli < ./irmin/src/irmin-pack/unix/chunked_suffix.mli","success":true}
  {"sample_id":1022,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1021,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '47:30' -index 0 -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1020,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix chu -position '47:30' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1019,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix chu -position '47:30' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1018,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '47:30' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1017,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '162:37' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1016,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '358:6' -end '358:8' -filename ./irmin/src/irmin-pack/unix/chunked_suffix.ml < ./irmin/src/irmin-pack/unix/chunked_suffix.ml","success":true}
  {"sample_id":1015,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1014,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '215:19' -index 0 -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1013,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix opti -position '215:19' -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1012,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix opti -position '215:19' -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1011,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '215:19' -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1010,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:17' -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1009,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '57:2' -end '57:51' -filename ./irmin/src/irmin-pack/unix/checks_intf.ml < ./irmin/src/irmin-pack/unix/checks_intf.ml","success":true}
  {"sample_id":1008,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1007,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:23' -index 0 -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1006,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Checks_in -position '19:23' -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1005,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Checks_in -position '19:23' -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1004,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:23' -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1003,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:13' -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1002,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:37' -filename ./irmin/src/irmin-pack/unix/checks.mli < ./irmin/src/irmin-pack/unix/checks.mli","success":true}
  {"sample_id":1001,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":1000,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '216:65' -index 0 -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":999,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix hea -position '216:65' -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":998,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix hea -position '216:65' -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":997,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '216:65' -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":996,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '141:79' -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":995,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '138:17' -end '138:41' -filename ./irmin/src/irmin-pack/unix/checks.ml < ./irmin/src/irmin-pack/unix/checks.ml","success":true}
  {"sample_id":994,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/atomic_write.mli < ./irmin/src/irmin-pack/unix/atomic_write.mli","success":true}
  {"sample_id":993,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":992,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '52:14' -index 0 -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":991,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix of -position '52:14' -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":990,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix of -position '52:14' -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":989,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '52:14' -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":988,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '35:22' -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":987,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '43:4' -end '43:17' -filename ./irmin/src/irmin-pack/unix/atomic_write.ml < ./irmin/src/irmin-pack/unix/atomic_write.ml","success":true}
  {"sample_id":986,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":985,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '41:23' -index 0 -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":984,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix boo -position '41:23' -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":983,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix boo -position '41:23' -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":982,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '41:23' -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":981,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:18' -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":980,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '26:51' -end '26:55' -filename ./irmin/src/irmin-pack/unix/async_intf.ml < ./irmin/src/irmin-pack/unix/async_intf.ml","success":true}
  {"sample_id":979,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":978,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":977,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Async_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":976,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Async_in -position '17:22' -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":975,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":974,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":973,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/async.mli < ./irmin/src/irmin-pack/unix/async.mli","success":true}
  {"sample_id":972,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":971,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '80:21' -index 0 -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":970,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lwt_unix. -position '80:21' -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":969,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lwt_unix. -position '80:21' -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":968,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '80:21' -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":967,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '94:26' -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":966,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '91:11' -end '91:17' -filename ./irmin/src/irmin-pack/unix/async.ml < ./irmin/src/irmin-pack/unix/async.ml","success":true}
  {"sample_id":965,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":964,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '144:29' -index 0 -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":963,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix stri -position '144:29' -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":962,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix stri -position '144:29' -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":961,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '144:29' -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":960,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '40:39' -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":959,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '107:2' -end '110:48' -filename ./irmin/src/irmin-pack/unix/append_only_file_intf.ml < ./irmin/src/irmin-pack/unix/append_only_file_intf.ml","success":true}
  {"sample_id":958,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":957,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:33' -index 0 -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":956,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Append_only_fi -position '17:33' -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":955,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Append_only_fi -position '17:33' -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":954,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:33' -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":953,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:33' -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":952,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/unix/append_only_file.mli < ./irmin/src/irmin-pack/unix/append_only_file.mli","success":true}
  {"sample_id":951,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":950,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '153:14' -index 0 -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":949,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Io.rea -position '153:14' -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":948,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Io.rea -position '153:14' -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":947,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '153:14' -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":946,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '143:36' -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":945,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '122:23' -end '122:23' -filename ./irmin/src/irmin-pack/unix/append_only_file.ml < ./irmin/src/irmin-pack/unix/append_only_file.ml","success":true}
  {"sample_id":944,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/stats.mli < ./irmin/src/irmin-pack/stats.mli","success":true}
  {"sample_id":943,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":942,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '85:24' -index 0 -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":941,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix v -position '85:24' -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":940,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix v -position '85:24' -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":939,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '85:24' -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":938,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '90:72' -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":937,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '80:4' -end '93:26' -filename ./irmin/src/irmin-pack/stats.ml < ./irmin/src/irmin-pack/stats.ml","success":true}
  {"sample_id":936,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":935,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '87:43' -index 0 -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":934,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Kind -position '87:43' -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":933,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Kind -position '87:43' -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":932,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '87:43' -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":931,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '85:4' -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":930,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '83:4' -end '84:33' -filename ./irmin/src/irmin-pack/pack_value_intf.ml < ./irmin/src/irmin-pack/pack_value_intf.ml","success":true}
  {"sample_id":929,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":928,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '45:27' -index 0 -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":927,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Pack_value_ -position '45:27' -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":926,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Pack_value_ -position '45:27' -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":925,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '45:27' -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":924,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '46:13' -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":923,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '43:68' -filename ./irmin/src/irmin-pack/pack_value.mli < ./irmin/src/irmin-pack/pack_value.mli","success":true}
  {"sample_id":922,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":921,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '127:57' -index 0 -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":920,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix val -position '127:57' -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":919,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix val -position '127:57' -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":918,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '127:57' -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":917,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '125:30' -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":916,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '128:21' -end '128:59' -filename ./irmin/src/irmin-pack/pack_value.ml < ./irmin/src/irmin-pack/pack_value.ml","success":true}
  {"sample_id":915,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":914,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:35' -index 0 -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":913,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '24:35' -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":912,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '24:35' -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":911,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:35' -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":910,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '28:81' -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":909,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '25:2' -end '28:81' -filename ./irmin/src/irmin-pack/pack_key.ml < ./irmin/src/irmin-pack/pack_key.ml","success":true}
  {"sample_id":908,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.mli < ./irmin/src/irmin-pack/mem/irmin_pack_mem.mli","success":true}
  {"sample_id":907,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":906,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '122:22' -index 0 -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":905,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix AW. -position '122:22' -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":904,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix AW. -position '122:22' -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":903,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '122:22' -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":902,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '89:14' -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":901,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '154:47' -end '154:50' -filename ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml < ./irmin/src/irmin-pack/mem/irmin_pack_mem.ml","success":true}
  {"sample_id":900,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/mem/indexable.mli < ./irmin/src/irmin-pack/mem/indexable.mli","success":true}
  {"sample_id":899,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":898,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '44:23' -index 0 -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":897,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '44:23' -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":896,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '44:23' -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":895,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '44:23' -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":894,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '117:31' -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":893,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '106:11' -end '106:11' -filename ./irmin/src/irmin-pack/mem/indexable.ml < ./irmin/src/irmin-pack/mem/indexable.ml","success":true}
  {"sample_id":892,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":891,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":890,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":889,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":888,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":887,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":886,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:10' -end '20:71' -filename ./irmin/src/irmin-pack/mem/import.ml < ./irmin/src/irmin-pack/mem/import.ml","success":true}
  {"sample_id":885,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":884,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '118:21' -index 0 -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":883,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix in -position '118:21' -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":882,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix in -position '118:21' -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":881,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '118:21' -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":880,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '50:55' -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":879,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '50:32' -end '50:72' -filename ./irmin/src/irmin-pack/layout.ml < ./irmin/src/irmin-pack/layout.ml","success":true}
  {"sample_id":878,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":877,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '74:17' -index 0 -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":876,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix boo -position '74:17' -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":875,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix boo -position '74:17' -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":874,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '74:17' -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":873,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '110:21' -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":872,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '104:2' -end '104:27' -filename ./irmin/src/irmin-pack/irmin_pack_intf.ml < ./irmin/src/irmin-pack/irmin_pack_intf.ml","success":true}
  {"sample_id":871,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":870,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:27' -index 0 -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":869,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_pack_ -position '17:27' -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":868,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_pack_ -position '17:27' -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":867,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:27' -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":866,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:27' -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":865,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/irmin_pack.mli < ./irmin/src/irmin-pack/irmin_pack.mli","success":true}
  {"sample_id":864,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":863,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:35' -index 0 -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":862,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Path.S -position '25:35' -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":861,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Path.S -position '25:35' -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":860,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:35' -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":859,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '25:35' -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":858,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:13' -end '19:21' -filename ./irmin/src/irmin-pack/irmin_pack.ml < ./irmin/src/irmin-pack/irmin_pack.ml","success":true}
  {"sample_id":857,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":856,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '100:31' -index 0 -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":855,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix stri -position '100:31' -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":854,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix stri -position '100:31' -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":853,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '100:31' -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":852,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '210:71' -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":851,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '213:4' -end '216:16' -filename ./irmin/src/irmin-pack/inode_intf.ml < ./irmin/src/irmin-pack/inode_intf.ml","success":true}
  {"sample_id":850,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":849,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":848,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Inode_in -position '17:22' -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":847,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Inode_in -position '17:22' -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":846,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":845,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":844,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/inode.mli < ./irmin/src/irmin-pack/inode.mli","success":true}
  {"sample_id":843,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":842,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '1748:25' -index 0 -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":841,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ke -position '1748:25' -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":840,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ke -position '1748:25' -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":839,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '1748:25' -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":838,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '1350:33' -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":837,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '1246:8' -end '1246:11' -filename ./irmin/src/irmin-pack/inode.ml < ./irmin/src/irmin-pack/inode.ml","success":true}
  {"sample_id":836,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":835,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '46:17' -index 0 -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":834,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '46:17' -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":833,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '46:17' -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":832,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '46:17' -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":831,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '44:23' -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":830,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '42:0' -end '44:23' -filename ./irmin/src/irmin-pack/indexing_strategy.mli < ./irmin/src/irmin-pack/indexing_strategy.mli","success":true}
  {"sample_id":829,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":828,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '50:41' -index 0 -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":827,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix fal -position '50:41' -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":826,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix fal -position '50:41' -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":825,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '50:41' -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":824,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '46:25' -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":823,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '44:24' -end '50:41' -filename ./irmin/src/irmin-pack/indexing_strategy.ml < ./irmin/src/irmin-pack/indexing_strategy.ml","success":true}
  {"sample_id":822,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":821,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '31:65' -index 0 -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":820,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix val -position '31:65' -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":819,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix val -position '31:65' -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":818,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '31:65' -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":817,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '55:4' -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":816,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:2' -end '23:66' -filename ./irmin/src/irmin-pack/indexable_intf.ml < ./irmin/src/irmin-pack/indexable_intf.ml","success":true}
  {"sample_id":815,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":814,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:26' -index 0 -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":813,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Indexable_ -position '17:26' -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":812,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Indexable_ -position '17:26' -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":811,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:26' -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":810,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:26' -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":809,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-pack/indexable.mli < ./irmin/src/irmin-pack/indexable.mli","success":true}
  {"sample_id":808,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/indexable.ml < ./irmin/src/irmin-pack/indexable.ml","success":true}
  {"sample_id":807,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '32:24' -index 0 -filename ./irmin/src/irmin-pack/indexable.ml < ./irmin/src/irmin-pack/indexable.ml","success":true}
  {"sample_id":804,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '32:24' -filename ./irmin/src/irmin-pack/indexable.ml < ./irmin/src/irmin-pack/indexable.ml","success":true}
  {"sample_id":803,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '27:64' -filename ./irmin/src/irmin-pack/indexable.ml < ./irmin/src/irmin-pack/indexable.ml","success":true}
  {"sample_id":802,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '27:44' -end '27:45' -filename ./irmin/src/irmin-pack/indexable.ml < ./irmin/src/irmin-pack/indexable.ml","success":true}
  {"sample_id":801,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":800,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":799,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":798,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":797,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":796,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":795,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '19:10' -end '19:63' -filename ./irmin/src/irmin-pack/import.ml < ./irmin/src/irmin-pack/import.ml","success":true}
  {"sample_id":794,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/conf.mli < ./irmin/src/irmin-pack/conf.mli","success":true}
  {"sample_id":793,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":792,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '150:24' -index 0 -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":791,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conf -position '150:24' -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":790,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conf -position '150:24' -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":789,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '150:24' -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":788,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '124:33' -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":787,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '124:15' -end '124:20' -filename ./irmin/src/irmin-pack/conf.ml < ./irmin/src/irmin-pack/conf.ml","success":true}
  {"sample_id":786,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":785,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '54:28' -index 0 -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":784,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix AW. -position '54:28' -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":783,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix AW. -position '54:28' -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":782,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '54:28' -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":781,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:2' -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":780,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '33:2' -end '34:66' -filename ./irmin/src/irmin-pack/atomic_write_intf.ml < ./irmin/src/irmin-pack/atomic_write_intf.ml","success":true}
  {"sample_id":779,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/atomic_write.mli < ./irmin/src/irmin-pack/atomic_write.mli","success":true}
  {"sample_id":778,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:29' -index 0 -filename ./irmin/src/irmin-pack/atomic_write.mli < ./irmin/src/irmin-pack/atomic_write.mli","success":true}
  {"sample_id":777,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Atomic_write -position '17:29' -filename ./irmin/src/irmin-pack/atomic_write.mli < ./irmin/src/irmin-pack/atomic_write.mli","success":true}
  {"sample_id":776,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Atomic_write -position '17:29' -filename ./irmin/src/irmin-pack/atomic_write.mli < ./irmin/src/irmin-pack/atomic_write.mli","success":true}
  {"sample_id":775,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:29' -filename ./irmin/src/irmin-pack/atomic_write.mli < ./irmin/src/irmin-pack/atomic_write.mli","success":true}
  {"sample_id":774,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:29' -filename ./irmin/src/irmin-pack/atomic_write.mli < ./irmin/src/irmin-pack/atomic_write.mli","success":true}
  {"sample_id":772,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":771,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:11' -index 0 -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":770,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Impo -position '17:11' -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":769,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Impo -position '17:11' -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":768,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:11' -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":767,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:11' -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":766,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '24:29' -end '24:48' -filename ./irmin/src/irmin-pack/atomic_write.ml < ./irmin/src/irmin-pack/atomic_write.ml","success":true}
  {"sample_id":765,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":764,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '200:46' -index 0 -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":763,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix tool -position '200:46' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":762,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix tool -position '200:46' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":761,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '200:46' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":760,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '147:2' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":759,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '145:2' -end '145:22' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/show.ml","success":true}
  {"sample_id":758,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":757,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '6:26' -index 0 -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":756,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix x -position '6:26' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":755,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix x -position '6:26' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":754,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '6:26' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":753,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '6:27' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":752,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '11:46' -end '11:52' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/ring.ml","success":true}
  {"sample_id":751,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":750,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '77:49' -index 0 -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":749,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix O_RD -position '77:49' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":748,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix O_RD -position '77:49' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":747,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '77:49' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":746,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '49:3' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":745,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '48:10' -end '48:40' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/parse.ml","success":true}
  {"sample_id":744,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":743,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '14:8' -index 0 -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":742,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix op -position '14:8' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":741,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix op -position '14:8' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":740,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '14:8' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":739,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '66:55' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":738,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '67:12' -end '67:15' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/main.ml","success":true}
  {"sample_id":737,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":736,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '2:21' -index 0 -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":735,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Optint. -position '2:21' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":734,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Optint. -position '2:21' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":733,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '2:21' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":732,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '5:2' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":731,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '4:10' -end '4:25' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/import.ml","success":true}
  {"sample_id":730,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":729,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '28:47' -index 0 -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":728,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Con -position '28:47' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":727,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Con -position '28:47' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":726,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '28:47' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":725,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '142:25' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":724,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '53:26' -end '53:31' -filename ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml < ./irmin/src/irmin-pack-tools/tezos_explorer/files.ml","success":true}
  {"sample_id":723,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":722,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '15:20' -index 0 -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":721,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Index. -position '15:20' -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":720,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Index. -position '15:20' -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":719,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '15:20' -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":718,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '25:16' -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":717,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '25:13' -end '25:16' -filename ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml < ./irmin/src/irmin-pack-tools/ppidx/ppidx.ml","success":true}
  {"sample_id":716,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":715,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '41:20' -index 0 -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":714,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Cmd.i -position '41:20' -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":713,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Cmd.i -position '41:20' -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":712,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '41:20' -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":711,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '42:56' -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":710,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '29:44' -end '29:63' -filename ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml < ./irmin/src/irmin-pack-tools/ppcf/ppcf.ml","success":true}
  {"sample_id":709,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/irmin_mirage.mli < ./irmin/src/irmin-mirage/irmin_mirage.mli","success":true}
  {"sample_id":708,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":707,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:51' -index 0 -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":706,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Ptime.to_ -position '21:51' -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":705,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Ptime.to_ -position '21:51' -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":704,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:51' -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":703,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '21:69' -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":702,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '21:8' -end '21:69' -filename ./irmin/src/irmin-mirage/irmin_mirage.ml < ./irmin/src/irmin-mirage/irmin_mirage.ml","success":true}
  {"sample_id":701,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.mli < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.mli","success":true}
  {"sample_id":700,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":699,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:56' -index 0 -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":698,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store. -position '25:56' -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":697,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store. -position '25:56' -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":696,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:56' -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":695,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '71:16' -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":694,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '43:10' -end '43:24' -filename ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml < ./irmin/src/irmin-mirage/graphql/irmin_mirage_graphql.ml","success":true}
  {"sample_id":693,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":692,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '160:70' -index 0 -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":691,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix G. -position '160:70' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":690,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix G. -position '160:70' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":689,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '160:70' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":688,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '120:26' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":687,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '133:2' -end '134:48' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git_intf.ml","success":true}
  {"sample_id":686,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":685,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:33' -index 0 -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":684,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_mirage_g -position '17:33' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":683,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_mirage_g -position '17:33' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":682,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:33' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":681,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:33' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":680,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli < ./irmin/src/irmin-mirage/git/irmin_mirage_git.mli","success":true}
  {"sample_id":679,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":678,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '292:10' -index 0 -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":677,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Sto -position '292:10' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":676,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Sto -position '292:10' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":675,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '292:10' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":674,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '198:49' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":673,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '197:15' -end '197:53' -filename ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml < ./irmin/src/irmin-mirage/git/irmin_mirage_git.ml","success":true}
  {"sample_id":672,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/unix/irmin_http_unix.mli < ./irmin/src/irmin-http/unix/irmin_http_unix.mli","success":true}
  {"sample_id":671,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":670,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '44:32' -index 0 -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":669,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_htt -position '44:32' -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":668,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_htt -position '44:32' -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":667,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '44:32' -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":666,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '40:52' -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":665,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '40:54' -end '40:55' -filename ./irmin/src/irmin-http/unix/irmin_http_unix.ml < ./irmin/src/irmin-http/unix/irmin_http_unix.ml","success":true}
  {"sample_id":664,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/s.ml < ./irmin/src/irmin-http/s.ml","success":true}
  {"sample_id":663,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '80:17' -index 0 -filename ./irmin/src/irmin-http/s.ml < ./irmin/src/irmin-http/s.ml","success":true}
  {"sample_id":662,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '80:17' -filename ./irmin/src/irmin-http/s.ml < ./irmin/src/irmin-http/s.ml","success":true}
  {"sample_id":661,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '80:17' -filename ./irmin/src/irmin-http/s.ml < ./irmin/src/irmin-http/s.ml","success":true}
  {"sample_id":660,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '80:17' -filename ./irmin/src/irmin-http/s.ml < ./irmin/src/irmin-http/s.ml","success":true}
  {"sample_id":659,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '121:24' -filename ./irmin/src/irmin-http/s.ml < ./irmin/src/irmin-http/s.ml","success":true}
  {"sample_id":657,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/irmin_http_server.mli < ./irmin/src/irmin-http/irmin_http_server.mli","success":true}
  {"sample_id":656,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":655,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '298:18' -index 0 -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":654,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lwt.a -position '298:18' -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":653,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lwt.a -position '298:18' -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":652,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '298:18' -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":651,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '206:29' -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":650,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '388:8' -end '388:19' -filename ./irmin/src/irmin-http/irmin_http_server.ml < ./irmin/src/irmin-http/irmin_http_server.ml","success":true}
  {"sample_id":649,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/irmin_http.mli < ./irmin/src/irmin-http/irmin_http.mli","success":true}
  {"sample_id":648,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":647,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '57:35' -index 0 -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":646,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix bu -position '57:35' -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":645,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix bu -position '57:35' -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":644,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '57:35' -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":643,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '43:27' -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":642,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '45:17' -end '45:20' -filename ./irmin/src/irmin-http/irmin_http.ml < ./irmin/src/irmin-http/irmin_http.ml","success":true}
  {"sample_id":641,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/import.ml < ./irmin/src/irmin-http/import.ml","success":true}
  {"sample_id":640,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-http/import.ml < ./irmin/src/irmin-http/import.ml","success":true}
  {"sample_id":639,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-http/import.ml < ./irmin/src/irmin-http/import.ml","success":true}
  {"sample_id":638,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-http/import.ml < ./irmin/src/irmin-http/import.ml","success":true}
  {"sample_id":637,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-http/import.ml < ./irmin/src/irmin-http/import.ml","success":true}
  {"sample_id":636,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-http/import.ml < ./irmin/src/irmin-http/import.ml","success":true}
  {"sample_id":634,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":633,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '22:16' -index 0 -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":632,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix irm -position '22:16' -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":631,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix irm -position '22:16' -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":630,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '22:16' -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":629,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '26:68' -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":628,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '26:64' -end '26:68' -filename ./irmin/src/irmin-http/common.ml < ./irmin/src/irmin-http/common.ml","success":true}
  {"sample_id":627,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/closeable.mli < ./irmin/src/irmin-http/closeable.mli","success":true}
  {"sample_id":626,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":625,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '51:13' -index 0 -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":624,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix clos -position '51:13' -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":623,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix clos -position '51:13' -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":622,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '51:13' -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":621,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '95:22' -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":620,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '81:26' -end '81:29' -filename ./irmin/src/irmin-http/closeable.ml < ./irmin/src/irmin-http/closeable.ml","success":true}
  {"sample_id":619,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.mli < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.mli","success":true}
  {"sample_id":618,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":617,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '72:59' -index 0 -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":616,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix v -position '72:59' -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":615,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix v -position '72:59' -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":614,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '72:59' -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":613,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '74:8' -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":612,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '51:47' -end '51:59' -filename ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml < ./irmin/src/irmin-graphql/unix/irmin_graphql_unix.ml","success":true}
  {"sample_id":611,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-graphql/server.mli < ./irmin/src/irmin-graphql/server.mli","success":true}
  {"sample_id":610,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-graphql/server.ml < ./irmin/src/irmin-graphql/server.ml","success":true}
  {"sample_id":609,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '293:61' -index 0 -filename ./irmin/src/irmin-graphql/server.ml < ./irmin/src/irmin-graphql/server.ml","success":true}
  {"sample_id":606,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '293:61' -filename ./irmin/src/irmin-graphql/server.ml < ./irmin/src/irmin-graphql/server.ml","success":true}
  {"sample_id":605,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '710:14' -filename ./irmin/src/irmin-graphql/server.ml < ./irmin/src/irmin-graphql/server.ml","success":true}
  {"sample_id":604,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '676:8' -end '676:15' -filename ./irmin/src/irmin-graphql/server.ml < ./irmin/src/irmin-graphql/server.ml","success":true}
  {"sample_id":603,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-graphql/import.ml < ./irmin/src/irmin-graphql/import.ml","success":true}
  {"sample_id":602,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-graphql/import.ml < ./irmin/src/irmin-graphql/import.ml","success":true}
  {"sample_id":601,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-graphql/import.ml < ./irmin/src/irmin-graphql/import.ml","success":true}
  {"sample_id":600,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-graphql/import.ml < ./irmin/src/irmin-graphql/import.ml","success":true}
  {"sample_id":599,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-graphql/import.ml < ./irmin/src/irmin-graphql/import.ml","success":true}
  {"sample_id":598,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-graphql/import.ml < ./irmin/src/irmin-graphql/import.ml","success":true}
  {"sample_id":596,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/value_intf.ml < ./irmin/src/irmin-git/value_intf.ml","success":true}
  {"sample_id":595,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '21:57' -index 0 -filename ./irmin/src/irmin-git/value_intf.ml < ./irmin/src/irmin-git/value_intf.ml","success":true}
  {"sample_id":594,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix boo -position '21:57' -filename ./irmin/src/irmin-git/value_intf.ml < ./irmin/src/irmin-git/value_intf.ml","success":true}
  {"sample_id":593,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix boo -position '21:57' -filename ./irmin/src/irmin-git/value_intf.ml < ./irmin/src/irmin-git/value_intf.ml","success":true}
  {"sample_id":592,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '21:57' -filename ./irmin/src/irmin-git/value_intf.ml < ./irmin/src/irmin-git/value_intf.ml","success":true}
  {"sample_id":591,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:2' -filename ./irmin/src/irmin-git/value_intf.ml < ./irmin/src/irmin-git/value_intf.ml","success":true}
  {"sample_id":589,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":588,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:22' -index 0 -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":587,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Value_in -position '17:22' -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":586,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Value_in -position '17:22' -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":585,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:22' -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":584,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:22' -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":583,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '18:0' -end '18:13' -filename ./irmin/src/irmin-git/value.mli < ./irmin/src/irmin-git/value.mli","success":true}
  {"sample_id":582,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/value.ml < ./irmin/src/irmin-git/value.ml","success":true}
  {"sample_id":581,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:17' -index 0 -filename ./irmin/src/irmin-git/value.ml < ./irmin/src/irmin-git/value.ml","success":true}
  {"sample_id":580,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Value_ -position '17:17' -filename ./irmin/src/irmin-git/value.ml < ./irmin/src/irmin-git/value.ml","success":true}
  {"sample_id":579,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Value_ -position '17:17' -filename ./irmin/src/irmin-git/value.ml < ./irmin/src/irmin-git/value.ml","success":true}
  {"sample_id":578,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:17' -filename ./irmin/src/irmin-git/value.ml < ./irmin/src/irmin-git/value.ml","success":true}
  {"sample_id":577,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:17' -filename ./irmin/src/irmin-git/value.ml < ./irmin/src/irmin-git/value.ml","success":true}
  {"sample_id":575,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/unix/xgit_intf.ml < ./irmin/src/irmin-git/unix/xgit_intf.ml","success":true}
  {"sample_id":574,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '76:18' -index 0 -filename ./irmin/src/irmin-git/unix/xgit_intf.ml < ./irmin/src/irmin-git/unix/xgit_intf.ml","success":true}
  {"sample_id":573,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S -position '76:18' -filename ./irmin/src/irmin-git/unix/xgit_intf.ml < ./irmin/src/irmin-git/unix/xgit_intf.ml","success":true}
  {"sample_id":572,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S -position '76:18' -filename ./irmin/src/irmin-git/unix/xgit_intf.ml < ./irmin/src/irmin-git/unix/xgit_intf.ml","success":true}
  {"sample_id":571,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '76:18' -filename ./irmin/src/irmin-git/unix/xgit_intf.ml < ./irmin/src/irmin-git/unix/xgit_intf.ml","success":true}
  {"sample_id":570,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '47:33' -filename ./irmin/src/irmin-git/unix/xgit_intf.ml < ./irmin/src/irmin-git/unix/xgit_intf.ml","success":true}
  {"sample_id":568,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/unix/xgit.mli < ./irmin/src/irmin-git/unix/xgit.mli","success":true}
  {"sample_id":567,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":566,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '55:28' -index 0 -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":565,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Hash -position '55:28' -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":564,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Hash -position '55:28' -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":563,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '55:28' -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":562,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '71:8' -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":561,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '78:36' -end '78:38' -filename ./irmin/src/irmin-git/unix/xgit.ml < ./irmin/src/irmin-git/unix/xgit.ml","success":true}
  {"sample_id":560,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/unix/irmin_git_unix.ml < ./irmin/src/irmin-git/unix/irmin_git_unix.ml","success":true}
  {"sample_id":559,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:11' -index 0 -filename ./irmin/src/irmin-git/unix/irmin_git_unix.ml < ./irmin/src/irmin-git/unix/irmin_git_unix.ml","success":true}
  {"sample_id":558,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Xgi -position '17:11' -filename ./irmin/src/irmin-git/unix/irmin_git_unix.ml < ./irmin/src/irmin-git/unix/irmin_git_unix.ml","success":true}
  {"sample_id":557,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Xgi -position '17:11' -filename ./irmin/src/irmin-git/unix/irmin_git_unix.ml < ./irmin/src/irmin-git/unix/irmin_git_unix.ml","success":true}
  {"sample_id":556,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:11' -filename ./irmin/src/irmin-git/unix/irmin_git_unix.ml < ./irmin/src/irmin-git/unix/irmin_git_unix.ml","success":true}
  {"sample_id":555,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:11' -filename ./irmin/src/irmin-git/unix/irmin_git_unix.ml < ./irmin/src/irmin-git/unix/irmin_git_unix.ml","success":true}
  {"sample_id":553,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/schema.ml < ./irmin/src/irmin-git/schema.ml","success":true}
  {"sample_id":552,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '34:56' -index 0 -filename ./irmin/src/irmin-git/schema.ml < ./irmin/src/irmin-git/schema.ml","success":true}
  {"sample_id":551,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Inf -position '34:56' -filename ./irmin/src/irmin-git/schema.ml < ./irmin/src/irmin-git/schema.ml","success":true}
  {"sample_id":550,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Inf -position '34:56' -filename ./irmin/src/irmin-git/schema.ml < ./irmin/src/irmin-git/schema.ml","success":true}
  {"sample_id":549,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '34:56' -filename ./irmin/src/irmin-git/schema.ml < ./irmin/src/irmin-git/schema.ml","success":true}
  {"sample_id":548,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '50:28' -filename ./irmin/src/irmin-git/schema.ml < ./irmin/src/irmin-git/schema.ml","success":true}
  {"sample_id":546,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/remote.mli < ./irmin/src/irmin-git/remote.mli","success":true}
  {"sample_id":545,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":544,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '77:35' -index 0 -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":543,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Ok -position '77:35' -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":542,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Ok -position '77:35' -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":541,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '77:35' -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":540,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '65:75' -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":539,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '63:44' -end '63:61' -filename ./irmin/src/irmin-git/remote.ml < ./irmin/src/irmin-git/remote.ml","success":true}
  {"sample_id":538,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/reference.mli < ./irmin/src/irmin-git/reference.mli","success":true}
  {"sample_id":537,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":536,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '37:30' -index 0 -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":535,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Ok -position '37:30' -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":534,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Ok -position '37:30' -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":533,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '37:30' -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":532,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '45:78' -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":531,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '39:60' -end '39:62' -filename ./irmin/src/irmin-git/reference.ml < ./irmin/src/irmin-git/reference.ml","success":true}
  {"sample_id":530,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/node.mli < ./irmin/src/irmin-git/node.mli","success":true}
  {"sample_id":529,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/node.ml < ./irmin/src/irmin-git/node.ml","success":true}
  {"sample_id":528,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '58:71' -index 0 -filename ./irmin/src/irmin-git/node.ml < ./irmin/src/irmin-git/node.ml","success":true}
  {"sample_id":525,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '58:71' -filename ./irmin/src/irmin-git/node.ml < ./irmin/src/irmin-git/node.ml","success":true}
  {"sample_id":524,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '54:34' -filename ./irmin/src/irmin-git/node.ml < ./irmin/src/irmin-git/node.ml","success":true}
  {"sample_id":523,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '199:15' -end '199:15' -filename ./irmin/src/irmin-git/node.ml < ./irmin/src/irmin-git/node.ml","success":true}
  {"sample_id":522,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/metadata.mli < ./irmin/src/irmin-git/metadata.mli","success":true}
  {"sample_id":521,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":520,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '33:34' -index 0 -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":519,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix X. -position '33:34' -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":518,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix X. -position '33:34' -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":517,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '33:34' -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":516,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '26:19' -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":515,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '26:22' -end '26:31' -filename ./irmin/src/irmin-git/metadata.ml < ./irmin/src/irmin-git/metadata.ml","success":true}
  {"sample_id":514,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":513,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '107:13' -index 0 -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":512,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix in -position '107:13' -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":511,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix in -position '107:13' -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":510,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '107:13' -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":509,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '102:32' -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":508,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '116:4' -end '117:24' -filename ./irmin/src/irmin-git/irmin_git_intf.ml < ./irmin/src/irmin-git/irmin_git_intf.ml","success":true}
  {"sample_id":507,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":506,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:26' -index 0 -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":505,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_git_ -position '19:26' -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":504,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_git_ -position '19:26' -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":503,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:26' -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":502,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:17' -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":501,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:17' -filename ./irmin/src/irmin-git/irmin_git.mli < ./irmin/src/irmin-git/irmin_git.mli","success":true}
  {"sample_id":500,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":499,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '327:30' -index 0 -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":498,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conte -position '327:30' -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":497,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conte -position '327:30' -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":496,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '327:30' -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":495,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '256:41' -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":494,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '333:10' -end '335:76' -filename ./irmin/src/irmin-git/irmin_git.ml < ./irmin/src/irmin-git/irmin_git.ml","success":true}
  {"sample_id":493,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":492,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":491,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":490,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":489,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":488,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":487,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '20:10' -end '20:66' -filename ./irmin/src/irmin-git/import.ml < ./irmin/src/irmin-git/import.ml","success":true}
  {"sample_id":486,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/contents.mli < ./irmin/src/irmin-git/contents.mli","success":true}
  {"sample_id":485,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":484,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '56:37' -index 0 -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":483,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix String. -position '56:37' -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":482,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix String. -position '56:37' -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":481,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '56:37' -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":480,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '59:48' -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":479,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '62:58' -end '62:64' -filename ./irmin/src/irmin-git/contents.ml < ./irmin/src/irmin-git/contents.ml","success":true}
  {"sample_id":478,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/content_addressable_intf.ml < ./irmin/src/irmin-git/content_addressable_intf.ml","success":true}
  {"sample_id":477,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:32' -index 0 -filename ./irmin/src/irmin-git/content_addressable_intf.ml < ./irmin/src/irmin-git/content_addressable_intf.ml","success":true}
  {"sample_id":476,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S.va -position '29:32' -filename ./irmin/src/irmin-git/content_addressable_intf.ml < ./irmin/src/irmin-git/content_addressable_intf.ml","success":true}
  {"sample_id":475,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S.va -position '29:32' -filename ./irmin/src/irmin-git/content_addressable_intf.ml < ./irmin/src/irmin-git/content_addressable_intf.ml","success":true}
  {"sample_id":474,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:32' -filename ./irmin/src/irmin-git/content_addressable_intf.ml < ./irmin/src/irmin-git/content_addressable_intf.ml","success":true}
  {"sample_id":473,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '31:2' -filename ./irmin/src/irmin-git/content_addressable_intf.ml < ./irmin/src/irmin-git/content_addressable_intf.ml","success":true}
  {"sample_id":471,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":470,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:36' -index 0 -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":469,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Content_address -position '19:36' -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":468,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Content_address -position '19:36' -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":467,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:36' -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":466,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:76' -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":465,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:76' -filename ./irmin/src/irmin-git/content_addressable.mli < ./irmin/src/irmin-git/content_addressable.mli","success":true}
  {"sample_id":464,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":463,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:7' -index 0 -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":462,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Ok -position '24:7' -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":461,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Ok -position '24:7' -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":460,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:7' -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":459,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '25:56' -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":458,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '25:26' -end '25:38' -filename ./irmin/src/irmin-git/content_addressable.ml < ./irmin/src/irmin-git/content_addressable.ml","success":true}
  {"sample_id":457,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/conf.mli < ./irmin/src/irmin-git/conf.mli","success":true}
  {"sample_id":456,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":455,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '69:19' -index 0 -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":454,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix C.a -position '69:19' -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":453,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix C.a -position '69:19' -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":452,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '69:19' -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":451,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '70:16' -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":450,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '69:28' -end '69:38' -filename ./irmin/src/irmin-git/conf.ml < ./irmin/src/irmin-git/conf.ml","success":true}
  {"sample_id":449,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/commit.mli < ./irmin/src/irmin-git/commit.mli","success":true}
  {"sample_id":448,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":447,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '103:31' -index 0 -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":446,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix String. -position '103:31' -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":445,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix String. -position '103:31' -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":444,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '103:31' -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":443,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '81:29' -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":442,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '80:39' -end '80:39' -filename ./irmin/src/irmin-git/commit.ml < ./irmin/src/irmin-git/commit.ml","success":true}
  {"sample_id":441,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/branch.mli < ./irmin/src/irmin-git/branch.mli","success":true}
  {"sample_id":440,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":439,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:23' -index 0 -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":438,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.T -position '24:23' -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":437,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.T -position '24:23' -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":436,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:23' -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":435,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:10' -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":434,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '25:13' -end '25:15' -filename ./irmin/src/irmin-git/branch.ml < ./irmin/src/irmin-git/branch.ml","success":true}
  {"sample_id":433,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/backend.mli < ./irmin/src/irmin-git/backend.mli","success":true}
  {"sample_id":432,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":431,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '81:57' -index 0 -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":430,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '81:57' -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":429,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '81:57' -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":428,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '81:57' -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":427,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '74:40' -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":426,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '98:25' -end '98:37' -filename ./irmin/src/irmin-git/backend.ml < ./irmin/src/irmin-git/backend.ml","success":true}
  {"sample_id":425,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/atomic_write_intf.ml < ./irmin/src/irmin-git/atomic_write_intf.ml","success":true}
  {"sample_id":424,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '39:68' -index 0 -filename ./irmin/src/irmin-git/atomic_write_intf.ml < ./irmin/src/irmin-git/atomic_write_intf.ml","success":true}
  {"sample_id":423,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix val -position '39:68' -filename ./irmin/src/irmin-git/atomic_write_intf.ml < ./irmin/src/irmin-git/atomic_write_intf.ml","success":true}
  {"sample_id":422,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix val -position '39:68' -filename ./irmin/src/irmin-git/atomic_write_intf.ml < ./irmin/src/irmin-git/atomic_write_intf.ml","success":true}
  {"sample_id":421,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '39:68' -filename ./irmin/src/irmin-git/atomic_write_intf.ml < ./irmin/src/irmin-git/atomic_write_intf.ml","success":true}
  {"sample_id":420,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:2' -filename ./irmin/src/irmin-git/atomic_write_intf.ml < ./irmin/src/irmin-git/atomic_write_intf.ml","success":true}
  {"sample_id":418,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":417,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:29' -index 0 -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":416,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Atomic_write -position '19:29' -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":415,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Atomic_write -position '19:29' -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":414,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:29' -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":413,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '20:13' -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":412,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:0' -end '17:80' -filename ./irmin/src/irmin-git/atomic_write.mli < ./irmin/src/irmin-git/atomic_write.mli","success":true}
  {"sample_id":411,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-git/atomic_write.ml < ./irmin/src/irmin-git/atomic_write.ml","success":true}
  {"sample_id":410,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '68:7' -index 0 -filename ./irmin/src/irmin-git/atomic_write.ml < ./irmin/src/irmin-git/atomic_write.ml","success":true}
  {"sample_id":407,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '68:7' -filename ./irmin/src/irmin-git/atomic_write.ml < ./irmin/src/irmin-git/atomic_write.ml","success":true}
  {"sample_id":406,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '48:21' -filename ./irmin/src/irmin-git/atomic_write.ml < ./irmin/src/irmin-git/atomic_write.ml","success":true}
  {"sample_id":405,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '44:21' -end '44:21' -filename ./irmin/src/irmin-git/atomic_write.ml < ./irmin/src/irmin-git/atomic_write.ml","success":true}
  {"sample_id":404,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.mli < ./irmin/src/irmin-fs/unix/irmin_fs_unix.mli","success":true}
  {"sample_id":403,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":402,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '91:44' -index 0 -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":401,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix fil -position '91:44' -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":400,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix fil -position '91:44' -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":399,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '91:44' -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":398,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '205:52' -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":397,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '191:7' -end '191:17' -filename ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml < ./irmin/src/irmin-fs/unix/irmin_fs_unix.ml","success":true}
  {"sample_id":396,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-fs/irmin_fs.mli < ./irmin/src/irmin-fs/irmin_fs.mli","success":true}
  {"sample_id":395,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":394,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '35:31' -index 0 -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":393,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix stri -position '35:31' -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":392,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix stri -position '35:31' -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":391,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '35:31' -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":390,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '61:18' -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":389,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '68:15' -end '68:62' -filename ./irmin/src/irmin-fs/irmin_fs.ml < ./irmin/src/irmin-fs/irmin_fs.ml","success":true}
  {"sample_id":388,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-fs/import.ml < ./irmin/src/irmin-fs/import.ml","success":true}
  {"sample_id":387,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-fs/import.ml < ./irmin/src/irmin-fs/import.ml","success":true}
  {"sample_id":386,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-fs/import.ml < ./irmin/src/irmin-fs/import.ml","success":true}
  {"sample_id":385,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-fs/import.ml < ./irmin/src/irmin-fs/import.ml","success":true}
  {"sample_id":384,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-fs/import.ml < ./irmin/src/irmin-fs/import.ml","success":true}
  {"sample_id":383,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-fs/import.ml < ./irmin/src/irmin-fs/import.ml","success":true}
  {"sample_id":381,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/time.mli < ./irmin/src/irmin-containers/time.mli","success":true}
  {"sample_id":380,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":379,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:17' -index 0 -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":378,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix S -position '24:17' -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":377,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix S -position '24:17' -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":376,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:17' -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":375,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:26' -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":374,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '28:4' -end '30:23' -filename ./irmin/src/irmin-containers/time.ml < ./irmin/src/irmin-containers/time.ml","success":true}
  {"sample_id":373,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/stores.ml < ./irmin/src/irmin-containers/stores.ml","success":true}
  {"sample_id":372,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:46' -index 0 -filename ./irmin/src/irmin-containers/stores.ml < ./irmin/src/irmin-containers/stores.ml","success":true}
  {"sample_id":371,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Content_ad -position '19:46' -filename ./irmin/src/irmin-containers/stores.ml < ./irmin/src/irmin-containers/stores.ml","success":true}
  {"sample_id":370,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Content_ad -position '19:46' -filename ./irmin/src/irmin-containers/stores.ml < ./irmin/src/irmin-containers/stores.ml","success":true}
  {"sample_id":369,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:46' -filename ./irmin/src/irmin-containers/stores.ml < ./irmin/src/irmin-containers/stores.ml","success":true}
  {"sample_id":368,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:2' -filename ./irmin/src/irmin-containers/stores.ml < ./irmin/src/irmin-containers/stores.ml","success":true}
  {"sample_id":366,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/lww_register.mli < ./irmin/src/irmin-containers/lww_register.mli","success":true}
  {"sample_id":365,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/lww_register.ml < ./irmin/src/irmin-containers/lww_register.ml","success":true}
  {"sample_id":364,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:11' -index 0 -filename ./irmin/src/irmin-containers/lww_register.ml < ./irmin/src/irmin-containers/lww_register.ml","success":true}
  {"sample_id":361,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:11' -filename ./irmin/src/irmin-containers/lww_register.ml < ./irmin/src/irmin-containers/lww_register.ml","success":true}
  {"sample_id":360,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '25:50' -filename ./irmin/src/irmin-containers/lww_register.ml < ./irmin/src/irmin-containers/lww_register.ml","success":true}
  {"sample_id":359,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '27:24' -end '27:25' -filename ./irmin/src/irmin-containers/lww_register.ml < ./irmin/src/irmin-containers/lww_register.ml","success":true}
  {"sample_id":358,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/linked_log.mli < ./irmin/src/irmin-containers/linked_log.mli","success":true}
  {"sample_id":357,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":356,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '77:30' -index 0 -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":355,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lv -position '77:30' -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":354,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lv -position '77:30' -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":353,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '77:30' -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":352,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '63:34' -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":351,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '71:17' -end '71:34' -filename ./irmin/src/irmin-containers/linked_log.ml < ./irmin/src/irmin-containers/linked_log.ml","success":true}
  {"sample_id":350,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":349,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '41:59' -index 0 -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":348,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Stores.Content -position '41:59' -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":347,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Stores.Content -position '41:59' -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":346,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '41:59' -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":345,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:25' -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":344,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '43:0' -end '45:46' -filename ./irmin/src/irmin-containers/irmin_containers.ml < ./irmin/src/irmin-containers/irmin_containers.ml","success":true}
  {"sample_id":343,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/import.ml < ./irmin/src/irmin-containers/import.ml","success":true}
  {"sample_id":342,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-containers/import.ml < ./irmin/src/irmin-containers/import.ml","success":true}
  {"sample_id":341,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-containers/import.ml < ./irmin/src/irmin-containers/import.ml","success":true}
  {"sample_id":340,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-containers/import.ml < ./irmin/src/irmin-containers/import.ml","success":true}
  {"sample_id":339,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-containers/import.ml < ./irmin/src/irmin-containers/import.ml","success":true}
  {"sample_id":338,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-containers/import.ml < ./irmin/src/irmin-containers/import.ml","success":true}
  {"sample_id":336,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/counter.mli < ./irmin/src/irmin-containers/counter.mli","success":true}
  {"sample_id":335,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":334,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '20:32' -index 0 -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":333,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Con -position '20:32' -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":332,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Con -position '20:32' -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":331,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '20:32' -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":330,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '55:32' -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":329,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '53:51' -end '53:52' -filename ./irmin/src/irmin-containers/counter.ml < ./irmin/src/irmin-containers/counter.ml","success":true}
  {"sample_id":328,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/blob_log.mli < ./irmin/src/irmin-containers/blob_log.mli","success":true}
  {"sample_id":327,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":326,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '43:55' -index 0 -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":325,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix v2 -position '43:55' -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":324,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix v2 -position '43:55' -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":323,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '43:55' -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":322,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '46:30' -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":321,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '35:17' -end '35:18' -filename ./irmin/src/irmin-containers/blob_log.ml < ./irmin/src/irmin-containers/blob_log.ml","success":true}
  {"sample_id":320,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/resolver.mli < ./irmin/src/irmin-cli/resolver.mli","success":true}
  {"sample_id":319,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":318,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '571:7' -index 0 -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":317,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Non -position '571:7' -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":316,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Non -position '571:7' -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":315,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '571:7' -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":314,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '389:48' -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":313,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '393:21' -end '393:24' -filename ./irmin/src/irmin-cli/resolver.ml < ./irmin/src/irmin-cli/resolver.ml","success":true}
  {"sample_id":312,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/irmin_cli.mli < ./irmin/src/irmin-cli/irmin_cli.mli","success":true}
  {"sample_id":311,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/irmin_cli.ml < ./irmin/src/irmin-cli/irmin_cli.ml","success":true}
  {"sample_id":310,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:10' -index 0 -filename ./irmin/src/irmin-cli/irmin_cli.ml < ./irmin/src/irmin-cli/irmin_cli.ml","success":true}
  {"sample_id":309,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Cl -position '17:10' -filename ./irmin/src/irmin-cli/irmin_cli.ml < ./irmin/src/irmin-cli/irmin_cli.ml","success":true}
  {"sample_id":308,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Cl -position '17:10' -filename ./irmin/src/irmin-cli/irmin_cli.ml < ./irmin/src/irmin-cli/irmin_cli.ml","success":true}
  {"sample_id":307,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:10' -filename ./irmin/src/irmin-cli/irmin_cli.ml < ./irmin/src/irmin-cli/irmin_cli.ml","success":true}
  {"sample_id":306,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:10' -filename ./irmin/src/irmin-cli/irmin_cli.ml < ./irmin/src/irmin-cli/irmin_cli.ml","success":true}
  {"sample_id":304,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/info.mli < ./irmin/src/irmin-cli/info.mli","success":true}
  {"sample_id":303,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":302,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '32:16' -index 0 -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":301,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix auth -position '32:16' -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":300,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix auth -position '32:16' -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":299,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '32:16' -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":298,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '32:30' -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":297,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '30:29' -end '30:30' -filename ./irmin/src/irmin-cli/info.ml < ./irmin/src/irmin-cli/info.ml","success":true}
  {"sample_id":296,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/import.ml < ./irmin/src/irmin-cli/import.ml","success":true}
  {"sample_id":295,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/src/irmin-cli/import.ml < ./irmin/src/irmin-cli/import.ml","success":true}
  {"sample_id":294,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/src/irmin-cli/import.ml < ./irmin/src/irmin-cli/import.ml","success":true}
  {"sample_id":293,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/src/irmin-cli/import.ml < ./irmin/src/irmin-cli/import.ml","success":true}
  {"sample_id":292,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/src/irmin-cli/import.ml < ./irmin/src/irmin-cli/import.ml","success":true}
  {"sample_id":291,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/src/irmin-cli/import.ml < ./irmin/src/irmin-cli/import.ml","success":true}
  {"sample_id":289,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":288,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '25:21' -index 0 -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":287,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix comm -position '25:21' -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":286,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix comm -position '25:21' -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":285,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '25:21' -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":284,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '29:60' -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":283,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '29:0' -end '29:60' -filename ./irmin/src/irmin-cli/cli.mli < ./irmin/src/irmin-cli/cli.mli","success":true}
  {"sample_id":282,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":281,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '439:50' -index 0 -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":280,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix mess -position '439:50' -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":279,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix mess -position '439:50' -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":278,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '439:50' -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":277,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '303:26' -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":276,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '273:39' -end '273:39' -filename ./irmin/src/irmin-cli/cli.ml < ./irmin/src/irmin-cli/cli.ml","success":true}
  {"sample_id":275,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-cli/bin/main.ml < ./irmin/src/irmin-cli/bin/main.ml","success":true}
  {"sample_id":274,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:5' -index 0 -filename ./irmin/src/irmin-cli/bin/main.ml < ./irmin/src/irmin-cli/bin/main.ml","success":true}
  {"sample_id":271,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:5' -filename ./irmin/src/irmin-cli/bin/main.ml < ./irmin/src/irmin-cli/bin/main.ml","success":true}
  {"sample_id":270,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:41' -filename ./irmin/src/irmin-cli/bin/main.ml < ./irmin/src/irmin-cli/bin/main.ml","success":true}
  {"sample_id":269,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '17:9' -end '17:41' -filename ./irmin/src/irmin-cli/bin/main.ml < ./irmin/src/irmin-cli/bin/main.ml","success":true}
  {"sample_id":268,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-chunk/irmin_chunk.mli < ./irmin/src/irmin-chunk/irmin_chunk.mli","success":true}
  {"sample_id":267,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":266,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '154:31' -index 0 -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":265,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix ke -position '154:31' -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":264,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix ke -position '154:31' -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":263,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '154:31' -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":262,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '95:64' -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":261,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '94:7' -end '94:7' -filename ./irmin/src/irmin-chunk/irmin_chunk.ml < ./irmin/src/irmin-chunk/irmin_chunk.ml","success":true}
  {"sample_id":260,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/src/irmin-chunk/import.ml < ./irmin/src/irmin-chunk/import.ml","success":true}
  {"sample_id":259,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '18:32' -index 0 -filename ./irmin/src/irmin-chunk/import.ml < ./irmin/src/irmin-chunk/import.ml","success":true}
  {"sample_id":258,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-chunk/import.ml < ./irmin/src/irmin-chunk/import.ml","success":true}
  {"sample_id":257,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '18:32' -filename ./irmin/src/irmin-chunk/import.ml < ./irmin/src/irmin-chunk/import.ml","success":true}
  {"sample_id":256,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '18:32' -filename ./irmin/src/irmin-chunk/import.ml < ./irmin/src/irmin-chunk/import.ml","success":true}
  {"sample_id":255,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:32' -filename ./irmin/src/irmin-chunk/import.ml < ./irmin/src/irmin-chunk/import.ml","success":true}
  {"sample_id":253,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":252,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '27:15' -index 0 -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":251,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix lis -position '27:15' -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":250,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix lis -position '27:15' -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":249,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '27:15' -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":248,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '38:6' -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":247,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '32:12' -end '32:12' -filename ./irmin/examples/trees.ml < ./irmin/examples/trees.ml","success":true}
  {"sample_id":246,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":245,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '38:27' -index 0 -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":244,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store.Tr -position '38:27' -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":243,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store.Tr -position '38:27' -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":242,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '38:27' -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":241,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '42:30' -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":240,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '42:22' -end '42:30' -filename ./irmin/examples/sync.ml < ./irmin/examples/sync.ml","success":true}
  {"sample_id":239,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":238,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":237,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conf -position '17:32' -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":236,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conf -position '17:32' -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":235,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":234,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '13:45' -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":233,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '13:36' -end '13:41' -filename ./irmin/examples/readme.ml < ./irmin/examples/readme.ml","success":true}
  {"sample_id":232,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":231,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '24:16' -index 0 -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":230,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Array.l -position '24:16' -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":229,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Array.l -position '24:16' -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":228,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '24:16' -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":227,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '24:29' -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":226,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '23:15' -end '23:19' -filename ./irmin/examples/push.ml < ./irmin/examples/push.ml","success":true}
  {"sample_id":225,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":224,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '159:48' -index 0 -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":223,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Random. -position '159:48' -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":222,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Random. -position '159:48' -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":221,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '159:48' -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":220,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '119:26' -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":219,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '119:12' -end '119:69' -filename ./irmin/examples/process.ml < ./irmin/examples/process.ml","success":true}
  {"sample_id":218,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":217,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '27:47' -index 0 -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":216,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin_mem. -position '27:47' -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":215,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin_mem. -position '27:47' -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":214,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '27:47' -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":213,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '18:37' -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":212,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '28:52' -end '28:64' -filename ./irmin/examples/plugin/plugin.ml < ./irmin/examples/plugin/plugin.ml","success":true}
  {"sample_id":211,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/irmin_git_store.ml < ./irmin/examples/irmin_git_store.ml","success":true}
  {"sample_id":210,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '42:47' -index 0 -filename ./irmin/examples/irmin_git_store.ml < ./irmin/examples/irmin_git_store.ml","success":true}
  {"sample_id":207,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '42:47' -filename ./irmin/examples/irmin_git_store.ml < ./irmin/examples/irmin_git_store.ml","success":true}
  {"sample_id":206,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '46:17' -filename ./irmin/examples/irmin_git_store.ml < ./irmin/examples/irmin_git_store.ml","success":true}
  {"sample_id":205,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '45:2' -end '45:14' -filename ./irmin/examples/irmin_git_store.ml < ./irmin/examples/irmin_git_store.ml","success":true}
  {"sample_id":204,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":203,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '77:43' -index 0 -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":202,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Con -position '77:43' -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":201,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Con -position '77:43' -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":200,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '77:43' -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":199,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '97:41' -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":198,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '99:9' -end '99:10' -filename ./irmin/examples/irmin-pack/kv.ml < ./irmin/examples/irmin-pack/kv.ml","success":true}
  {"sample_id":197,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":196,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '163:11' -index 0 -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":195,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Ok -position '163:11' -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":194,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Ok -position '163:11' -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":193,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '163:11' -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":192,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '120:40' -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":191,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '122:18' -end '122:18' -filename ./irmin/examples/irmin-pack/gc.ml < ./irmin/examples/irmin-pack/gc.ml","success":true}
  {"sample_id":190,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/fold.ml < ./irmin/examples/fold.ml","success":true}
  {"sample_id":189,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '91:8' -index 0 -filename ./irmin/examples/fold.ml < ./irmin/examples/fold.ml","success":true}
  {"sample_id":186,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '91:8' -filename ./irmin/examples/fold.ml < ./irmin/examples/fold.ml","success":true}
  {"sample_id":185,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '71:32' -filename ./irmin/examples/fold.ml < ./irmin/examples/fold.ml","success":true}
  {"sample_id":184,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '71:14' -end '71:32' -filename ./irmin/examples/fold.ml < ./irmin/examples/fold.ml","success":true}
  {"sample_id":183,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":182,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '78:25' -index 0 -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":181,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Store.He -position '78:25' -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":180,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Store.He -position '78:25' -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":179,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '78:25' -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":178,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '47:30' -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":177,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '47:7' -end '47:7' -filename ./irmin/examples/deploy.ml < ./irmin/examples/deploy.ml","success":true}
  {"sample_id":176,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":175,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '101:20' -index 0 -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":174,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Lwt_mai -position '101:20' -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":173,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Lwt_mai -position '101:20' -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":172,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '101:20' -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":171,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '99:27' -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":170,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '98:32' -end '98:32' -filename ./irmin/examples/custom_storage.ml < ./irmin/examples/custom_storage.ml","success":true}
  {"sample_id":169,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":168,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '92:61' -index 0 -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":167,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix List. -position '92:61' -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":166,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix List. -position '92:61' -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":165,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '92:61' -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":164,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '81:40' -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":163,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '75:16' -end '75:26' -filename ./irmin/examples/custom_merge.ml < ./irmin/examples/custom_merge.ml","success":true}
  {"sample_id":162,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":161,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '112:32' -index 0 -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":160,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix conf -position '112:32' -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":159,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix conf -position '112:32' -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":158,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '112:32' -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":157,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '77:59' -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":156,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '85:14' -end '85:43' -filename ./irmin/examples/custom_graphql.ml < ./irmin/examples/custom_graphql.ml","success":true}
  {"sample_id":155,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/examples/config.ml < ./irmin/examples/config.ml","success":true}
  {"sample_id":154,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '19:10' -index 0 -filename ./irmin/examples/config.ml < ./irmin/examples/config.ml","success":true}
  {"sample_id":151,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '19:10' -filename ./irmin/examples/config.ml < ./irmin/examples/config.ml","success":true}
  {"sample_id":150,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '22:59' -filename ./irmin/examples/config.ml < ./irmin/examples/config.ml","success":true}
  {"sample_id":149,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '22:2' -end '22:59' -filename ./irmin/examples/config.ml < ./irmin/examples/config.ml","success":true}
  {"sample_id":148,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":147,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '118:37' -index 0 -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":146,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Random.St -position '118:37' -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":145,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Random.St -position '118:37' -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":144,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '118:37' -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":143,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '86:36' -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":142,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '87:2' -end '89:5' -filename ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml < ./irmin/bench/irmin/data/bench_fixed_size_string_set.ml","success":true}
  {"sample_id":141,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/tree.mli < ./irmin/bench/irmin-pack/tree.mli","success":true}
  {"sample_id":134,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":133,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '177:24' -index 0 -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":132,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix replay_tr -position '177:24' -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":131,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix replay_tr -position '177:24' -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":130,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '177:24' -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":129,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '155:54' -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":128,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '152:15' -end '152:23' -filename ./irmin/bench/irmin-pack/tree.ml < ./irmin/bench/irmin-pack/tree.ml","success":true}
  {"sample_id":127,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":126,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '106:17' -index 0 -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":125,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix name_pe -position '106:17' -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":124,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix name_pe -position '106:17' -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":123,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '106:17' -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":122,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '213:57' -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":121,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '211:2' -end '211:16' -filename ./irmin/bench/irmin-pack/trace_stats.ml < ./irmin/bench/irmin-pack/trace_stats.ml","success":true}
  {"sample_id":120,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.mli < ./irmin/bench/irmin-pack/trace_stat_summary_utils.mli","success":true}
  {"sample_id":119,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":118,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '85:13' -index 0 -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":117,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix no -position '85:13' -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":116,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix no -position '85:13' -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":115,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '85:13' -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":114,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '363:16' -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":113,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '333:48' -end '339:33' -filename ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml < ./irmin/bench/irmin-pack/trace_stat_summary_utils.ml","success":true}
  {"sample_id":112,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":111,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '967:29' -index 0 -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":110,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix List.s -position '967:29' -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":109,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix List.s -position '967:29' -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":108,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '967:29' -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":107,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '1005:15' -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":106,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '930:6' -end '930:78' -filename ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml < ./irmin/bench/irmin-pack/trace_stat_summary_pp.ml","success":true}
  {"sample_id":105,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml < ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml","success":true}
  {"sample_id":104,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '39:41' -index 0 -filename ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml < ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml","success":true}
  {"sample_id":101,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '39:41' -filename ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml < ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml","success":true}
  {"sample_id":100,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '39:38' -filename ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml < ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml","success":true}
  {"sample_id":99,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '39:37' -end '39:38' -filename ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml < ./irmin/bench/irmin-pack/trace_stat_summary_conf.ml","success":true}
  {"sample_id":98,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":97,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '181:9' -index 0 -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":96,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix nam -position '181:9' -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":95,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix nam -position '181:9' -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":94,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '181:9' -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":93,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '137:20' -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":92,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '133:13' -end '133:19' -filename ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml < ./irmin/bench/irmin-pack/trace_stat_summary_cb.ml","success":true}
  {"sample_id":91,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":90,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '407:35' -index 0 -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":89,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Span.Ma -position '407:35' -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":88,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Span.Ma -position '407:35' -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":87,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '407:35' -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":86,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '391:9' -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":85,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '377:44' -end '377:54' -filename ./irmin/bench/irmin-pack/trace_stat_summary.ml < ./irmin/bench/irmin-pack/trace_stat_summary.ml","success":true}
  {"sample_id":84,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":83,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '29:20' -index 0 -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":82,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix boo -position '29:20' -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":81,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix boo -position '29:20' -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":80,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '29:20' -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":79,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '124:26' -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":78,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '38:2' -end '81:50' -filename ./irmin/bench/irmin-pack/trace_replay_intf.ml < ./irmin/bench/irmin-pack/trace_replay_intf.ml","success":true}
  {"sample_id":77,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_replay.mli < ./irmin/bench/irmin-pack/trace_replay.mli","success":true}
  {"sample_id":76,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:29' -index 0 -filename ./irmin/bench/irmin-pack/trace_replay.mli < ./irmin/bench/irmin-pack/trace_replay.mli","success":true}
  {"sample_id":75,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Trace_replay -position '17:29' -filename ./irmin/bench/irmin-pack/trace_replay.mli < ./irmin/bench/irmin-pack/trace_replay.mli","success":true}
  {"sample_id":74,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Trace_replay -position '17:29' -filename ./irmin/bench/irmin-pack/trace_replay.mli < ./irmin/bench/irmin-pack/trace_replay.mli","success":true}
  {"sample_id":73,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:29' -filename ./irmin/bench/irmin-pack/trace_replay.mli < ./irmin/bench/irmin-pack/trace_replay.mli","success":true}
  {"sample_id":72,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:29' -filename ./irmin/bench/irmin-pack/trace_replay.mli < ./irmin/bench/irmin-pack/trace_replay.mli","success":true}
  {"sample_id":70,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":69,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '239:21' -index 0 -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":68,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix t -position '239:21' -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":67,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix t -position '239:21' -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":66,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '239:21' -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":65,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '197:25' -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":64,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '178:16' -end '178:17' -filename ./irmin/bench/irmin-pack/trace_replay.ml < ./irmin/bench/irmin-pack/trace_replay.ml","success":true}
  {"sample_id":63,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":62,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '86:28' -index 0 -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":61,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix contex -position '86:28' -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":60,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix contex -position '86:28' -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":59,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '86:28' -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":58,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '132:67' -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":57,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '132:13' -end '132:67' -filename ./irmin/bench/irmin-pack/trace_definitions.ml < ./irmin/bench/irmin-pack/trace_definitions.ml","success":true}
  {"sample_id":56,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":55,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '105:63' -index 0 -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":54,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix unfo -position '105:63' -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":53,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix unfo -position '105:63' -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":52,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '105:63' -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":51,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '105:12' -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":50,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '105:12' -end '105:12' -filename ./irmin/bench/irmin-pack/trace_common.ml < ./irmin/bench/irmin-pack/trace_common.ml","success":true}
  {"sample_id":49,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/trace_collection.ml < ./irmin/bench/irmin-pack/trace_collection.ml","success":true}
  {"sample_id":48,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '45:35' -index 0 -filename ./irmin/bench/irmin-pack/trace_collection.ml < ./irmin/bench/irmin-pack/trace_collection.ml","success":true}
  {"sample_id":45,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '45:35' -filename ./irmin/bench/irmin-pack/trace_collection.ml < ./irmin/bench/irmin-pack/trace_collection.ml","success":true}
  {"sample_id":44,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '45:32' -filename ./irmin/bench/irmin-pack/trace_collection.ml < ./irmin/bench/irmin-pack/trace_collection.ml","success":true}
  {"sample_id":43,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '47:30' -end '47:54' -filename ./irmin/bench/irmin-pack/trace_collection.ml < ./irmin/bench/irmin-pack/trace_collection.ml","success":true}
  {"sample_id":42,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/tezos_history_metrics.ml < ./irmin/bench/irmin-pack/tezos_history_metrics.ml","success":true}
  {"sample_id":41,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '178:2' -index 0 -filename ./irmin/bench/irmin-pack/tezos_history_metrics.ml < ./irmin/bench/irmin-pack/tezos_history_metrics.ml","success":true}
  {"sample_id":38,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '178:2' -filename ./irmin/bench/irmin-pack/tezos_history_metrics.ml < ./irmin/bench/irmin-pack/tezos_history_metrics.ml","success":true}
  {"sample_id":37,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '46:32' -filename ./irmin/bench/irmin-pack/tezos_history_metrics.ml < ./irmin/bench/irmin-pack/tezos_history_metrics.ml","success":true}
  {"sample_id":36,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '46:27' -end '46:32' -filename ./irmin/bench/irmin-pack/tezos_history_metrics.ml < ./irmin/bench/irmin-pack/tezos_history_metrics.ml","success":true}
  {"sample_id":35,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/main.mli < ./irmin/bench/irmin-pack/main.mli","success":true}
  {"sample_id":28,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":27,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '36:17' -index 0 -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":26,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix f -position '36:17' -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":25,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix f -position '36:17' -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":24,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '36:17' -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":23,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '28:34' -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":22,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '36:2' -end '37:46' -filename ./irmin/bench/irmin-pack/main.ml < ./irmin/bench/irmin-pack/main.ml","success":true}
  {"sample_id":21,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/import.ml < ./irmin/bench/irmin-pack/import.ml","success":true}
  {"sample_id":20,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '17:32' -index 0 -filename ./irmin/bench/irmin-pack/import.ml < ./irmin/bench/irmin-pack/import.ml","success":true}
  {"sample_id":19,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/bench/irmin-pack/import.ml < ./irmin/bench/irmin-pack/import.ml","success":true}
  {"sample_id":18,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix Irmin.Export_ -position '17:32' -filename ./irmin/bench/irmin-pack/import.ml < ./irmin/bench/irmin-pack/import.ml","success":true}
  {"sample_id":17,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '17:32' -filename ./irmin/bench/irmin-pack/import.ml < ./irmin/bench/irmin-pack/import.ml","success":true}
  {"sample_id":16,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '17:32' -filename ./irmin/bench/irmin-pack/import.ml < ./irmin/bench/irmin-pack/import.ml","success":true}
  {"sample_id":14,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/bench_common.mli < ./irmin/bench/irmin-pack/bench_common.mli","success":true}
  {"sample_id":13,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":12,"merlin_id":0,"cmd":" ocamlmerlin server locate -look-for ml -position '168:34' -index 0 -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":11,"merlin_id":0,"cmd":"ocamlmerlin server expand-prefix -prefix tre -position '168:34' -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":10,"merlin_id":0,"cmd":"ocamlmerlin server complete-prefix -prefix tre -position '168:34' -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":9,"merlin_id":0,"cmd":"ocamlmerlin server occurrences -identifier-at '168:34' -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":8,"merlin_id":0,"cmd":"ocamlmerlin server type-enclosing -position '177:21' -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":7,"merlin_id":0,"cmd":"ocamlmerlin server case-analysis -start '173:22' -end '181:13' -filename ./irmin/bench/irmin-pack/bench_common.ml < ./irmin/bench/irmin-pack/bench_common.ml","success":true}
  {"sample_id":6,"merlin_id":0,"cmd":"ocamlmerlin server errors -filename ./irmin/bench/irmin-pack/_layers.mli < ./irmin/bench/irmin-pack/_layers.mli","success":true}

