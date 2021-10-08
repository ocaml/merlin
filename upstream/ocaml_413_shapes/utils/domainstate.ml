# 1 "utils/domainstate.ml.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 367 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "utils/domainstate.ml.c" 2
# 17 "utils/domainstate.ml.c"
type t =


# 1 "runtime/caml/domain_state.tbl" 1
# 17 "runtime/caml/domain_state.tbl"
| Domain_young_limit
| Domain_young_ptr


| Domain_exception_pointer


| Domain_young_base
| Domain_young_start
| Domain_young_end
| Domain_young_alloc_start
| Domain_young_alloc_end
| Domain_young_alloc_mid
| Domain_young_trigger
| Domain_minor_heap_wsz
| Domain_in_minor_collection
| Domain_extra_heap_resources_minor
| Domain_ref_table
| Domain_ephe_ref_table
| Domain_custom_table


| Domain_mark_stack


| Domain_stack_low
| Domain_stack_high
| Domain_stack_threshold
| Domain_extern_sp
| Domain_trapsp
| Domain_trap_barrier
| Domain_external_raise
| Domain_exn_bucket


| Domain_top_of_stack
| Domain_bottom_of_stack
| Domain_last_return_address
| Domain_gc_regs


| Domain_backtrace_active
| Domain_backtrace_pos
| Domain_backtrace_buffer
| Domain_backtrace_last_exn


| Domain_compare_unordered
| Domain_requested_major_slice
| Domain_requested_minor_gc
| Domain_local_roots

| Domain_stat_minor_words
| Domain_stat_promoted_words
| Domain_stat_major_words
| Domain_stat_minor_collections
| Domain_stat_major_collections
| Domain_stat_heap_wsz
| Domain_stat_top_heap_wsz
| Domain_stat_compactions
| Domain_stat_forced_major_collections
| Domain_stat_heap_chunks


| Domain_eventlog_startup_timestamp
| Domain_eventlog_startup_pid
| Domain_eventlog_paused
| Domain_eventlog_enabled
| Domain_eventlog_out
# 20 "utils/domainstate.ml.c" 2


let idx_of_field =
  let curr = 0 in




# 1 "runtime/caml/domain_state.tbl" 1
# 17 "runtime/caml/domain_state.tbl"
let idx__young_limit = curr in let curr = curr + 1 in
let idx__young_ptr = curr in let curr = curr + 1 in


let idx__exception_pointer = curr in let curr = curr + 1 in


let idx__young_base = curr in let curr = curr + 1 in
let idx__young_start = curr in let curr = curr + 1 in
let idx__young_end = curr in let curr = curr + 1 in
let idx__young_alloc_start = curr in let curr = curr + 1 in
let idx__young_alloc_end = curr in let curr = curr + 1 in
let idx__young_alloc_mid = curr in let curr = curr + 1 in
let idx__young_trigger = curr in let curr = curr + 1 in
let idx__minor_heap_wsz = curr in let curr = curr + 1 in
let idx__in_minor_collection = curr in let curr = curr + 1 in
let idx__extra_heap_resources_minor = curr in let curr = curr + 1 in
let idx__ref_table = curr in let curr = curr + 1 in
let idx__ephe_ref_table = curr in let curr = curr + 1 in
let idx__custom_table = curr in let curr = curr + 1 in


let idx__mark_stack = curr in let curr = curr + 1 in


let idx__stack_low = curr in let curr = curr + 1 in
let idx__stack_high = curr in let curr = curr + 1 in
let idx__stack_threshold = curr in let curr = curr + 1 in
let idx__extern_sp = curr in let curr = curr + 1 in
let idx__trapsp = curr in let curr = curr + 1 in
let idx__trap_barrier = curr in let curr = curr + 1 in
let idx__external_raise = curr in let curr = curr + 1 in
let idx__exn_bucket = curr in let curr = curr + 1 in


let idx__top_of_stack = curr in let curr = curr + 1 in
let idx__bottom_of_stack = curr in let curr = curr + 1 in
let idx__last_return_address = curr in let curr = curr + 1 in
let idx__gc_regs = curr in let curr = curr + 1 in


let idx__backtrace_active = curr in let curr = curr + 1 in
let idx__backtrace_pos = curr in let curr = curr + 1 in
let idx__backtrace_buffer = curr in let curr = curr + 1 in
let idx__backtrace_last_exn = curr in let curr = curr + 1 in


let idx__compare_unordered = curr in let curr = curr + 1 in
let idx__requested_major_slice = curr in let curr = curr + 1 in
let idx__requested_minor_gc = curr in let curr = curr + 1 in
let idx__local_roots = curr in let curr = curr + 1 in

let idx__stat_minor_words = curr in let curr = curr + 1 in
let idx__stat_promoted_words = curr in let curr = curr + 1 in
let idx__stat_major_words = curr in let curr = curr + 1 in
let idx__stat_minor_collections = curr in let curr = curr + 1 in
let idx__stat_major_collections = curr in let curr = curr + 1 in
let idx__stat_heap_wsz = curr in let curr = curr + 1 in
let idx__stat_top_heap_wsz = curr in let curr = curr + 1 in
let idx__stat_compactions = curr in let curr = curr + 1 in
let idx__stat_forced_major_collections = curr in let curr = curr + 1 in
let idx__stat_heap_chunks = curr in let curr = curr + 1 in


let idx__eventlog_startup_timestamp = curr in let curr = curr + 1 in
let idx__eventlog_startup_pid = curr in let curr = curr + 1 in
let idx__eventlog_paused = curr in let curr = curr + 1 in
let idx__eventlog_enabled = curr in let curr = curr + 1 in
let idx__eventlog_out = curr in let curr = curr + 1 in
# 28 "utils/domainstate.ml.c" 2

  let _ = curr in
  function



# 1 "runtime/caml/domain_state.tbl" 1
# 17 "runtime/caml/domain_state.tbl"
| Domain_young_limit -> idx__young_limit
| Domain_young_ptr -> idx__young_ptr


| Domain_exception_pointer -> idx__exception_pointer


| Domain_young_base -> idx__young_base
| Domain_young_start -> idx__young_start
| Domain_young_end -> idx__young_end
| Domain_young_alloc_start -> idx__young_alloc_start
| Domain_young_alloc_end -> idx__young_alloc_end
| Domain_young_alloc_mid -> idx__young_alloc_mid
| Domain_young_trigger -> idx__young_trigger
| Domain_minor_heap_wsz -> idx__minor_heap_wsz
| Domain_in_minor_collection -> idx__in_minor_collection
| Domain_extra_heap_resources_minor -> idx__extra_heap_resources_minor
| Domain_ref_table -> idx__ref_table
| Domain_ephe_ref_table -> idx__ephe_ref_table
| Domain_custom_table -> idx__custom_table


| Domain_mark_stack -> idx__mark_stack


| Domain_stack_low -> idx__stack_low
| Domain_stack_high -> idx__stack_high
| Domain_stack_threshold -> idx__stack_threshold
| Domain_extern_sp -> idx__extern_sp
| Domain_trapsp -> idx__trapsp
| Domain_trap_barrier -> idx__trap_barrier
| Domain_external_raise -> idx__external_raise
| Domain_exn_bucket -> idx__exn_bucket


| Domain_top_of_stack -> idx__top_of_stack
| Domain_bottom_of_stack -> idx__bottom_of_stack
| Domain_last_return_address -> idx__last_return_address
| Domain_gc_regs -> idx__gc_regs


| Domain_backtrace_active -> idx__backtrace_active
| Domain_backtrace_pos -> idx__backtrace_pos
| Domain_backtrace_buffer -> idx__backtrace_buffer
| Domain_backtrace_last_exn -> idx__backtrace_last_exn


| Domain_compare_unordered -> idx__compare_unordered
| Domain_requested_major_slice -> idx__requested_major_slice
| Domain_requested_minor_gc -> idx__requested_minor_gc
| Domain_local_roots -> idx__local_roots

| Domain_stat_minor_words -> idx__stat_minor_words
| Domain_stat_promoted_words -> idx__stat_promoted_words
| Domain_stat_major_words -> idx__stat_major_words
| Domain_stat_minor_collections -> idx__stat_minor_collections
| Domain_stat_major_collections -> idx__stat_major_collections
| Domain_stat_heap_wsz -> idx__stat_heap_wsz
| Domain_stat_top_heap_wsz -> idx__stat_top_heap_wsz
| Domain_stat_compactions -> idx__stat_compactions
| Domain_stat_forced_major_collections -> idx__stat_forced_major_collections
| Domain_stat_heap_chunks -> idx__stat_heap_chunks


| Domain_eventlog_startup_timestamp -> idx__eventlog_startup_timestamp
| Domain_eventlog_startup_pid -> idx__eventlog_startup_pid
| Domain_eventlog_paused -> idx__eventlog_paused
| Domain_eventlog_enabled -> idx__eventlog_enabled
| Domain_eventlog_out -> idx__eventlog_out
# 34 "utils/domainstate.ml.c" 2

