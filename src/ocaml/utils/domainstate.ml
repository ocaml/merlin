let stack_ctx_words = (6 + 1)
type t =
| Domain_young_limit
| Domain_young_ptr
| Domain_young_start
| Domain_young_end
| Domain_young_trigger
| Domain_current_stack
| Domain_exn_handler
| Domain_action_pending
| Domain_c_stack
| Domain_stack_cache
| Domain_gc_regs_buckets
| Domain_gc_regs
| Domain_minor_tables
| Domain_mark_stack
| Domain_marking_done
| Domain_sweeping_done
| Domain_allocated_words
| Domain_allocated_words_direct
| Domain_allocated_words_suspended
| Domain_allocated_words_resumed
| Domain_current_ramp_up_allocated_words_diff
| Domain_swept_words
| Domain_gc_policy
| Domain_major_slice_epoch
| Domain_local_roots
| Domain_ephe_info
| Domain_final_info
| Domain_backtrace_pos
| Domain_backtrace_active
| Domain_backtrace_buffer
| Domain_backtrace_last_exn
| Domain_compare_unordered
| Domain_oo_next_id_local
| Domain_requested_major_slice
| Domain_requested_global_major_slice
| Domain_requested_minor_gc
| Domain_requested_external_interrupt
| Domain_parser_trace
| Domain_minor_heap_wsz
| Domain_shared_heap
| Domain_id
| Domain_unique_id
| Domain_dls_root
| Domain_extra_heap_resources
| Domain_extra_heap_resources_minor
| Domain_dependent_size
| Domain_dependent_allocated
| Domain_slice_target
| Domain_slice_budget
| Domain_sweep_work_done_between_slices
| Domain_mark_work_done_between_slices
| Domain_extern_state
| Domain_intern_state
| Domain_stat_minor_words
| Domain_stat_promoted_words
| Domain_stat_major_words
| Domain_stat_forced_major_collections
| Domain_stat_blocks_marked
| Domain_inside_stw_handler
| Domain_trap_sp_off
| Domain_trap_barrier_off
| Domain_trap_barrier_block
| Domain_external_raise
| Domain_memprof
| Domain_memprof_young_trigger
| Domain_extra_params
let idx_of_field =
  let curr = 0 in
let idx__young_limit = curr in let curr = curr + 1 in
let idx__young_ptr = curr in let curr = curr + 1 in
let idx__young_start = curr in let curr = curr + 1 in
let idx__young_end = curr in let curr = curr + 1 in
let idx__young_trigger = curr in let curr = curr + 1 in
let idx__current_stack = curr in let curr = curr + 1 in
let idx__exn_handler = curr in let curr = curr + 1 in
let idx__action_pending = curr in let curr = curr + 1 in
let idx__c_stack = curr in let curr = curr + 1 in
let idx__stack_cache = curr in let curr = curr + 1 in
let idx__gc_regs_buckets = curr in let curr = curr + 1 in
let idx__gc_regs = curr in let curr = curr + 1 in
let idx__minor_tables = curr in let curr = curr + 1 in
let idx__mark_stack = curr in let curr = curr + 1 in
let idx__marking_done = curr in let curr = curr + 1 in
let idx__sweeping_done = curr in let curr = curr + 1 in
let idx__allocated_words = curr in let curr = curr + 1 in
let idx__allocated_words_direct = curr in let curr = curr + 1 in
let idx__allocated_words_suspended = curr in let curr = curr + 1 in
let idx__allocated_words_resumed = curr in let curr = curr + 1 in
let idx__current_ramp_up_allocated_words_diff = curr in let curr = curr + 1 in
let idx__swept_words = curr in let curr = curr + 1 in
let idx__gc_policy = curr in let curr = curr + 1 in
let idx__major_slice_epoch = curr in let curr = curr + 1 in
let idx__local_roots = curr in let curr = curr + 1 in
let idx__ephe_info = curr in let curr = curr + 1 in
let idx__final_info = curr in let curr = curr + 1 in
let idx__backtrace_pos = curr in let curr = curr + 1 in
let idx__backtrace_active = curr in let curr = curr + 1 in
let idx__backtrace_buffer = curr in let curr = curr + 1 in
let idx__backtrace_last_exn = curr in let curr = curr + 1 in
let idx__compare_unordered = curr in let curr = curr + 1 in
let idx__oo_next_id_local = curr in let curr = curr + 1 in
let idx__requested_major_slice = curr in let curr = curr + 1 in
let idx__requested_global_major_slice = curr in let curr = curr + 1 in
let idx__requested_minor_gc = curr in let curr = curr + 1 in
let idx__requested_external_interrupt = curr in let curr = curr + 1 in
let idx__parser_trace = curr in let curr = curr + 1 in
let idx__minor_heap_wsz = curr in let curr = curr + 1 in
let idx__shared_heap = curr in let curr = curr + 1 in
let idx__id = curr in let curr = curr + 1 in
let idx__unique_id = curr in let curr = curr + 1 in
let idx__dls_root = curr in let curr = curr + 1 in
let idx__extra_heap_resources = curr in let curr = curr + 1 in
let idx__extra_heap_resources_minor = curr in let curr = curr + 1 in
let idx__dependent_size = curr in let curr = curr + 1 in
let idx__dependent_allocated = curr in let curr = curr + 1 in
let idx__slice_target = curr in let curr = curr + 1 in
let idx__slice_budget = curr in let curr = curr + 1 in
let idx__sweep_work_done_between_slices = curr in let curr = curr + 1 in
let idx__mark_work_done_between_slices = curr in let curr = curr + 1 in
let idx__extern_state = curr in let curr = curr + 1 in
let idx__intern_state = curr in let curr = curr + 1 in
let idx__stat_minor_words = curr in let curr = curr + 1 in
let idx__stat_promoted_words = curr in let curr = curr + 1 in
let idx__stat_major_words = curr in let curr = curr + 1 in
let idx__stat_forced_major_collections = curr in let curr = curr + 1 in
let idx__stat_blocks_marked = curr in let curr = curr + 1 in
let idx__inside_stw_handler = curr in let curr = curr + 1 in
let idx__trap_sp_off = curr in let curr = curr + 1 in
let idx__trap_barrier_off = curr in let curr = curr + 1 in
let idx__trap_barrier_block = curr in let curr = curr + 1 in
let idx__external_raise = curr in let curr = curr + 1 in
let idx__memprof = curr in let curr = curr + 1 in
let idx__memprof_young_trigger = curr in let curr = curr + 1 in
let idx__extra_params = curr in let curr = curr + 1 in

  let _ = curr in
  function
| Domain_young_limit -> idx__young_limit
| Domain_young_ptr -> idx__young_ptr
| Domain_young_start -> idx__young_start
| Domain_young_end -> idx__young_end
| Domain_young_trigger -> idx__young_trigger
| Domain_current_stack -> idx__current_stack
| Domain_exn_handler -> idx__exn_handler
| Domain_action_pending -> idx__action_pending
| Domain_c_stack -> idx__c_stack
| Domain_stack_cache -> idx__stack_cache
| Domain_gc_regs_buckets -> idx__gc_regs_buckets
| Domain_gc_regs -> idx__gc_regs
| Domain_minor_tables -> idx__minor_tables
| Domain_mark_stack -> idx__mark_stack
| Domain_marking_done -> idx__marking_done
| Domain_sweeping_done -> idx__sweeping_done
| Domain_allocated_words -> idx__allocated_words
| Domain_allocated_words_direct -> idx__allocated_words_direct
| Domain_allocated_words_suspended -> idx__allocated_words_suspended
| Domain_allocated_words_resumed -> idx__allocated_words_resumed
| Domain_current_ramp_up_allocated_words_diff -> idx__current_ramp_up_allocated_words_diff
| Domain_swept_words -> idx__swept_words
| Domain_gc_policy -> idx__gc_policy
| Domain_major_slice_epoch -> idx__major_slice_epoch
| Domain_local_roots -> idx__local_roots
| Domain_ephe_info -> idx__ephe_info
| Domain_final_info -> idx__final_info
| Domain_backtrace_pos -> idx__backtrace_pos
| Domain_backtrace_active -> idx__backtrace_active
| Domain_backtrace_buffer -> idx__backtrace_buffer
| Domain_backtrace_last_exn -> idx__backtrace_last_exn
| Domain_compare_unordered -> idx__compare_unordered
| Domain_oo_next_id_local -> idx__oo_next_id_local
| Domain_requested_major_slice -> idx__requested_major_slice
| Domain_requested_global_major_slice -> idx__requested_global_major_slice
| Domain_requested_minor_gc -> idx__requested_minor_gc
| Domain_requested_external_interrupt -> idx__requested_external_interrupt
| Domain_parser_trace -> idx__parser_trace
| Domain_minor_heap_wsz -> idx__minor_heap_wsz
| Domain_shared_heap -> idx__shared_heap
| Domain_id -> idx__id
| Domain_unique_id -> idx__unique_id
| Domain_dls_root -> idx__dls_root
| Domain_extra_heap_resources -> idx__extra_heap_resources
| Domain_extra_heap_resources_minor -> idx__extra_heap_resources_minor
| Domain_dependent_size -> idx__dependent_size
| Domain_dependent_allocated -> idx__dependent_allocated
| Domain_slice_target -> idx__slice_target
| Domain_slice_budget -> idx__slice_budget
| Domain_sweep_work_done_between_slices -> idx__sweep_work_done_between_slices
| Domain_mark_work_done_between_slices -> idx__mark_work_done_between_slices
| Domain_extern_state -> idx__extern_state
| Domain_intern_state -> idx__intern_state
| Domain_stat_minor_words -> idx__stat_minor_words
| Domain_stat_promoted_words -> idx__stat_promoted_words
| Domain_stat_major_words -> idx__stat_major_words
| Domain_stat_forced_major_collections -> idx__stat_forced_major_collections
| Domain_stat_blocks_marked -> idx__stat_blocks_marked
| Domain_inside_stw_handler -> idx__inside_stw_handler
| Domain_trap_sp_off -> idx__trap_sp_off
| Domain_trap_barrier_off -> idx__trap_barrier_off
| Domain_trap_barrier_block -> idx__trap_barrier_block
| Domain_external_raise -> idx__external_raise
| Domain_memprof -> idx__memprof
| Domain_memprof_young_trigger -> idx__memprof_young_trigger
| Domain_extra_params -> idx__extra_params

