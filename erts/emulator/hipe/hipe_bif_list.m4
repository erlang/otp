/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2018. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */
/*
 *
 * List all non architecture-specific BIFs and primops, and
 * classify each as belonging to one of the classes below.
 * This list is included in hipe_${ARCH}_bifs.m4, which is
 * responsible for translating these classifications to the
 * best possible native code wrappers.
 *
 * XXX: We should have a more detailed BIF classification
 * with a number of orthogonal properties (e.g., UPDATES_HP,
 * NEEDS_NSP, CAN_FAIL, CAN_GC, etc), from which we should
 * generate appropriate interfaces.
 *
 * The classification is expressed in terms of the resources
 * and BIF failure modes described below.
 *
 * Resources:
 * - NSP: native stack pointer
 *   NSP is read by GC BIFs and primops, and hipe_handle_exception().
 *   NSP is updated at compiler-inserted calls to hipe_inc_nstack().
 *   No other BIF or primop may access NSP.
 * - NSP_LIMIT: native stack limit
 *   NSP_LIMIT is only updated at compiler-inserted calls to inc_stack.
 *   Everywhere else, the cached value equals the value stored in P.
 * - NRA: native return address
 *   NRA is read by GC BIFs and primops, and hipe_handle_exception().
 *   No BIF or primop may update NRA.
 * - HP: heap pointer
 *   All BIFs can read and update HP.
 *   Primops with access to P that do not access HP are called "nocons".
 * - HP_LIMIT: heap limit
 *   HP_LIMIT is only updated by GC BIFs and primops.
 *   Everywhere else, the cached value equals the value stored in P.
 * - FCALLS: reduction counter
 *   All BIFs can read and update FCALLS (because BEAM abuses FCALLS
 *   to trigger GCs). XXX: can we avoid that overhead?
 *   All nocons primops do not access FCALLS.
 *   All other primops with access to P can read and update FCALLS.
 * - P: pointer to the state record for the process
 *
 * BIF failure modes:
 * - none: may not signal any exception
 *   The BIF wrapper needs no checks before returning.
 * - standard: may signal any exception
 *   The BIF wrapper must check for an exception before returning.
 *   Zero-arity BIFs signal no exceptions, except in a small number
 *   of cases explicitly enumerated here.
 */

/****************************************************************
 *			BIF CLASS DESCRIPTIONS			*
 ****************************************************************/

/*
 * NOTE:
 *  Beam BIFs have the prototype:
 *    Eterm (*BIF)(Process *c_p, Eterm *regs, UWord *I)
 *  Native BIFs have the prototype:
 *    Eterm (*BIF)(Process *c_p, Eterm *regs)
 *
 * Beam BIFs expect 'I' to contain current instruction
 * pointer when called from beam, and expect 'I' to
 * contain a pointer to the export entry of the BIF
 * when called from native code. In order to facilitate
 * this, beam BIFs are called via wrapper functions
 * when called from native code. These wrapper functions
 * are auto-generated (by utils/make_tables) and have
 * the function names nbif_impl_<BIF>.
 *
 * The standard_bif_interface_*() and
 * gc_bif_interface_*() will add the prefix and
 * thus call nbif_impl_<cbif_name>. That is, all
 * functions (true BIFs as well as other c-functions)
 * called via these interfaces have to be named
 * nbif_impl_<FUNC>.
 */

/*
 * See NOTE above!
 *
 * standard_bif_interface_0(nbif_name, cbif_name)
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * A BIF with implicit P parameter, 0-3 ordinary parameters,
 * which may fail.
 * HP and FCALLS may be read and updated.
 * HP_LIMIT, NSP, NSP_LIMIT, and NRA may not be accessed.
 */

/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 * nofail_primop_interface_3(nbif_name, cbif_name)
 *
 * A primop or guard BIF with no failure mode, otherwise
 * identical to standard_bif_interface_N.
 */

/*
 * See NOTE above!
 *
 * gc_bif_interface_0(nbif_name, cbif_name)
 * gc_bif_interface_1(nbif_name, cbif_name)
 * gc_bif_interface_2(nbif_name, cbif_name)
 * gc_bif_interface_3(nbif_name, cbif_name)
 *
 * A BIF which may do a GC or walk the native stack.
 * May read NSP, NSP_LIMIT, NRA, HP, HP_LIMIT, and FCALLS.
 * May update HP, HP_LIMIT, and FCALLS.
 * May not update NSP, NSP_LIMIT, or NRA.
 * Otherwise identical to standard_bif_interface_N.
 */

/*
 * gc_nofail_primop_interface_1(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 1 ordinary parameter,
 * and no failure mode.
 * May read NSP, NSP_LIMIT, NRA, HP, HP_LIMIT, and FCALLS.
 * May update HP, HP_LIMIT, and FCALLS.
 * May not update NSP, NSP_LIMIT, or NRA.
 */

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 0-3 or 5 ordinary parameters,
 * and no failure mode.
 * HP, HP_LIMIT, FCALLS, NSP, NSP_LIMIT, and NRA may not be accessed.
 */

/*
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * A primop with no P parameter, 0-3 or 5 ordinary parameters,
 * and no failure mode.
 * HP, HP_LIMIT, FCALLS, NSP, NSP_LIMIT, and NRA may not be accessed.
 */

/****************************************************************
 *			BIF CLASSIFICATION			*
 ****************************************************************/

/*
 * Zero-arity BIFs that can fail.
 */
standard_bif_interface_0(nbif_processes_0, processes_0)
standard_bif_interface_0(nbif_ports_0, ports_0)

/*
 * BIFs and primops that may do a GC (change heap limit and walk the native stack).
 * XXX: erase/1 and put/2 cannot fail
 */
gc_bif_interface_1(nbif_erts_internal_check_process_code_1, hipe_erts_internal_check_process_code_1)
gc_bif_interface_1(nbif_erase_1, erase_1)
gc_bif_interface_1(nbif_erts_internal_garbage_collect_1, erts_internal_garbage_collect_1)
gc_nofail_primop_interface_1(nbif_gc_1, hipe_gc)
gc_bif_interface_2(nbif_put_2, put_2)
gc_bif_interface_2(nbif_hipe_bifs_build_stacktrace, hipe_bifs_build_stacktrace_1)

/*
 * Debug BIFs that need read access to the full state.
 * hipe_bifs:nstack_used_size/0 only needs read access to NSP.
 * They are classified as GC BIFs for simplicity.
 */
gc_bif_interface_1(nbif_hipe_bifs_show_nstack_1, hipe_show_nstack_1)
gc_bif_interface_1(nbif_hipe_bifs_show_pcb_1, hipe_bifs_show_pcb_1)
gc_bif_interface_0(nbif_hipe_bifs_nstack_used_size_0, hipe_bifs_nstack_used_size_0)
gc_bif_interface_2(nbif_hipe_bifs_debug_native_called, hipe_bifs_debug_native_called_2)

/*
 * Arithmetic operators called indirectly by the HiPE compiler.
 */
standard_bif_interface_2(nbif_add_2, splus_2)
standard_bif_interface_2(nbif_sub_2, sminus_2)
standard_bif_interface_2(nbif_mul_2, stimes_2)
standard_bif_interface_2(nbif_div_2, div_2)
standard_bif_interface_2(nbif_intdiv_2, intdiv_2)
standard_bif_interface_2(nbif_rem_2, rem_2)
standard_bif_interface_2(nbif_bsl_2, bsl_2)
standard_bif_interface_2(nbif_bsr_2, bsr_2)
standard_bif_interface_2(nbif_band_2, band_2)
standard_bif_interface_2(nbif_bor_2, bor_2)
standard_bif_interface_2(nbif_bxor_2, bxor_2)
standard_bif_interface_1(nbif_bnot_1, bnot_1)

/*
 * Miscellaneous primops.
 */
standard_bif_interface_1(nbif_set_timeout, hipe_set_timeout)
standard_bif_interface_1(nbif_conv_big_to_float, hipe_conv_big_to_float)
standard_bif_interface_2(nbif_rethrow, hipe_rethrow)
standard_bif_interface_3(nbif_raw_raise, hipe_raw_raise)
standard_bif_interface_3(nbif_find_na_or_make_stub, hipe_find_na_or_make_stub)
standard_bif_interface_2(nbif_nonclosure_address, hipe_nonclosure_address)
nocons_nofail_primop_interface_0(nbif_fclearerror_error, hipe_fclearerror_error)
noproc_primop_interface_2(nbif_is_divisible, hipe_is_divisible)
noproc_primop_interface_1(nbif_is_unicode, hipe_is_unicode)

/*
 * Mbox primops with implicit P parameter.
 */
nocons_nofail_primop_interface_0(nbif_select_msg, hipe_select_msg)

/*
 * Primops without any P parameter.
 * These cannot CONS or gc.
 */
noproc_primop_interface_2(nbif_cmp_2, cmp)
noproc_primop_interface_2(nbif_eq_2, eq)

/*
 * Bit-syntax primops with implicit P parameter.
 * XXX: all of the _2 versions cons on the ordinary heap
 * XXX: all of them can cons and thus update FCALLS
 */
nofail_primop_interface_3(nbif_bs_get_integer_2, erts_bs_get_integer_2)
nofail_primop_interface_3(nbif_bs_get_binary_2, erts_bs_get_binary_2)
nofail_primop_interface_3(nbif_bs_get_float_2, erts_bs_get_float_2)
nocons_nofail_primop_interface_3(nbif_bs_put_utf8, hipe_bs_put_utf8)
standard_bif_interface_3(nbif_bs_put_utf16be, hipe_bs_put_utf16be)
standard_bif_interface_3(nbif_bs_put_utf16le, hipe_bs_put_utf16le)

/*
 * Bit-syntax primops without any P parameter.
 * These cannot CONS or gc.
 */
noproc_primop_interface_1(nbif_bs_allocate, hipe_bs_allocate)
noproc_primop_interface_2(nbif_bs_reallocate, hipe_bs_reallocate)
noproc_primop_interface_1(nbif_bs_utf8_size, hipe_bs_utf8_size)
noproc_primop_interface_1(nbif_bs_get_utf8, erts_bs_get_utf8)
noproc_primop_interface_1(nbif_bs_utf16_size, hipe_bs_utf16_size)
noproc_primop_interface_2(nbif_bs_get_utf16, erts_bs_get_utf16)
noproc_primop_interface_2(nbif_bs_validate_unicode_retract, hipe_bs_validate_unicode_retract)

/*
 * Bit-syntax primops. The runtime system requires P,
 * hence the use of nocons_nofail_primop_interface_N().
 */
nocons_nofail_primop_interface_5(nbif_bs_put_small_float, hipe_bs_put_small_float)
noproc_primop_interface_5(nbif_bs_put_bits, hipe_bs_put_bits)
nocons_nofail_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)

nofail_primop_interface_0(nbif_check_get_msg, hipe_check_get_msg)

#`ifdef' NO_FPE_SIGNALS
nocons_nofail_primop_interface_0(nbif_emulate_fpe, hipe_emulate_fpe)
#endif

noproc_primop_interface_1(nbif_emasculate_binary, hipe_emasculate_binary)

nocons_nofail_primop_interface_0(nbif_clear_timeout, hipe_clear_timeout)
noproc_primop_interface_1(nbif_atomic_inc, hipe_atomic_inc)

/*
 * BIFs that disable GC while trapping are called via a wrapper
 * to reserve stack space for the "trap frame".
 * They occasionally need to call the garbage collector in order to make room
 * for the trap frame on the BEAM stack.
 */
gc_bif_interface_1(nbif_term_to_binary_1, hipe_wrapper_term_to_binary_1)
gc_bif_interface_2(nbif_term_to_binary_2, hipe_wrapper_term_to_binary_2)
gc_bif_interface_1(nbif_binary_to_term_1, hipe_wrapper_binary_to_term_1)
gc_bif_interface_2(nbif_binary_to_term_2, hipe_wrapper_binary_to_term_2)
gc_bif_interface_1(nbif_binary_to_list_1, hipe_wrapper_binary_to_list_1)
gc_bif_interface_3(nbif_binary_to_list_3, hipe_wrapper_binary_to_list_3)
gc_bif_interface_1(nbif_bitstring_to_list_1, hipe_wrapper_bitstring_to_list_1)
gc_bif_interface_1(nbif_list_to_binary_1, hipe_wrapper_list_to_binary_1)
gc_bif_interface_1(nbif_iolist_to_binary_1, hipe_wrapper_iolist_to_binary_1)
gc_bif_interface_1(nbif_binary_list_to_bin_1, hipe_wrapper_binary_list_to_bin_1)
gc_bif_interface_1(nbif_list_to_bitstring_1, hipe_wrapper_list_to_bitstring_1)
gc_bif_interface_2(nbif_send_2, hipe_wrapper_send_2)
gc_bif_interface_3(nbif_send_3, hipe_wrapper_send_3)
gc_bif_interface_2(nbif_ebif_bang_2, hipe_wrapper_ebif_bang_2)
gc_bif_interface_2(nbif_maps_merge_2, hipe_wrapper_maps_merge_2)


/*
 * Standard BIFs.
 * BIF_LIST(ModuleAtom,FunctionAtom,Arity,CFun,Index)
 */

define(BIF_LIST,`standard_bif_interface_$3(nbif_$5, $5)')
include(TTF_DIR/`erl_bif_list.h')

/*
 * Guard BIFs.
 * GBIF_LIST(FunctionAtom,Arity,CFun)
 */
define(GBIF_LIST,`nofail_primop_interface_$2(gbif_$3, $3)')
include(`hipe/hipe_gbif_list.h')
