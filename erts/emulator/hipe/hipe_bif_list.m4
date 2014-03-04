/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2014. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
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
 * gc_bif_interface_0(nbif_name, cbif_name)
 * gc_bif_interface_1(nbif_name, cbif_name)
 * gc_bif_interface_2(nbif_name, cbif_name)
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
gc_bif_interface_2(nbif_erts_internal_check_process_code_2, hipe_erts_internal_check_process_code_2)
gc_bif_interface_1(nbif_erase_1, erase_1)
gc_bif_interface_0(nbif_garbage_collect_0, garbage_collect_0)
gc_nofail_primop_interface_1(nbif_gc_1, hipe_gc)
gc_bif_interface_2(nbif_put_2, put_2)

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
standard_bif_interface_3(nbif_find_na_or_make_stub, hipe_find_na_or_make_stub)
standard_bif_interface_2(nbif_nonclosure_address, hipe_nonclosure_address)
nocons_nofail_primop_interface_0(nbif_fclearerror_error, hipe_fclearerror_error)

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
standard_bif_interface_3(nbif_bs_put_utf8, hipe_bs_put_utf8)
standard_bif_interface_3(nbif_bs_put_utf16be, hipe_bs_put_utf16be)
standard_bif_interface_3(nbif_bs_put_utf16le, hipe_bs_put_utf16le)
standard_bif_interface_1(nbif_bs_validate_unicode, hipe_bs_validate_unicode)

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
 * Bit-syntax primops. The ERTS_SMP runtime system requires P,
 * hence the use of nocons_nofail_primop_interface_N().
 * When ERTS_SMP is disabled, noproc_primop_interface_N()
 * should be used instead.
 */
nocons_nofail_primop_interface_5(nbif_bs_put_small_float, hipe_bs_put_small_float)
noproc_primop_interface_5(nbif_bs_put_bits, hipe_bs_put_bits)
ifelse(ERTS_SMP,1,`
nocons_nofail_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)
',`
noproc_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)
')dnl

gc_bif_interface_0(nbif_check_get_msg, hipe_check_get_msg)

#`ifdef' NO_FPE_SIGNALS
nocons_nofail_primop_interface_0(nbif_emulate_fpe, hipe_emulate_fpe)
#endif

/*
 * SMP-specific stuff
 */
ifelse(ERTS_SMP,1,`
nocons_nofail_primop_interface_0(nbif_clear_timeout, hipe_clear_timeout)
noproc_primop_interface_1(nbif_atomic_inc, hipe_atomic_inc)
',)dnl

/*
 * Standard BIFs.
 * BIF_LIST(ModuleAtom,FunctionAtom,Arity,CFun,Index)
 */

/* BIFs that disable GC while trapping are called via a wrapper
 * to reserve stack space for the "trap frame".
 */
define(CFUN,`ifelse($1,term_to_binary_1,hipe_wrapper_term_to_binary_1,
ifelse($1,term_to_binary_2,hipe_wrapper_term_to_binary_2,
ifelse($1,erts_internal_binary_to_term_1,hipe_wrapper_erts_internal_binary_to_term_1,
ifelse($1,erts_internal_binary_to_term_2,hipe_wrapper_erts_internal_binary_to_term_2,
$1))))')

define(BIF_LIST,`standard_bif_interface_$3(nbif_$4, CFUN($4))')
include(TARGET/`erl_bif_list.h')

/*
 * Guard BIFs.
 * GBIF_LIST(FunctionAtom,Arity,CFun)
 */
define(GBIF_LIST,`nofail_primop_interface_$2(gbif_$3, $3)')
include(`hipe/hipe_gbif_list.h')
