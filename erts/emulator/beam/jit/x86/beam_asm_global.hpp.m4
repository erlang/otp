m4_include(gen_warn.m4)GEN_WARN
m4_changecom(`/*', `*/')m4_dnl
/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2021. All Rights Reserved.
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

#ifndef _BEAM_ASM_GLOBAL_HPP
#define _BEAM_ASM_GLOBAL_HPP

m4_dnl Please keep this in alphabetical order.
m4_define(`BEAM_GLOBAL_FUNCS',
$1(apply_fun_shared)
$1(arith_compare_shared)
$1(arith_eq_shared)
$1(bif_nif_epilogue)
$1(bif_element_shared)
$1(bif_export_trap)
$1(bs_add_shared)
$1(bs_create_bin_error_shared)
$1(bs_size_check_shared)
$1(bs_fixed_integer_shared)
$1(bs_get_tail_shared)
$1(call_bif_shared)
$1(call_light_bif_shared)
$1(call_nif_early)
$1(call_nif_shared)
$1(call_nif_yield_helper)
$1(catch_end_shared)
$1(check_float_error)
$1(dispatch_bif)
$1(dispatch_nif)
$1(dispatch_return)
$1(dispatch_save_calls)
$1(export_trampoline)
$1(garbage_collect)
$1(generic_bp_global)
$1(generic_bp_local)
$1(debug_bp)
$1(fconv_shared)
$1(handle_call_fun_error)
$1(handle_element_error)
$1(handle_hd_error)
$1(i_band_body_shared)
$1(i_band_guard_shared)
$1(i_bif_body_shared)
$1(i_bif_guard_shared)
$1(i_bor_body_shared)
$1(i_bor_guard_shared)
$1(i_bnot_body_shared)
$1(i_bnot_guard_shared)
$1(i_bsl_guard_shared)
$1(i_bsl_body_shared)
$1(i_bsr_guard_shared)
$1(i_bsr_body_shared)
$1(i_bxor_body_shared)
$1(i_bxor_guard_shared)
$1(i_func_info_shared)
$1(i_load_nif_shared)
$1(i_length_guard_shared)
$1(i_length_body_shared)
$1(i_loop_rec_shared)
$1(i_new_small_map_lit_shared)
$1(i_test_yield_shared)
$1(increment_body_shared)
$1(int_div_rem_body_shared)
$1(int_div_rem_guard_shared)
$1(minus_body_shared)
$1(minus_guard_shared)
$1(new_map_shared)
$1(plus_body_shared)
$1(plus_guard_shared)
$1(process_exit)
$1(process_main)
$1(raise_exception)
$1(raise_exception_shared)
$1(times_body_shared)
$1(times_guard_shared)
$1(unary_minus_body_shared)
$1(unary_minus_guard_shared)
$1(unloaded_fun)
$1(update_map_assoc_shared)
$1(update_map_exact_guard_shared)
$1(update_map_exact_body_shared)
)

m4_dnl Labels exported from within process_main
m4_define(`PROCESS_MAIN_LABELS',
$1(context_switch)
$1(context_switch_simplified)
$1(do_schedule)
)

class BeamGlobalAssembler : public BeamAssembler {
    typedef void (BeamGlobalAssembler::*emitFptr)(void);
    typedef void (*fptr)(void);

    enum GlobalLabels : uint32_t {
m4_define(`DECL_ENUM', `        $1,')
BEAM_GLOBAL_FUNCS(`DECL_ENUM')
PROCESS_MAIN_LABELS(`DECL_ENUM')
    };

    static const std::map<GlobalLabels, const std::string> labelNames;
    static const std::map<GlobalLabels, emitFptr> emitPtrs;
    std::unordered_map<GlobalLabels, Label> labels;
    std::unordered_map<GlobalLabels, fptr> ptrs;

m4_define(`DECL_FUNC', `    void emit_$1(void);')
BEAM_GLOBAL_FUNCS(`DECL_FUNC')

    template<typename T>
    void emit_bitwise_fallback_body(T(*func_ptr), const ErtsCodeMFA *mfa);

    template<typename T>
    void emit_bitwise_fallback_guard(T(*func_ptr));

    x86::Mem emit_i_length_common(Label fail, int state_size);

public:
    BeamGlobalAssembler(JitAllocator *allocator);

    void (*get(GlobalLabels lbl))(void) {
        ASSERT(ptrs[lbl]);
        return ptrs[lbl];
    }

m4_define(`GET_CODE',`    void (*get_$1(void))() { return get($1); }')
BEAM_GLOBAL_FUNCS(`GET_CODE')
PROCESS_MAIN_LABELS(`GET_CODE')
};

#ifdef ERTS_BEAM_ASM_GLOBAL_WANT_STATIC_DEFS

m4_define(`DECL_EMIT',`    {$1, &BeamGlobalAssembler::emit_$1},')
const std::map<BeamGlobalAssembler::GlobalLabels, BeamGlobalAssembler::emitFptr>
BeamGlobalAssembler::emitPtrs = {
BEAM_GLOBAL_FUNCS(`DECL_EMIT')
};

m4_define(`DECL_LABEL_NAME',`    {$1, "$1"},')
const std::map<BeamGlobalAssembler::GlobalLabels, const std::string>
BeamGlobalAssembler::labelNames = {
BEAM_GLOBAL_FUNCS(`DECL_LABEL_NAME')
PROCESS_MAIN_LABELS(`DECL_LABEL_NAME')
};

#endif /* ERTS_BEAM_ASM_GLOBAL_WANT_STATIC_DEFS */

#endif /* !_BEAM_ASM_GLOBAL_HPP */
