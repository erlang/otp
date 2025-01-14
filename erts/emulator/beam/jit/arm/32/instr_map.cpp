/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2023. All Rights Reserved.
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
#include <algorithm>
#include "beam_asm.hpp"

using namespace asmjit;

extern "C"
{
#include "erl_map.h"
#include "erl_term_hashing.h"
#include "beam_common.h"
}

/* ARG2 = term
 *
 * Helper for calculating the internal hash of keys before looking them up in a
 * map. This is a manual expansion of `erts_internal_hash`, and all changes to
 * that function must be mirrored here.
 *
 * Result in ARG3. Clobbers TMP1. */
void BeamGlobalAssembler::emit_internal_hash_helper() {
    // TODO
    ASSERT(false);
}

/* ARG1 = untagged hash map root
 * ARG2 = key
 * ARG3 = key hash
 * ARG4 = node header
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_hashmap_get_element() {
    // TODO
    ASSERT(false);
}

/* ARG1 = untagged flat map
 * ARG2 = key
 * ARG5 = size
 *
 * Result is returned in ARG1. ZF is set on success. */
void BeamGlobalAssembler::emit_flatmap_get_element() {
    // TODO
    ASSERT(false);
}

void BeamGlobalAssembler::emit_new_map_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_new_map(const ArgRegister &Dst,
                                       const ArgWord &Live,
                                       const ArgWord &Size,
                                       const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_new_small_map_lit(const ArgRegister &Dst,
                                                   const ArgWord &Live,
                                                   const ArgLiteral &Keys,
                                                   const ArgWord &Size,
                                                   const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

/* ARG1 = map
 * ARG2 = key
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_get_map_element(const ArgLabel &Fail,
                                                 const ArgRegister &Src,
                                                 const ArgRegister &Key,
                                                 const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_get_map_elements(const ArgLabel &Fail,
                                                  const ArgSource &Src,
                                                  const ArgWord &Size,
                                                  const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

/* ARG1 = map
 * ARG2 = key
 * ARG3 = key hash
 *
 * Result is returned in RET. ZF is set on success. */
void BeamGlobalAssembler::emit_i_get_map_element_hash_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_i_get_map_element_hash(const ArgLabel &Fail,
                                                      const ArgRegister &Src,
                                                      const ArgConstant &Key,
                                                      const ArgWord &Hx,
                                                      const ArgRegister &Dst) {
    // TODO
    ASSERT(false);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector. */
void BeamGlobalAssembler::emit_update_map_assoc_shared() {
    // TODO
    ASSERT(false);
}

/* ARG2 = key
 * ARG3 = value
 * ARG4 = map
 */
void BeamGlobalAssembler::emit_update_map_single_assoc_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_update_map_assoc(const ArgSource &Src,
                                                const ArgRegister &Dst,
                                                const ArgWord &Live,
                                                const ArgWord &Size,
                                                const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Result is returned in RET, error is indicated by ZF. */
void BeamGlobalAssembler::emit_update_map_exact_guard_shared() {
    // TODO
    ASSERT(false);
}

/* ARG3 = live registers, ARG4 = update vector size, ARG5 = update vector.
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_exact_body_shared() {
    // TODO
    ASSERT(false);
}

/* ARG2 = key
 * ARG3 = value
 * ARG4 = map
 *
 * Does not return on error. */
void BeamGlobalAssembler::emit_update_map_single_exact_body_shared() {
    // TODO
    ASSERT(false);
}

void BeamModuleAssembler::emit_update_map_exact(const ArgSource &Src,
                                                const ArgLabel &Fail,
                                                const ArgRegister &Dst,
                                                const ArgWord &Live,
                                                const ArgWord &Size,
                                                const Span<ArgVal> &args) {
    // TODO
    ASSERT(false);
}
