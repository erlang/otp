// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_BUILDER_P_H_INCLUDED
#define ASMJIT_CORE_BUILDER_P_H_INCLUDED

#include "../core/api-config.h"
#ifndef ASMJIT_NO_BUILDER

#include "../core/builder.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_builder
//! \{

static inline void BaseBuilder_assignInlineComment(BaseBuilder* self, BaseNode* node, const char* comment) noexcept {
  if (comment)
    node->setInlineComment(static_cast<char*>(self->_dataZone.dup(comment, strlen(comment), true)));
}

static inline void BaseBuilder_assignInstState(BaseBuilder* self, InstNode* node, const BaseEmitter::State& state) noexcept {
  node->setOptions(state.options);
  node->setExtraReg(state.extraReg);
  BaseBuilder_assignInlineComment(self, node, state.comment);
}

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // !ASMJIT_NO_BUILDER
#endif // ASMJIT_CORE_BUILDER_P_H_INCLUDED
