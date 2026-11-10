// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INTEGRATION_H_INCLUDED
#define ASMJIT_AXL_INTEGRATION_H_INCLUDED

// This file acts as an integration that provides the following:
//
// <Result> type:
//   - Result (could be using a Result type from upper namespace if compatible)
//   - kSuccess - a successful result (usually a zero constant)
//   - make_out_of_memory() - makes an out of memory result

#include <asmjit/core/build_defs.h>
#include <asmjit/core/debug_utils.h>
#include <asmjit/core/error.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

using Result = Error;

static constexpr Result kSuccess = Result::kOk;

[[nodiscard]]
static constexpr Result make_out_of_memory() noexcept { return make_error(Error::kOutOfMemory); }

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INTEGRATION_H_INCLUDED
