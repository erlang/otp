// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_FORMATTER_P_H_INCLUDED
#define ASMJIT_CORE_FORMATTER_P_H_INCLUDED

#include "../core/formatter.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_logging
//! \{

namespace Formatter {

static ASMJIT_FORCE_INLINE size_t paddingFromOptions(const FormatOptions& formatOptions, FormatPaddingGroup group) noexcept {
  static constexpr uint16_t _defaultPaddingTable[uint32_t(FormatPaddingGroup::kMaxValue) + 1] = { 44, 26 };
  static_assert(uint32_t(FormatPaddingGroup::kMaxValue) + 1 == 2, "If a new group is defined it must be added here");

  size_t padding = formatOptions.padding(group);
  return padding ? padding : size_t(_defaultPaddingTable[uint32_t(group)]);
}

} // {Formatter}

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_FORMATTER_H_P_INCLUDED
