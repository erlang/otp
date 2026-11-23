// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_STRING_INDEX_H_INCLUDED
#define ASMJIT_AXL_STRING_INDEX_H_INCLUDED

#include <asmjit/axl/commons.h>
#include <asmjit/axl/inplace_array.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

template<size_t IndexN, size_t StrN, typename IndexT = std_uint_t<(StrN < 256) ? 1 : (StrN < 65535) ? 2 : 4>>
static consteval InplaceArray<IndexT, IndexN> make_string_index(const char(&str)[StrN]) noexcept {
  InplaceArray<IndexT, IndexN> out;
  for (size_t idx_i = 0, str_i = 0; idx_i < IndexN; idx_i++) {
    out[idx_i] = IndexT(str_i);
    while (str[str_i++] != '\0') {
      continue;
    }
  }
  return out;
}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_STRING_INDEX_H_INCLUDED