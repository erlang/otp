// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_SUPPORT_P_H_INCLUDED
#define ASMJIT_CORE_SUPPORT_P_H_INCLUDED

#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_utilities
//! \{

namespace Support {

//! \cond INTERNAL

static ASMJIT_FORCE_INLINE char decode5BitChar(uint32_t c) noexcept {
  uint32_t base = c <= 26 ? uint32_t('a') - 1u : uint32_t('0') - 27u;
  return char(base + c);
}

static ASMJIT_FORCE_INLINE size_t decodeInstName(char nameOut[32], uint32_t index, const char* stringTable) noexcept {
  size_t i;

  if (index & 0x80000000u) {
    // Small string of 5-bit characters.
    for (i = 0; i < 6; i++, index >>= 5) {
      uint32_t c = index & 0x1F;
      if (c == 0)
        break;
      nameOut[i] = decode5BitChar(c);
    }
    return i;
  }
  else {
    size_t prefixBase = index & 0xFFFu;
    size_t prefixSize = (index >> 12) & 0xFu;

    size_t suffixBase = (index >> 16) & 0xFFFu;
    size_t suffixSize = (index >> 28) & 0x7u;

    for (i = 0; i < prefixSize; i++)
      nameOut[i] = stringTable[prefixBase + i];

    char* suffixOut = nameOut + prefixSize;
    for (i = 0; i < suffixSize; i++)
      suffixOut[i] = stringTable[suffixBase + i];

    return prefixSize + suffixSize;
  }
}

//! \endcond

} // {Support}

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_SUPPORT_P_H_INCLUDED
