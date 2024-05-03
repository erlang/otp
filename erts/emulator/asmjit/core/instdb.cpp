// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/instdb_p.h"

ASMJIT_BEGIN_NAMESPACE

namespace InstNameUtils {

static constexpr uint32_t kBufferSize = 32;

static ASMJIT_FORCE_INLINE char decode5BitChar(uint32_t c) noexcept {
  uint32_t base = c <= 26 ? uint32_t('a') - 1u : uint32_t('0') - 27u;
  return char(base + c);
}

static ASMJIT_FORCE_INLINE size_t decodeToBuffer(char nameOut[kBufferSize], uint32_t nameValue, const char* stringTable) noexcept {
  size_t i;

  if (nameValue & 0x80000000u) {
    // Small string of 5-bit characters.
    for (i = 0; i < 6; i++, nameValue >>= 5) {
      uint32_t c = nameValue & 0x1F;
      if (c == 0)
        break;
      nameOut[i] = decode5BitChar(c);
    }
    return i;
  }
  else {
    size_t prefixBase = nameValue & 0xFFFu;
    size_t prefixSize = (nameValue >> 12) & 0xFu;

    size_t suffixBase = (nameValue >> 16) & 0xFFFu;
    size_t suffixSize = (nameValue >> 28) & 0x7u;

    for (i = 0; i < prefixSize; i++)
      nameOut[i] = stringTable[prefixBase + i];

    char* suffixOut = nameOut + prefixSize;
    for (i = 0; i < suffixSize; i++)
      suffixOut[i] = stringTable[suffixBase + i];

    return prefixSize + suffixSize;
  }
}

Error decode(String& output, uint32_t nameValue, const char* stringTable) noexcept {
  char nameData[kBufferSize];
  size_t nameSize = decodeToBuffer(nameData, nameValue, stringTable);

  return output.append(nameData, nameSize);
}

InstId find(const char* s, size_t len, const InstNameIndex& nameIndex, const uint32_t* nameTable, const char* stringTable) noexcept {
  if (ASMJIT_UNLIKELY(!s))
    return BaseInst::kIdNone;

  if (len == SIZE_MAX)
    len = strlen(s);

  if (ASMJIT_UNLIKELY(len == 0 || len > nameIndex.maxNameLength))
    return BaseInst::kIdNone;

  uint32_t prefix = uint32_t(s[0]) - 'a';
  if (ASMJIT_UNLIKELY(prefix > 'z' - 'a'))
    return BaseInst::kIdNone;

  size_t base = nameIndex.data[prefix].start;
  size_t end = nameIndex.data[prefix].end;

  if (ASMJIT_UNLIKELY(!base))
    return BaseInst::kIdNone;

  char nameData[kBufferSize];
  for (size_t lim = end - base; lim != 0; lim >>= 1) {
    size_t instId = base + (lim >> 1);
    size_t nameSize = decodeToBuffer(nameData, nameTable[instId], stringTable);

    int result = Support::compareStringViews(s, len, nameData, nameSize);
    if (result < 0)
      continue;

    if (result > 0) {
      base = instId + 1;
      lim--;
      continue;
    }

    return InstId(instId);
  }

  return BaseInst::kIdNone;
}

} // {InstNameUtils}

ASMJIT_END_NAMESPACE
