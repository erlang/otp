// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#include "../core/api-build_p.h"
#include "../core/arch.h"

#ifdef ASMJIT_BUILD_X86
  #include "../x86/x86archdata_p.h"
#endif

#ifdef ASMJIT_BUILD_ARM
  #include "../arm/armarchdata_p.h"
#endif

ASMJIT_BEGIN_NAMESPACE

// ============================================================================
// [asmjit::ArchUtils]
// ============================================================================

ASMJIT_FAVOR_SIZE Error ArchUtils::typeIdToRegInfo(uint32_t arch, uint32_t typeId, uint32_t* typeIdOut, RegInfo* regInfoOut) noexcept {
  // Zero the output in case the input is invalid.
  *typeIdOut = 0;
  regInfoOut->reset();

#ifdef ASMJIT_BUILD_X86
  if (Environment::isFamilyX86(arch))
    return x86::ArchInternal::typeIdToRegInfo(arch, typeId, typeIdOut, regInfoOut);
#endif

#ifdef ASMJIT_BUILD_ARM
  if (Environment::isFamilyARM(arch))
    return arm::ArchInternal::typeIdToRegInfo(arch, typeId, typeIdOut, regInfoOut);
#endif

  return DebugUtils::errored(kErrorInvalidArch);
}

ASMJIT_END_NAMESPACE
