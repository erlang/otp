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
#include "../core/func.h"
#include "../core/type.h"

#ifdef ASMJIT_BUILD_X86
  #include "../x86/x86callconv_p.h"
#endif

#ifdef ASMJIT_BUILD_ARM
  #include "../arm/armcallconv_p.h"
#endif

ASMJIT_BEGIN_NAMESPACE

// ============================================================================
// [asmjit::CallConv - Init / Reset]
// ============================================================================

ASMJIT_FAVOR_SIZE Error CallConv::init(uint32_t ccId, const Environment& environment) noexcept {
  reset();

#ifdef ASMJIT_BUILD_X86
  if (environment.isFamilyX86())
    return x86::CallConvInternal::init(*this, ccId, environment);
#endif

#ifdef ASMJIT_BUILD_ARM
  if (environment.isFamilyARM())
    return arm::CallConvInternal::init(*this, ccIdv, environment);
#endif

  return DebugUtils::errored(kErrorInvalidArgument);
}

ASMJIT_END_NAMESPACE
