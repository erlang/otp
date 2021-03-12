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
#ifndef ASMJIT_NO_JIT

#include "../core/cpuinfo.h"
#include "../core/jitruntime.h"

ASMJIT_BEGIN_NAMESPACE

// ============================================================================
// [asmjit::JitRuntime - Construction / Destruction]
// ============================================================================

JitRuntime::JitRuntime(const JitAllocator::CreateParams* params) noexcept
  : _allocator(params) {
  _environment = hostEnvironment();
  _environment.setFormat(Environment::kFormatJIT);
}

JitRuntime::~JitRuntime() noexcept {}

// ============================================================================
// [asmjit::JitRuntime - Interface]
// ============================================================================

Error JitRuntime::_add(void** dst, CodeHolder* code) noexcept {
  *dst = nullptr;

  ASMJIT_PROPAGATE(code->flatten());
  ASMJIT_PROPAGATE(code->resolveUnresolvedLinks());

  size_t estimatedCodeSize = code->codeSize();
  if (ASMJIT_UNLIKELY(estimatedCodeSize == 0))
    return DebugUtils::errored(kErrorNoCodeGenerated);

  uint8_t* rx;
  uint8_t* rw;
  ASMJIT_PROPAGATE(_allocator.alloc((void**)&rx, (void**)&rw, estimatedCodeSize));

  // Relocate the code.
  Error err = code->relocateToBase(uintptr_t((void*)rx));
  if (ASMJIT_UNLIKELY(err)) {
    _allocator.release(rx);
    return err;
  }

  // Recalculate the final code size and shrink the memory we allocated for it
  // in case that some relocations didn't require records in an address table.
  size_t codeSize = code->codeSize();
  if (codeSize < estimatedCodeSize)
    _allocator.shrink(rx, codeSize);

  {
    VirtMem::ProtectJitReadWriteScope rwScope(rx, codeSize);

    for (Section* section : code->_sections) {
      size_t offset = size_t(section->offset());
      size_t bufferSize = size_t(section->bufferSize());
      size_t virtualSize = size_t(section->virtualSize());

      ASMJIT_ASSERT(offset + bufferSize <= codeSize);
      memcpy(rw + offset, section->data(), bufferSize);

      if (virtualSize > bufferSize) {
        ASMJIT_ASSERT(offset + virtualSize <= codeSize);
        memset(rw + offset + bufferSize, 0, virtualSize - bufferSize);
      }
    }
  }

  *dst = rx;
  return kErrorOk;
}

Error JitRuntime::_release(void* p) noexcept {
  return _allocator.release(p);
}

ASMJIT_END_NAMESPACE

#endif
