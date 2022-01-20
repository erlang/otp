// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#ifndef ASMJIT_NO_JIT

#include "../core/cpuinfo.h"
#include "../core/jitruntime.h"

ASMJIT_BEGIN_NAMESPACE

JitRuntime::JitRuntime(const JitAllocator::CreateParams* params) noexcept
  : _allocator(params) {
  _environment = Environment::host();
  _environment.setObjectFormat(ObjectFormat::kJIT);
}

JitRuntime::~JitRuntime() noexcept {}

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
