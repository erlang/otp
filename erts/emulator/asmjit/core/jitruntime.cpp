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
  _cpuFeatures = CpuInfo::host().features();
}

JitRuntime::~JitRuntime() noexcept {}

Error JitRuntime::_add(void** dst, CodeHolder* code) noexcept {
  *dst = nullptr;

  ASMJIT_PROPAGATE(code->flatten());
  ASMJIT_PROPAGATE(code->resolveUnresolvedLinks());

  size_t estimatedCodeSize = code->codeSize();
  if (ASMJIT_UNLIKELY(estimatedCodeSize == 0))
    return DebugUtils::errored(kErrorNoCodeGenerated);

  JitAllocator::Span span;
  ASMJIT_PROPAGATE(_allocator.alloc(span, estimatedCodeSize));

  // Relocate the code.
  Error err = code->relocateToBase(uintptr_t(span.rx()));
  if (ASMJIT_UNLIKELY(err)) {
    _allocator.release(span.rx());
    return err;
  }

  // Recalculate the final code size and shrink the memory we allocated for it
  // in case that some relocations didn't require records in an address table.
  size_t codeSize = code->codeSize();
  ASMJIT_ASSERT(codeSize <= estimatedCodeSize);

  _allocator.write(span, [&](JitAllocator::Span& span) noexcept -> Error {
    uint8_t* rw = static_cast<uint8_t*>(span.rw());

    for (Section* section : code->_sections) {
      size_t offset = size_t(section->offset());
      size_t bufferSize = size_t(section->bufferSize());
      size_t virtualSize = size_t(section->virtualSize());

      ASMJIT_ASSERT(offset + bufferSize <= span.size());
      memcpy(rw + offset, section->data(), bufferSize);

      if (virtualSize > bufferSize) {
        ASMJIT_ASSERT(offset + virtualSize <= span.size());
        memset(rw + offset + bufferSize, 0, virtualSize - bufferSize);
      }
    }

    span.shrink(codeSize);
    return kErrorOk;
  });

  *dst = span.rx();
  return kErrorOk;
}

Error JitRuntime::_release(void* p) noexcept {
  return _allocator.release(p);
}

ASMJIT_END_NAMESPACE

#endif
