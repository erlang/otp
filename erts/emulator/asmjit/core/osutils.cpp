// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/osutils_p.h"
#include "../core/support.h"

#if !defined(_WIN32)
  #include <fcntl.h>
  #include <unistd.h>
#endif

ASMJIT_BEGIN_NAMESPACE

#if !defined(_WIN32)
Error OSUtils::readFile(const char* name, String& dst, size_t maxSize) noexcept {
  char* buffer = dst.prepare(String::ModifyOp::kAssign, maxSize);
  if (ASMJIT_UNLIKELY(!buffer))
    return DebugUtils::errored(kErrorOutOfMemory);

  int fd = ASMJIT_FILE64_API(::open)(name, O_RDONLY);
  if (fd < 0) {
    dst.clear();
    return DebugUtils::errored(kErrorFailedToOpenFile);
  }

  intptr_t len = ::read(fd, buffer, maxSize);
  if (len >= 0) {
    buffer[len] = '\0';
    dst._setSize(size_t(len));
  }

  ::close(fd);
  return kErrorOk;
}
#endif

ASMJIT_END_NAMESPACE
