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
#ifndef ASMJIT_NO_LOGGING

#include "../core/logger.h"
#include "../core/string.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

// ============================================================================
// [asmjit::Logger - Construction / Destruction]
// ============================================================================

Logger::Logger() noexcept
  : _options() {}
Logger::~Logger() noexcept {}

// ============================================================================
// [asmjit::Logger - Logging]
// ============================================================================

Error Logger::logf(const char* fmt, ...) noexcept {
  Error err;
  va_list ap;

  va_start(ap, fmt);
  err = logv(fmt, ap);
  va_end(ap);

  return err;
}

Error Logger::logv(const char* fmt, va_list ap) noexcept {
  StringTmp<2048> sb;
  ASMJIT_PROPAGATE(sb.appendVFormat(fmt, ap));
  return log(sb);
}

Error Logger::logBinary(const void* data, size_t size) noexcept {
  static const char prefix[] = "db ";

  StringTmp<256> sb;
  sb.append(prefix, ASMJIT_ARRAY_SIZE(prefix) - 1);

  size_t i = size;
  const uint8_t* s = static_cast<const uint8_t*>(data);

  while (i) {
    uint32_t n = uint32_t(Support::min<size_t>(i, 16));
    sb.truncate(ASMJIT_ARRAY_SIZE(prefix) - 1);
    sb.appendHex(s, n);
    sb.append('\n');
    ASMJIT_PROPAGATE(log(sb));
    s += n;
    i -= n;
  }

  return kErrorOk;
}

// ============================================================================
// [asmjit::FileLogger - Construction / Destruction]
// ============================================================================

FileLogger::FileLogger(FILE* file) noexcept
  : _file(file) {}
FileLogger::~FileLogger() noexcept {}

// ============================================================================
// [asmjit::FileLogger - Logging]
// ============================================================================

Error FileLogger::_log(const char* data, size_t size) noexcept {
  if (!_file)
    return kErrorOk;

  if (size == SIZE_MAX)
    size = strlen(data);

  fwrite(data, 1, size, _file);
  return kErrorOk;
}

// ============================================================================
// [asmjit::StringLogger - Construction / Destruction]
// ============================================================================

StringLogger::StringLogger() noexcept {}
StringLogger::~StringLogger() noexcept {}

// ============================================================================
// [asmjit::StringLogger - Logging]
// ============================================================================

Error StringLogger::_log(const char* data, size_t size) noexcept {
  return _content.append(data, size);
}

ASMJIT_END_NAMESPACE

#endif
