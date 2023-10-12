// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_CODEBUFFERWRITER_P_H_INCLUDED
#define ASMJIT_CORE_CODEBUFFERWRITER_P_H_INCLUDED

#include "../core/assembler.h"
#include "../core/codebuffer.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \cond INTERNAL
//! \addtogroup asmjit_assembler
//! \{

struct OffsetFormat;

//! Helper that is used to write into a \ref CodeBuffer held by \ref BaseAssembler.
class CodeWriter {
public:
  uint8_t* _cursor;

  ASMJIT_FORCE_INLINE explicit CodeWriter(BaseAssembler* a) noexcept
    : _cursor(a->_bufferPtr) {}

  ASMJIT_FORCE_INLINE Error ensureSpace(BaseAssembler* a, size_t n) noexcept {
    size_t remainingSpace = (size_t)(a->_bufferEnd - _cursor);
    if (ASMJIT_UNLIKELY(remainingSpace < n)) {
      CodeBuffer& buffer = a->_section->_buffer;
      Error err = a->_code->growBuffer(&buffer, n);
      if (ASMJIT_UNLIKELY(err))
        return a->reportError(err);
      _cursor = a->_bufferPtr;
    }
    return kErrorOk;
  }

  ASMJIT_FORCE_INLINE uint8_t* cursor() const noexcept { return _cursor; }
  ASMJIT_FORCE_INLINE void setCursor(uint8_t* cursor) noexcept { _cursor = cursor; }
  ASMJIT_FORCE_INLINE void advance(size_t n) noexcept { _cursor += n; }

  ASMJIT_FORCE_INLINE size_t offsetFrom(uint8_t* from) const noexcept {
    ASMJIT_ASSERT(_cursor >= from);
    return (size_t)(_cursor - from);
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emit8(T val) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    _cursor[0] = uint8_t(U(val) & U(0xFF));
    _cursor++;
  }

  template<typename T, typename Y>
  ASMJIT_FORCE_INLINE void emit8If(T val, Y cond) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    ASMJIT_ASSERT(size_t(cond) <= 1u);

    _cursor[0] = uint8_t(U(val) & U(0xFF));
    _cursor += size_t(cond);
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emit16uLE(T val) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    Support::writeU16uLE(_cursor, uint16_t(U(val) & 0xFFFFu));
    _cursor += 2;
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emit16uBE(T val) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    Support::writeU16uBE(_cursor, uint16_t(U(val) & 0xFFFFu));
    _cursor += 2;
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emit32uLE(T val) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    Support::writeU32uLE(_cursor, uint32_t(U(val) & 0xFFFFFFFFu));
    _cursor += 4;
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emit32uBE(T val) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    Support::writeU32uBE(_cursor, uint32_t(U(val) & 0xFFFFFFFFu));
    _cursor += 4;
  }

  ASMJIT_FORCE_INLINE void emitData(const void* data, size_t size) noexcept {
    ASMJIT_ASSERT(size != 0);
    memcpy(_cursor, data, size);
    _cursor += size;
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emitValueLE(const T& value, size_t size) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    ASMJIT_ASSERT(size <= sizeof(T));

    U v = U(value);
    for (uint32_t i = 0; i < size; i++) {
      _cursor[i] = uint8_t(v & 0xFFu);
      v >>= 8;
    }
    _cursor += size;
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void emitValueBE(const T& value, size_t size) noexcept {
    typedef typename std::make_unsigned<T>::type U;
    ASMJIT_ASSERT(size <= sizeof(T));

    U v = U(value);
    for (uint32_t i = 0; i < size; i++) {
      _cursor[i] = uint8_t(v >> (sizeof(T) - 8));
      v <<= 8;
    }
    _cursor += size;
  }

  ASMJIT_FORCE_INLINE void emitZeros(size_t size) noexcept {
    ASMJIT_ASSERT(size != 0);
    memset(_cursor, 0, size);
    _cursor += size;
  }

  ASMJIT_FORCE_INLINE void remove8(uint8_t* where) noexcept {
    ASMJIT_ASSERT(where < _cursor);

    uint8_t* p = where;
    while (++p != _cursor)
      p[-1] = p[0];
    _cursor--;
  }

  template<typename T>
  ASMJIT_FORCE_INLINE void insert8(uint8_t* where, T val) noexcept {
    uint8_t* p = _cursor;

    while (p != where) {
      p[0] = p[-1];
      p--;
    }

    *p = uint8_t(val & 0xFF);
    _cursor++;
  }

  ASMJIT_FORCE_INLINE void done(BaseAssembler* a) noexcept {
    CodeBuffer& buffer = a->_section->_buffer;
    size_t newSize = (size_t)(_cursor - a->_bufferData);
    ASMJIT_ASSERT(newSize <= buffer.capacity());

    a->_bufferPtr = _cursor;
    buffer._size = Support::max(buffer._size, newSize);
  }
};

//! Code writer utilities.
namespace CodeWriterUtils {

bool encodeOffset32(uint32_t* dst, int64_t offset64, const OffsetFormat& format) noexcept;
bool encodeOffset64(uint64_t* dst, int64_t offset64, const OffsetFormat& format) noexcept;

bool writeOffset(void* dst, int64_t offset64, const OffsetFormat& format) noexcept;

} // {CodeWriterUtils}

//! \}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_CODEBUFFERWRITER_P_H_INCLUDED
