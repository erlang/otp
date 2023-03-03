// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_CODEBUFFER_H_INCLUDED
#define ASMJIT_CORE_CODEBUFFER_H_INCLUDED

#include "../core/globals.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_core
//! \{

//! Flags used by \ref CodeBuffer.
enum class CodeBufferFlags : uint32_t {
  //! No flags.
  kNone = 0,
  //! Buffer is external (not allocated by asmjit).
  kIsExternal = 0x00000001u,
  //! Buffer is fixed (cannot be reallocated).
  kIsFixed = 0x00000002u
};
ASMJIT_DEFINE_ENUM_FLAGS(CodeBufferFlags)

//! Code or data buffer.
struct CodeBuffer {
  //! \name Members
  //! \{

  //! The content of the buffer (data).
  uint8_t* _data;
  //! Number of bytes of `data` used.
  size_t _size;
  //! Buffer capacity (in bytes).
  size_t _capacity;
  //! Buffer flags.
  CodeBufferFlags _flags;

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Returns a reference to the byte at the given `index`.
  inline uint8_t& operator[](size_t index) noexcept {
    ASMJIT_ASSERT(index < _size);
    return _data[index];
  }
  //! \overload
  inline const uint8_t& operator[](size_t index) const noexcept {
    ASMJIT_ASSERT(index < _size);
    return _data[index];
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns code buffer flags.
  inline CodeBufferFlags flags() const noexcept { return _flags; }
  //! Tests whether the code buffer has the given `flag` set.
  inline bool hasFlag(CodeBufferFlags flag) const noexcept { return Support::test(_flags, flag); }

  //! Tests whether this code buffer has a fixed size.
  //!
  //! Fixed size means that the code buffer is fixed and cannot grow.
  inline bool isFixed() const noexcept { return hasFlag(CodeBufferFlags::kIsFixed); }

  //! Tests whether the data in this code buffer is external.
  //!
  //! External data can only be provided by users, it's never used by AsmJit.
  inline bool isExternal() const noexcept { return hasFlag(CodeBufferFlags::kIsExternal); }

  //! Tests whether the data in this code buffer is allocated (non-null).
  inline bool isAllocated() const noexcept { return _data != nullptr; }

  //! Tests whether the code buffer is empty.
  inline bool empty() const noexcept { return !_size; }

  //! Returns the size of the data.
  inline size_t size() const noexcept { return _size; }
  //! Returns the capacity of the data.
  inline size_t capacity() const noexcept { return _capacity; }

  //! Returns the pointer to the data the buffer references.
  inline uint8_t* data() noexcept { return _data; }
  //! \overload
  inline const uint8_t* data() const noexcept { return _data; }

  //! \}

  //! \name Iterators
  //! \{

  inline uint8_t* begin() noexcept { return _data; }
  inline const uint8_t* begin() const noexcept { return _data; }

  inline uint8_t* end() noexcept { return _data + _size; }
  inline const uint8_t* end() const noexcept { return _data + _size; }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_CODEBUFFER_H_INCLUDED

