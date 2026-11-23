// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INPLACE_STORAGE_H_INCLUDED
#define ASMJIT_AXL_INPLACE_STORAGE_H_INCLUDED

#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Inline storage that can be used as an underlying storage for arenas and containers.
template<size_t N>
struct alignas(8) InplaceStorage {
  //! \name Members
  //! \{

  //! Inline storage.
  uint8_t _data[N];

  //! \}

  //! \name Accessors
  //! \{

  //! Returns the size of inline storage.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG size_t size() noexcept { return N; }
  //! Returns the inline storage data.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG uint8_t* data() noexcept { return _data; }
  //! Returns the inline storage data as `Span<uint8_t>`.
  [[nodiscard]] ASMJIT_INLINE_NODEBUG Span<uint8_t> span() noexcept { return Span<uint8_t>(_data, N); }

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INPLACE_STORAGE_H_INCLUDED
