// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ZONESTRING_H_INCLUDED
#define ASMJIT_CORE_ZONESTRING_H_INCLUDED

#include "../core/globals.h"
#include "../core/zone.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_zone
//! \{

//! A helper class used by \ref ZoneString implementation.
struct ZoneStringBase {
  union {
    struct {
      uint32_t _size;
      char _embedded[sizeof(void*) * 2 - 4];
    };
    struct {
      void* _dummy;
      char* _external;
    };
  };

  inline void reset() noexcept {
    _dummy = nullptr;
    _external = nullptr;
  }

  Error setData(Zone* zone, uint32_t maxEmbeddedSize, const char* str, size_t size) noexcept {
    if (size == SIZE_MAX)
      size = strlen(str);

    if (size <= maxEmbeddedSize) {
      memcpy(_embedded, str, size);
      _embedded[size] = '\0';
    }
    else {
      char* external = static_cast<char*>(zone->dup(str, size, true));
      if (ASMJIT_UNLIKELY(!external))
        return DebugUtils::errored(kErrorOutOfMemory);
      _external = external;
    }

    _size = uint32_t(size);
    return kErrorOk;
  }
};

//! A string template that can be zone allocated.
//!
//! Helps with creating strings that can be either statically allocated if they are small, or externally allocated
//! in case their size exceeds the limit. The `N` represents the size of the whole `ZoneString` structure, based on
//! that size the maximum size of the internal buffer is determined.
template<size_t N>
class ZoneString {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    kWholeSize = (N > sizeof(ZoneStringBase)) ? uint32_t(N) : uint32_t(sizeof(ZoneStringBase)),
    kMaxEmbeddedSize = kWholeSize - 5
  };

  //! \}

  //! \name Members
  //! \{

  union {
    ZoneStringBase _base;
    char _wholeData[kWholeSize];
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline ZoneString() noexcept { reset(); }
  inline void reset() noexcept { _base.reset(); }

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether the string is empty.
  inline bool empty() const noexcept { return _base._size == 0; }

  //! Returns the string data.
  inline const char* data() const noexcept { return _base._size <= kMaxEmbeddedSize ? _base._embedded : _base._external; }
  //! Returns the string size.
  inline uint32_t size() const noexcept { return _base._size; }

  //! Tests whether the string is embedded (e.g. no dynamically allocated).
  inline bool isEmbedded() const noexcept { return _base._size <= kMaxEmbeddedSize; }

  //! Copies a new `data` of the given `size` to the string.
  //!
  //! If the `size` exceeds the internal buffer the given `zone` will be used to duplicate the data, otherwise
  //! the internal buffer will be used as a storage.
  inline Error setData(Zone* zone, const char* data, size_t size) noexcept {
    return _base.setData(zone, kMaxEmbeddedSize, data, size);
  }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_ZONESTRING_H_INCLUDED
