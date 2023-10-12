// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ZONESTACK_H_INCLUDED
#define ASMJIT_CORE_ZONESTACK_H_INCLUDED

#include "../core/zone.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_zone
//! \{

//! Base class used by \ref ZoneStack.
class ZoneStackBase {
public:
  ASMJIT_NONCOPYABLE(ZoneStackBase)

  //! \name Constants
  //! \{

  enum : size_t {
    kBlockIndexPrev = 0,
    kBlockIndexNext = 1,

    kBlockIndexFirst = 0,
    kBlockIndexLast = 1,

    kBlockSize = ZoneAllocator::kHiMaxSize
  };

  //! \}

  //! \name Types
  //! \{

  struct Block {
    //! Next and previous blocks.
    Block* _link[2];
    //! Pointer to the start of the array.
    void* _start;
    //! Pointer to the end of the array.
    void* _end;

    inline bool empty() const noexcept { return _start == _end; }
    inline Block* prev() const noexcept { return _link[kBlockIndexPrev]; }
    inline Block* next() const noexcept { return _link[kBlockIndexNext]; }

    inline void setPrev(Block* block) noexcept { _link[kBlockIndexPrev] = block; }
    inline void setNext(Block* block) noexcept { _link[kBlockIndexNext] = block; }

    template<typename T>
    inline T* start() const noexcept { return static_cast<T*>(_start); }
    template<typename T>
    inline void setStart(T* start) noexcept { _start = static_cast<void*>(start); }

    template<typename T>
    inline T* end() const noexcept { return (T*)_end; }
    template<typename T>
    inline void setEnd(T* end) noexcept { _end = (void*)end; }

    template<typename T>
    inline T* data() const noexcept { return (T*)((uint8_t*)(this) + sizeof(Block)); }

    template<typename T>
    inline bool canPrepend() const noexcept { return _start > data<void>(); }

    template<typename T>
    inline bool canAppend() const noexcept {
      size_t kNumBlockItems = (kBlockSize - sizeof(Block)) / sizeof(T);
      size_t kStartBlockIndex = sizeof(Block);
      size_t kEndBlockIndex = kStartBlockIndex + kNumBlockItems * sizeof(T);

      return (uintptr_t)_end <= ((uintptr_t)this + kEndBlockIndex - sizeof(T));
    }
  };

  //! \}

  //! \name Members
  //! \{

  //! Allocator used to allocate data.
  ZoneAllocator* _allocator;
  //! First and last blocks.
  Block* _block[2];

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline ZoneStackBase() noexcept {
    _allocator = nullptr;
    _block[0] = nullptr;
    _block[1] = nullptr;
  }
  inline ~ZoneStackBase() noexcept { reset(); }

  inline bool isInitialized() const noexcept { return _allocator != nullptr; }
  ASMJIT_API Error _init(ZoneAllocator* allocator, size_t middleIndex) noexcept;
  inline Error reset() noexcept { return _init(nullptr, 0); }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns `ZoneAllocator` attached to this container.
  inline ZoneAllocator* allocator() const noexcept { return _allocator; }

  inline bool empty() const noexcept {
    ASMJIT_ASSERT(isInitialized());
    return _block[0]->start<void>() == _block[1]->end<void>();
  }

  //! \}

  //! \cond INTERNAL
  //! \name Internal
  //! \{

  ASMJIT_API Error _prepareBlock(uint32_t side, size_t initialIndex) noexcept;
  ASMJIT_API void _cleanupBlock(uint32_t side, size_t middleIndex) noexcept;

  //! \}
  //! \endcond
};

//! Zone allocated stack container.
template<typename T>
class ZoneStack : public ZoneStackBase {
public:
  ASMJIT_NONCOPYABLE(ZoneStack)

  //! \name Constants
  //! \{

  enum : uint32_t {
    kNumBlockItems   = uint32_t((kBlockSize - sizeof(Block)) / sizeof(T)),
    kStartBlockIndex = uint32_t(sizeof(Block)),
    kMidBlockIndex   = uint32_t(kStartBlockIndex + (kNumBlockItems / 2) * sizeof(T)),
    kEndBlockIndex   = uint32_t(kStartBlockIndex + (kNumBlockItems    ) * sizeof(T))
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline ZoneStack() noexcept {}
  inline ~ZoneStack() noexcept {}

  inline Error init(ZoneAllocator* allocator) noexcept { return _init(allocator, kMidBlockIndex); }

  //! \}

  //! \name Utilities
  //! \{

  inline Error prepend(T item) noexcept {
    ASMJIT_ASSERT(isInitialized());
    Block* block = _block[kBlockIndexFirst];

    if (!block->canPrepend<T>()) {
      ASMJIT_PROPAGATE(_prepareBlock(kBlockIndexFirst, kEndBlockIndex));
      block = _block[kBlockIndexFirst];
    }

    T* ptr = block->start<T>() - 1;
    ASMJIT_ASSERT(ptr >= block->data<T>() && ptr <= block->data<T>() + (kNumBlockItems - 1));
    *ptr = item;
    block->setStart<T>(ptr);
    return kErrorOk;
  }

  inline Error append(T item) noexcept {
    ASMJIT_ASSERT(isInitialized());
    Block* block = _block[kBlockIndexLast];

    if (!block->canAppend<T>()) {
      ASMJIT_PROPAGATE(_prepareBlock(kBlockIndexLast, kStartBlockIndex));
      block = _block[kBlockIndexLast];
    }

    T* ptr = block->end<T>();
    ASMJIT_ASSERT(ptr >= block->data<T>() && ptr <= block->data<T>() + (kNumBlockItems - 1));

    *ptr++ = item;
    block->setEnd(ptr);
    return kErrorOk;
  }

  inline T popFirst() noexcept {
    ASMJIT_ASSERT(isInitialized());
    ASMJIT_ASSERT(!empty());

    Block* block = _block[kBlockIndexFirst];
    ASMJIT_ASSERT(!block->empty());

    T* ptr = block->start<T>();
    T item = *ptr++;

    block->setStart(ptr);
    if (block->empty())
      _cleanupBlock(kBlockIndexFirst, kMidBlockIndex);

    return item;
  }

  inline T pop() noexcept {
    ASMJIT_ASSERT(isInitialized());
    ASMJIT_ASSERT(!empty());

    Block* block = _block[kBlockIndexLast];
    ASMJIT_ASSERT(!block->empty());

    T* ptr = block->end<T>();
    T item = *--ptr;
    ASMJIT_ASSERT(ptr >= block->data<T>());
    ASMJIT_ASSERT(ptr >= block->start<T>());

    block->setEnd(ptr);
    if (block->empty())
      _cleanupBlock(kBlockIndexLast, kMidBlockIndex);

    return item;
  }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_ZONESTACK_H_INCLUDED
