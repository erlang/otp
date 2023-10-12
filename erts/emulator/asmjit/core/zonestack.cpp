// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/zone.h"
#include "../core/zonestack.h"

ASMJIT_BEGIN_NAMESPACE

// ZoneStackBase - Init & Reset
// ============================

Error ZoneStackBase::_init(ZoneAllocator* allocator, size_t middleIndex) noexcept {
  ZoneAllocator* oldAllocator = _allocator;

  if (oldAllocator) {
    Block* block = _block[kBlockIndexFirst];
    while (block) {
      Block* next = block->next();
      oldAllocator->release(block, kBlockSize);
      block = next;
    }

    _allocator = nullptr;
    _block[kBlockIndexFirst] = nullptr;
    _block[kBlockIndexLast] = nullptr;
  }

  if (allocator) {
    Block* block = static_cast<Block*>(allocator->alloc(kBlockSize));
    if (ASMJIT_UNLIKELY(!block))
      return DebugUtils::errored(kErrorOutOfMemory);

    block->_link[kBlockIndexPrev] = nullptr;
    block->_link[kBlockIndexNext] = nullptr;
    block->_start = (uint8_t*)block + middleIndex;
    block->_end = (uint8_t*)block + middleIndex;

    _allocator = allocator;
    _block[kBlockIndexFirst] = block;
    _block[kBlockIndexLast] = block;
  }

  return kErrorOk;
}

// ZoneStackBase - Operations
// ==========================

Error ZoneStackBase::_prepareBlock(uint32_t side, size_t initialIndex) noexcept {
  ASMJIT_ASSERT(isInitialized());

  Block* prev = _block[side];
  ASMJIT_ASSERT(!prev->empty());

  Block* block = _allocator->allocT<Block>(kBlockSize);
  if (ASMJIT_UNLIKELY(!block))
    return DebugUtils::errored(kErrorOutOfMemory);

  block->_link[ side] = nullptr;
  block->_link[!side] = prev;
  block->_start = (uint8_t*)block + initialIndex;
  block->_end = (uint8_t*)block + initialIndex;

  prev->_link[side] = block;
  _block[side] = block;

  return kErrorOk;
}

void ZoneStackBase::_cleanupBlock(uint32_t side, size_t middleIndex) noexcept {
  Block* block = _block[side];
  ASMJIT_ASSERT(block->empty());

  Block* prev = block->_link[!side];
  if (prev) {
    ASMJIT_ASSERT(prev->_link[side] == block);
    _allocator->release(block, kBlockSize);

    prev->_link[side] = nullptr;
    _block[side] = prev;
  }
  else if (_block[!side] == block) {
    // If the container becomes empty center both pointers in the remaining block.
    block->_start = (uint8_t*)block + middleIndex;
    block->_end = (uint8_t*)block + middleIndex;
  }
}

// ZoneStack - Tests
// =================

#if defined(ASMJIT_TEST)
template<typename T>
static void test_zone_stack(ZoneAllocator* allocator, const char* typeName) {
  ZoneStack<T> stack;

  INFO("Testing ZoneStack<%s>", typeName);
  INFO("  (%d items per one Block)", ZoneStack<T>::kNumBlockItems);

  EXPECT(stack.init(allocator) == kErrorOk);
  EXPECT(stack.empty(), "Stack must be empty after `init()`");

  EXPECT(stack.append(42) == kErrorOk);
  EXPECT(!stack.empty()        , "Stack must not be empty after an item has been appended");
  EXPECT(stack.pop() == 42     , "Stack.pop() must return the item that has been appended last");
  EXPECT(stack.empty()         , "Stack must be empty after the last item has been removed");

  EXPECT(stack.prepend(43) == kErrorOk);
  EXPECT(!stack.empty()        , "Stack must not be empty after an item has been prepended");
  EXPECT(stack.popFirst() == 43, "Stack.popFirst() must return the item that has been prepended last");
  EXPECT(stack.empty()         , "Stack must be empty after the last item has been removed");

  int i;
  int iMin =-100000;
  int iMax = 100000;

  INFO("Validating prepend() & popFirst()");
  for (i = iMax; i >= 0; i--) stack.prepend(T(i));
  for (i = 0; i <= iMax; i++) {
    T item = stack.popFirst();
    EXPECT(i == item, "Item '%d' didn't match the item '%lld' popped", i, (long long)item);
    if (!stack.empty()) {
      item = stack.popFirst();
      EXPECT(i + 1 == item, "Item '%d' didn't match the item '%lld' popped", i + 1, (long long)item);
      stack.prepend(item);
    }
  }
  EXPECT(stack.empty());

  INFO("Validating append() & pop()");
  for (i = 0; i <= iMax; i++) stack.append(T(i));
  for (i = iMax; i >= 0; i--) {
    T item = stack.pop();
    EXPECT(i == item, "Item '%d' didn't match the item '%lld' popped", i, (long long)item);
    if (!stack.empty()) {
      item = stack.pop();
      EXPECT(i - 1 == item, "Item '%d' didn't match the item '%lld' popped", i - 1, (long long)item);
      stack.append(item);
    }
  }
  EXPECT(stack.empty());

  INFO("Validating append()/prepend() & popFirst()");
  for (i = 1; i <= iMax; i++) stack.append(T(i));
  for (i = 0; i >= iMin; i--) stack.prepend(T(i));

  for (i = iMin; i <= iMax; i++) {
    T item = stack.popFirst();
    EXPECT(i == item, "Item '%d' didn't match the item '%lld' popped", i, (long long)item);
  }
  EXPECT(stack.empty());

  INFO("Validating append()/prepend() & pop()");
  for (i = 0; i >= iMin; i--) stack.prepend(T(i));
  for (i = 1; i <= iMax; i++) stack.append(T(i));

  for (i = iMax; i >= iMin; i--) {
    T item = stack.pop();
    EXPECT(i == item, "Item '%d' didn't match the item '%lld' popped", i, (long long)item);
  }
  EXPECT(stack.empty());
}

UNIT(zone_stack) {
  Zone zone(8096 - Zone::kBlockOverhead);
  ZoneAllocator allocator(&zone);

  test_zone_stack<int>(&allocator, "int");
  test_zone_stack<int64_t>(&allocator, "int64_t");
}
#endif

ASMJIT_END_NAMESPACE
