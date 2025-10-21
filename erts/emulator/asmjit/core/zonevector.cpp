// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/support.h"
#include "../core/zone.h"
#include "../core/zonevector.h"

ASMJIT_BEGIN_NAMESPACE

// ZoneVectorBase - Helpers
// ========================

// ZoneVector is used as an array to hold short-lived data structures used during code generation. The growing
// strategy is simple - use small capacity at the beginning (very good for ZoneAllocator) and then grow quicker
// to prevent successive reallocations.
static ASMJIT_FORCE_INLINE uint32_t ZoneVector_growCapacity(uint32_t current, uint32_t growMinimum, uint32_t sizeOfT) noexcept {
  static constexpr size_t kGrowThreshold = Globals::kGrowThreshold;

  size_t byteSize = size_t(current) * sizeOfT;
  size_t minimumByteSize = size_t(growMinimum) * sizeOfT;

  // This is more than exponential growth at the beginning.
  if (byteSize < 32) {
    byteSize = 32;
  }
  else if (byteSize < 128) {
    byteSize = 128;
  }
  else if (byteSize < 512) {
    byteSize = 512;
  }

  if (byteSize < minimumByteSize) {
    // Exponential growth before we reach `kGrowThreshold`.
    byteSize = Support::alignUpPowerOf2(minimumByteSize);

    // Bail to `growMinimum` in case of overflow - most likely whatever that is happening afterwards would just fail.
    if (byteSize < minimumByteSize) {
      return growMinimum;
    }

    // Pretty much chunked growth advancing by `kGrowThreshold` after we exceed it.
    // This should not be a common case, so we don't really have to optimize for it.
    if (byteSize > kGrowThreshold) {
      // Align to kGrowThreshold.
      size_t remainder = minimumByteSize % kGrowThreshold;

      byteSize = minimumByteSize + remainder;

      // Bail to `growMinimum` in case of overflow - should never happen as it's unlikely we would hit this on a 32-bit
      // machine (consecutive near 4GiB allocation is impossible, and this should never happen on 64-bit machine as we
      // use 32-bit size & capacity, so overflow of 64 bit integer is not possible. Added just as an extreme measure.
      if (byteSize < minimumByteSize)
        return growMinimum;
    }
  }

  size_t n = byteSize / sizeOfT;
  return uint32_t(Support::min<size_t>(n, 0xFFFFFFFFu));
}

static ASMJIT_FORCE_INLINE bool ZoneVector_byteSizeIsSafe(size_t nBytes, uint32_t n) noexcept {
  if (sizeof(uint32_t) < sizeof(size_t))
    return true; // there is no problem when running on a 64-bit machine.
  else
    return nBytes >= size_t(n);
};

Error ZoneVectorBase::_grow(ZoneAllocator* allocator, uint32_t sizeOfT, uint32_t n) noexcept {
  uint32_t capacity = _capacity;
  uint32_t after = _size;

  if (ASMJIT_UNLIKELY(std::numeric_limits<uint32_t>::max() - n < after))
    return DebugUtils::errored(kErrorOutOfMemory);

  after += n;
  if (capacity >= after)
    return kErrorOk;

  return _reserve(allocator, sizeOfT, ZoneVector_growCapacity(capacity, after, sizeOfT));
}

Error ZoneVectorBase::_reserve(ZoneAllocator* allocator, uint32_t sizeOfT, uint32_t n) noexcept {
  uint32_t oldCapacity = _capacity;
  if (oldCapacity >= n)
    return kErrorOk;

  size_t nBytes = size_t(n) * sizeOfT;
  if (ASMJIT_UNLIKELY(!ZoneVector_byteSizeIsSafe(nBytes, n)))
    return DebugUtils::errored(kErrorOutOfMemory);

  size_t allocatedBytes;
  uint8_t* newData = static_cast<uint8_t*>(allocator->alloc(nBytes, allocatedBytes));

  if (ASMJIT_UNLIKELY(!newData))
    return DebugUtils::errored(kErrorOutOfMemory);

  uint32_t newCapacity = uint32_t(allocatedBytes / sizeOfT);
  ASMJIT_ASSERT(newCapacity >= n);

  void* oldData = _data;
  if (oldData && _size) {
    memcpy(newData, oldData, size_t(_size) * sizeOfT);
    allocator->release(oldData, size_t(oldCapacity) * sizeOfT);
  }

  _data = newData;
  _capacity = newCapacity;

  return kErrorOk;
}

Error ZoneVectorBase::_growingReserve(ZoneAllocator* allocator, uint32_t sizeOfT, uint32_t n) noexcept {
  uint32_t capacity = _capacity;
  if (capacity >= n)
    return kErrorOk;
  return _reserve(allocator, sizeOfT, ZoneVector_growCapacity(capacity, n, sizeOfT));
}

Error ZoneVectorBase::_resize(ZoneAllocator* allocator, uint32_t sizeOfT, uint32_t n) noexcept {
  uint32_t size = _size;

  if (_capacity < n) {
    ASMJIT_PROPAGATE(_grow(allocator, sizeOfT, n - size));
    ASMJIT_ASSERT(_capacity >= n);
  }

  if (size < n)
    memset(static_cast<uint8_t*>(_data) + size_t(size) * sizeOfT, 0, size_t(n - size) * sizeOfT);

  _size = n;
  return kErrorOk;
}

// ZoneBitVector - Operations
// ==========================

Error ZoneBitVector::copyFrom(ZoneAllocator* allocator, const ZoneBitVector& other) noexcept {
  BitWord* data = _data;
  uint32_t newSize = other.size();

  if (!newSize) {
    _size = 0;
    return kErrorOk;
  }

  if (newSize > _capacity) {
    // Realloc needed... Calculate the minimum capacity (in bytes) required.
    uint32_t minimumCapacityInBits = Support::alignUp<uint32_t>(newSize, kBitWordSizeInBits);
    if (ASMJIT_UNLIKELY(minimumCapacityInBits < newSize))
      return DebugUtils::errored(kErrorOutOfMemory);

    // Normalize to bytes.
    uint32_t minimumCapacity = minimumCapacityInBits / 8;
    size_t allocatedCapacity;

    BitWord* newData = static_cast<BitWord*>(allocator->alloc(minimumCapacity, allocatedCapacity));
    if (ASMJIT_UNLIKELY(!newData))
      return DebugUtils::errored(kErrorOutOfMemory);

    // `allocatedCapacity` now contains number in bytes, we need bits.
    size_t allocatedCapacityInBits = allocatedCapacity * 8;

    // Arithmetic overflow should normally not happen. If it happens we just
    // change the `allocatedCapacityInBits` to the `minimumCapacityInBits` as
    // this value is still safe to be used to call `_allocator->release(...)`.
    if (ASMJIT_UNLIKELY(allocatedCapacityInBits < allocatedCapacity))
      allocatedCapacityInBits = minimumCapacityInBits;

    if (data)
      allocator->release(data, _capacity / 8);
    data = newData;

    _data = data;
    _capacity = uint32_t(allocatedCapacityInBits);
  }

  _size = newSize;
  _copyBits(data, other.data(), _wordsPerBits(newSize));

  return kErrorOk;
}

Error ZoneBitVector::_resize(ZoneAllocator* allocator, uint32_t newSize, uint32_t idealCapacity, bool newBitsValue) noexcept {
  ASMJIT_ASSERT(idealCapacity >= newSize);

  if (newSize <= _size) {
    // The size after the resize is lesser than or equal to the current size.
    uint32_t idx = newSize / kBitWordSizeInBits;
    uint32_t bit = newSize % kBitWordSizeInBits;

    // Just set all bits outside of the new size in the last word to zero.
    // There is a case that there are not bits to set if `bit` is zero. This
    // happens when `newSize` is a multiply of `kBitWordSizeInBits` like 64, 128,
    // and so on. In that case don't change anything as that would mean settings
    // bits outside of the `_size`.
    if (bit)
      _data[idx] &= (BitWord(1) << bit) - 1u;

    _size = newSize;
    return kErrorOk;
  }

  uint32_t oldSize = _size;
  BitWord* data = _data;

  if (newSize > _capacity) {
    // Realloc needed, calculate the minimum capacity (in bytes) required.
    uint32_t minimumCapacityInBits = Support::alignUp<uint32_t>(idealCapacity, kBitWordSizeInBits);

    if (ASMJIT_UNLIKELY(minimumCapacityInBits < newSize))
      return DebugUtils::errored(kErrorOutOfMemory);

    // Normalize to bytes.
    uint32_t minimumCapacity = minimumCapacityInBits / 8;
    size_t allocatedCapacity;

    BitWord* newData = static_cast<BitWord*>(allocator->alloc(minimumCapacity, allocatedCapacity));
    if (ASMJIT_UNLIKELY(!newData))
      return DebugUtils::errored(kErrorOutOfMemory);

    // `allocatedCapacity` now contains number in bytes, we need bits.
    size_t allocatedCapacityInBits = allocatedCapacity * 8;

    // Arithmetic overflow should normally not happen. If it happens we just
    // change the `allocatedCapacityInBits` to the `minimumCapacityInBits` as
    // this value is still safe to be used to call `_allocator->release(...)`.
    if (ASMJIT_UNLIKELY(allocatedCapacityInBits < allocatedCapacity))
      allocatedCapacityInBits = minimumCapacityInBits;

    _copyBits(newData, data, _wordsPerBits(oldSize));

    if (data)
      allocator->release(data, _capacity / 8);
    data = newData;

    _data = data;
    _capacity = uint32_t(allocatedCapacityInBits);
  }

  // Start (of the old size) and end (of the new size) bits
  uint32_t idx = oldSize / kBitWordSizeInBits;
  uint32_t startBit = oldSize % kBitWordSizeInBits;
  uint32_t endBit = newSize % kBitWordSizeInBits;

  // Set new bits to either 0 or 1. The `pattern` is used to set multiple
  // bits per bit-word and contains either all zeros or all ones.
  BitWord pattern = Support::bitMaskFromBool<BitWord>(newBitsValue);

  // First initialize the last bit-word of the old size.
  if (startBit) {
    uint32_t nBits = 0;

    if (idx == (newSize / kBitWordSizeInBits)) {
      // The number of bit-words is the same after the resize. In that case
      // we need to set only bits necessary in the current last bit-word.
      ASMJIT_ASSERT(startBit < endBit);
      nBits = endBit - startBit;
    }
    else {
      // There is be more bit-words after the resize. In that case we don't
      // have to be extra careful about the last bit-word of the old size.
      nBits = kBitWordSizeInBits - startBit;
    }

    data[idx++] |= pattern << nBits;
  }

  // Initialize all bit-words after the last bit-word of the old size.
  uint32_t endIdx = _wordsPerBits(newSize);
  while (idx < endIdx) data[idx++] = pattern;

  // Clear unused bits of the last bit-word.
  if (endBit)
    data[endIdx - 1] = pattern & ((BitWord(1) << endBit) - 1);

  _size = newSize;
  return kErrorOk;
}

Error ZoneBitVector::_append(ZoneAllocator* allocator, bool value) noexcept {
  uint32_t kThreshold = Globals::kGrowThreshold * 8;
  uint32_t newSize = _size + 1;
  uint32_t idealCapacity = _capacity;

  if (idealCapacity < 128)
    idealCapacity = 128;
  else if (idealCapacity <= kThreshold)
    idealCapacity *= 2;
  else
    idealCapacity += kThreshold;

  if (ASMJIT_UNLIKELY(idealCapacity < _capacity)) {
    if (ASMJIT_UNLIKELY(_size == std::numeric_limits<uint32_t>::max()))
      return DebugUtils::errored(kErrorOutOfMemory);
    idealCapacity = newSize;
  }

  return _resize(allocator, newSize, idealCapacity, value);
}

// ZoneVector / ZoneBitVector - Tests
// ==================================

#if defined(ASMJIT_TEST)
template<typename T>
static void test_zone_vector(ZoneAllocator* allocator, const char* typeName) {
  constexpr uint32_t kMiB = 1024 * 1024;

  int i;
  int kMax = 100000;

  ZoneVector<T> vec;

  INFO("ZoneVector<%s> basic tests", typeName);
  EXPECT_EQ(vec.append(allocator, 0), kErrorOk);
  EXPECT_FALSE(vec.empty());
  EXPECT_EQ(vec.size(), 1u);
  EXPECT_GE(vec.capacity(), 1u);
  EXPECT_EQ(vec.indexOf(0), 0u);
  EXPECT_EQ(vec.indexOf(-11), Globals::kNotFound);

  vec.clear();
  EXPECT_TRUE(vec.empty());
  EXPECT_EQ(vec.size(), 0u);
  EXPECT_EQ(vec.indexOf(0), Globals::kNotFound);

  for (i = 0; i < kMax; i++) {
    EXPECT_EQ(vec.append(allocator, T(i)), kErrorOk);
  }
  EXPECT_FALSE(vec.empty());
  EXPECT_EQ(vec.size(), uint32_t(kMax));
  EXPECT_EQ(vec.indexOf(T(0)), uint32_t(0));
  EXPECT_EQ(vec.indexOf(T(kMax - 1)), uint32_t(kMax - 1));

  EXPECT_EQ(vec.begin()[0], 0);
  EXPECT_EQ(vec.end()[-1], kMax - 1);

  EXPECT_EQ(vec.rbegin()[0], kMax - 1);
  EXPECT_EQ(vec.rend()[-1], 0);

  int64_t fsum = 0;
  int64_t rsum = 0;

  for (const T& item : vec) {
    fsum += item;
  }

  for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
    rsum += *it;
  }

  EXPECT_EQ(fsum, rsum);
  vec.release(allocator);

  INFO("ZoneBitVector::growingReserve()");
  for (uint32_t j = 0; j < 40 / sizeof(T); j += 8) {
    EXPECT_EQ(vec.growingReserve(allocator, j * kMiB), kErrorOk);
    EXPECT_GE(vec.capacity(), j * kMiB);
  }
}

static void test_zone_bitvector(ZoneAllocator* allocator) {
  Zone zone(8096 - Zone::kBlockOverhead);

  uint32_t i, count;
  uint32_t kMaxCount = 100;

  ZoneBitVector vec;
  EXPECT_TRUE(vec.empty());
  EXPECT_EQ(vec.size(), 0u);

  INFO("ZoneBitVector::resize()");
  for (count = 1; count < kMaxCount; count++) {
    vec.clear();
    EXPECT_EQ(vec.resize(allocator, count, false), kErrorOk);
    EXPECT_EQ(vec.size(), count);

    for (i = 0; i < count; i++)
      EXPECT_FALSE(vec.bitAt(i));

    vec.clear();
    EXPECT_EQ(vec.resize(allocator, count, true), kErrorOk);
    EXPECT_EQ(vec.size(), count);

    for (i = 0; i < count; i++)
      EXPECT_TRUE(vec.bitAt(i));
  }

  INFO("ZoneBitVector::fillBits() / clearBits()");
  for (count = 1; count < kMaxCount; count += 2) {
    vec.clear();
    EXPECT_EQ(vec.resize(allocator, count), kErrorOk);
    EXPECT_EQ(vec.size(), count);

    for (i = 0; i < (count + 1) / 2; i++) {
      bool value = bool(i & 1);
      if (value)
        vec.fillBits(i, count - i * 2);
      else
        vec.clearBits(i, count - i * 2);
    }

    for (i = 0; i < count; i++) {
      EXPECT_EQ(vec.bitAt(i), bool(i & 1));
    }
  }
}

UNIT(zone_vector) {
  Zone zone(8096 - Zone::kBlockOverhead);
  ZoneAllocator allocator(&zone);

  test_zone_vector<int>(&allocator, "int");
  test_zone_vector<int64_t>(&allocator, "int64_t");
  test_zone_bitvector(&allocator);
}
#endif

ASMJIT_END_NAMESPACE
