// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

// Support - Tests
// ===============

#if defined(ASMJIT_TEST)
template<typename T>
static void testArrays(const T* a, const T* b, size_t size) noexcept {
  for (size_t i = 0; i < size; i++)
    EXPECT_EQ(a[i], b[i])
      .message("Mismatch at %u", unsigned(i));
}

static void testAlignment() noexcept {
  INFO("Support::isAligned()");
  EXPECT_FALSE(Support::isAligned<size_t>(0xFFFF, 4u));
  EXPECT_TRUE(Support::isAligned<size_t>(0xFFF4, 4u));
  EXPECT_TRUE(Support::isAligned<size_t>(0xFFF8, 8u));
  EXPECT_TRUE(Support::isAligned<size_t>(0xFFF0, 16u));

  INFO("Support::alignUp()");
  EXPECT_EQ(Support::alignUp<size_t>(0xFFFF, 4), 0x10000u);
  EXPECT_EQ(Support::alignUp<size_t>(0xFFF4, 4), 0x0FFF4u);
  EXPECT_EQ(Support::alignUp<size_t>(0xFFF8, 8), 0x0FFF8u);
  EXPECT_EQ(Support::alignUp<size_t>(0xFFF0, 16), 0x0FFF0u);
  EXPECT_EQ(Support::alignUp<size_t>(0xFFF0, 32), 0x10000u);

  INFO("Support::alignUpDiff()");
  EXPECT_EQ(Support::alignUpDiff<size_t>(0xFFFF, 4), 1u);
  EXPECT_EQ(Support::alignUpDiff<size_t>(0xFFF4, 4), 0u);
  EXPECT_EQ(Support::alignUpDiff<size_t>(0xFFF8, 8), 0u);
  EXPECT_EQ(Support::alignUpDiff<size_t>(0xFFF0, 16), 0u);
  EXPECT_EQ(Support::alignUpDiff<size_t>(0xFFF0, 32), 16u);

  INFO("Support::alignUpPowerOf2()");
  EXPECT_EQ(Support::alignUpPowerOf2<size_t>(0x0000), 0x00000u);
  EXPECT_EQ(Support::alignUpPowerOf2<size_t>(0xFFFF), 0x10000u);
  EXPECT_EQ(Support::alignUpPowerOf2<size_t>(0xF123), 0x10000u);
  EXPECT_EQ(Support::alignUpPowerOf2<size_t>(0x0F00), 0x01000u);
  EXPECT_EQ(Support::alignUpPowerOf2<size_t>(0x0100), 0x00100u);
  EXPECT_EQ(Support::alignUpPowerOf2<size_t>(0x1001), 0x02000u);
}

static void testBitUtils() noexcept {
  uint32_t i;

  INFO("Support::shl() / shr()");
  EXPECT_EQ(Support::shl(int32_t(0x00001111), 16), int32_t(0x11110000u));
  EXPECT_EQ(Support::shl(uint32_t(0x00001111), 16), uint32_t(0x11110000u));
  EXPECT_EQ(Support::shr(int32_t(0x11110000u), 16), int32_t(0x00001111u));
  EXPECT_EQ(Support::shr(uint32_t(0x11110000u), 16), uint32_t(0x00001111u));
  EXPECT_EQ(Support::sar(int32_t(0xFFFF0000u), 16), int32_t(0xFFFFFFFFu));
  EXPECT_EQ(Support::sar(uint32_t(0xFFFF0000u), 16), uint32_t(0xFFFFFFFFu));

  INFO("Support::blsi()");
  for (i = 0; i < 32; i++) EXPECT_EQ(Support::blsi(uint32_t(1) << i), uint32_t(1) << i);
  for (i = 0; i < 31; i++) EXPECT_EQ(Support::blsi(uint32_t(3) << i), uint32_t(1) << i);
  for (i = 0; i < 64; i++) EXPECT_EQ(Support::blsi(uint64_t(1) << i), uint64_t(1) << i);
  for (i = 0; i < 63; i++) EXPECT_EQ(Support::blsi(uint64_t(3) << i), uint64_t(1) << i);

  INFO("Support::ctz()");
  for (i = 0; i < 32; i++) EXPECT_EQ(Support::Internal::clzFallback(uint32_t(1) << i), 31 - i);
  for (i = 0; i < 64; i++) EXPECT_EQ(Support::Internal::clzFallback(uint64_t(1) << i), 63 - i);
  for (i = 0; i < 32; i++) EXPECT_EQ(Support::Internal::ctzFallback(uint32_t(1) << i), i);
  for (i = 0; i < 64; i++) EXPECT_EQ(Support::Internal::ctzFallback(uint64_t(1) << i), i);
  for (i = 0; i < 32; i++) EXPECT_EQ(Support::clz(uint32_t(1) << i), 31 - i);
  for (i = 0; i < 64; i++) EXPECT_EQ(Support::clz(uint64_t(1) << i), 63 - i);
  for (i = 0; i < 32; i++) EXPECT_EQ(Support::ctz(uint32_t(1) << i), i);
  for (i = 0; i < 64; i++) EXPECT_EQ(Support::ctz(uint64_t(1) << i), i);

  INFO("Support::bitMask()");
  EXPECT_EQ(Support::bitMask(0, 1, 7), 0x83u);
  for (i = 0; i < 32; i++)
    EXPECT_EQ(Support::bitMask(i), (1u << i));

  INFO("Support::bitTest()");
  for (i = 0; i < 32; i++) {
    EXPECT_TRUE(Support::bitTest((1 << i), i))
      .message("Support::bitTest(%X, %u) should return true", (1 << i), i);
  }

  INFO("Support::lsbMask<uint32_t>()");
  for (i = 0; i < 32; i++) {
    uint32_t expectedBits = 0;
    for (uint32_t b = 0; b < i; b++)
      expectedBits |= uint32_t(1) << b;
    EXPECT_EQ(Support::lsbMask<uint32_t>(i), expectedBits);
  }

  INFO("Support::lsbMask<uint64_t>()");
  for (i = 0; i < 64; i++) {
    uint64_t expectedBits = 0;
    for (uint32_t b = 0; b < i; b++)
      expectedBits |= uint64_t(1) << b;
    EXPECT_EQ(Support::lsbMask<uint64_t>(i), expectedBits);
  }

  INFO("Support::popcnt()");
  for (i = 0; i < 32; i++) EXPECT_EQ(Support::popcnt((uint32_t(1) << i)), 1u);
  for (i = 0; i < 64; i++) EXPECT_EQ(Support::popcnt((uint64_t(1) << i)), 1u);
  EXPECT_EQ(Support::popcnt(0x000000F0), 4u);
  EXPECT_EQ(Support::popcnt(0x10101010), 4u);
  EXPECT_EQ(Support::popcnt(0xFF000000), 8u);
  EXPECT_EQ(Support::popcnt(0xFFFFFFF7), 31u);
  EXPECT_EQ(Support::popcnt(0x7FFFFFFF), 31u);

  INFO("Support::isPowerOf2()");
  for (i = 0; i < 64; i++) {
    EXPECT_TRUE(Support::isPowerOf2(uint64_t(1) << i));
    EXPECT_FALSE(Support::isPowerOf2((uint64_t(1) << i) ^ 0x001101));
  }
}

static void testIntUtils() noexcept {
  INFO("Support::byteswap()");
  EXPECT_EQ(Support::byteswap16(0x0102), 0x0201u);
  EXPECT_EQ(Support::byteswap32(0x01020304), 0x04030201u);
  EXPECT_EQ(Support::byteswap32(0x01020304), 0x04030201u);
  EXPECT_EQ(Support::byteswap64(uint64_t(0x0102030405060708)), uint64_t(0x0807060504030201));

  INFO("Support::bytepack()");
  union BytePackData {
    uint8_t bytes[4];
    uint32_t u32;
  } bpdata;

  bpdata.u32 = Support::bytepack32_4x8(0x00, 0x11, 0x22, 0x33);
  EXPECT_EQ(bpdata.bytes[0], 0x00);
  EXPECT_EQ(bpdata.bytes[1], 0x11);
  EXPECT_EQ(bpdata.bytes[2], 0x22);
  EXPECT_EQ(bpdata.bytes[3], 0x33);

  INFO("Support::isBetween()");
  EXPECT_TRUE(Support::isBetween<int>(10 , 10, 20));
  EXPECT_TRUE(Support::isBetween<int>(11 , 10, 20));
  EXPECT_TRUE(Support::isBetween<int>(20 , 10, 20));
  EXPECT_FALSE(Support::isBetween<int>(9  , 10, 20));
  EXPECT_FALSE(Support::isBetween<int>(21 , 10, 20));
  EXPECT_FALSE(Support::isBetween<int>(101, 10, 20));

  INFO("Support::isInt8()");
  EXPECT_TRUE(Support::isInt8(-128));
  EXPECT_TRUE(Support::isInt8( 127));
  EXPECT_FALSE(Support::isInt8(-129));
  EXPECT_FALSE(Support::isInt8( 128));

  INFO("Support::isInt16()");
  EXPECT_TRUE(Support::isInt16(-32768));
  EXPECT_TRUE(Support::isInt16( 32767));
  EXPECT_FALSE(Support::isInt16(-32769));
  EXPECT_FALSE(Support::isInt16( 32768));

  INFO("Support::isInt32()");
  EXPECT_TRUE(Support::isInt32( 2147483647    ));
  EXPECT_TRUE(Support::isInt32(-2147483647 - 1));
  EXPECT_FALSE(Support::isInt32(uint64_t(2147483648u)));
  EXPECT_FALSE(Support::isInt32(uint64_t(0xFFFFFFFFu)));
  EXPECT_FALSE(Support::isInt32(uint64_t(0xFFFFFFFFu) + 1));

  INFO("Support::isUInt8()");
  EXPECT_TRUE(Support::isUInt8(0)  );
  EXPECT_TRUE(Support::isUInt8(255));
  EXPECT_FALSE(Support::isUInt8(256));
  EXPECT_FALSE(Support::isUInt8(-1) );

  INFO("Support::isUInt12()");
  EXPECT_TRUE(Support::isUInt12(0)   );
  EXPECT_TRUE(Support::isUInt12(4095));
  EXPECT_FALSE(Support::isUInt12(4096));
  EXPECT_FALSE(Support::isUInt12(-1)  );

  INFO("Support::isUInt16()");
  EXPECT_TRUE(Support::isUInt16(0)    );
  EXPECT_TRUE(Support::isUInt16(65535));
  EXPECT_FALSE(Support::isUInt16(65536));
  EXPECT_FALSE(Support::isUInt16(-1)   );

  INFO("Support::isUInt32()");
  EXPECT_TRUE(Support::isUInt32(uint64_t(0xFFFFFFFF)));
  EXPECT_FALSE(Support::isUInt32(uint64_t(0xFFFFFFFF) + 1));
  EXPECT_FALSE(Support::isUInt32(-1));
}

static void testReadWrite() noexcept {
  INFO("Support::readX() / writeX()");

  uint8_t arr[32] = { 0 };

  Support::writeU16uBE(arr + 1, 0x0102u);
  Support::writeU16uBE(arr + 3, 0x0304u);
  EXPECT_EQ(Support::readU32uBE(arr + 1), 0x01020304u);
  EXPECT_EQ(Support::readU32uLE(arr + 1), 0x04030201u);
  EXPECT_EQ(Support::readU32uBE(arr + 2), 0x02030400u);
  EXPECT_EQ(Support::readU32uLE(arr + 2), 0x00040302u);

  Support::writeU32uLE(arr + 5, 0x05060708u);
  EXPECT_EQ(Support::readU64uBE(arr + 1), 0x0102030408070605u);
  EXPECT_EQ(Support::readU64uLE(arr + 1), 0x0506070804030201u);

  Support::writeU64uLE(arr + 7, 0x1122334455667788u);
  EXPECT_EQ(Support::readU32uBE(arr + 8), 0x77665544u);
}

static void testBitVector() noexcept {
  INFO("Support::bitVectorOp");
  {
    uint32_t vec[3] = { 0 };
    Support::bitVectorFill(vec, 1, 64);
    EXPECT_EQ(vec[0], 0xFFFFFFFEu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    Support::bitVectorClear(vec, 1, 1);
    EXPECT_EQ(vec[0], 0xFFFFFFFCu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    Support::bitVectorFill(vec, 0, 32);
    EXPECT_EQ(vec[0], 0xFFFFFFFFu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    Support::bitVectorClear(vec, 0, 32);
    EXPECT_EQ(vec[0], 0x00000000u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    Support::bitVectorFill(vec, 1, 30);
    EXPECT_EQ(vec[0], 0x7FFFFFFEu);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x00000001u);

    Support::bitVectorClear(vec, 1, 95);
    EXPECT_EQ(vec[0], 0x00000000u);
    EXPECT_EQ(vec[1], 0x00000000u);
    EXPECT_EQ(vec[2], 0x00000000u);

    Support::bitVectorFill(vec, 32, 64);
    EXPECT_EQ(vec[0], 0x00000000u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0xFFFFFFFFu);

    Support::bitVectorSetBit(vec, 1, true);
    EXPECT_EQ(vec[0], 0x00000002u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0xFFFFFFFFu);

    Support::bitVectorSetBit(vec, 95, false);
    EXPECT_EQ(vec[0], 0x00000002u);
    EXPECT_EQ(vec[1], 0xFFFFFFFFu);
    EXPECT_EQ(vec[2], 0x7FFFFFFFu);

    Support::bitVectorClear(vec, 33, 32);
    EXPECT_EQ(vec[0], 0x00000002u);
    EXPECT_EQ(vec[1], 0x00000001u);
    EXPECT_EQ(vec[2], 0x7FFFFFFEu);
  }

  INFO("Support::bitVectorIndexOf");
  {
    uint32_t vec1[1] = { 0x80000000 };
    EXPECT_EQ(Support::bitVectorIndexOf(vec1, 0, true), 31u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec1, 1, true), 31u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec1, 31, true), 31u);

    uint32_t vec2[2] = { 0x00000000, 0x80000000 };
    EXPECT_EQ(Support::bitVectorIndexOf(vec2, 0, true), 63u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec2, 1, true), 63u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec2, 31, true), 63u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec2, 32, true), 63u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec2, 33, true), 63u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec2, 63, true), 63u);

    uint32_t vec3[3] = { 0x00000001, 0x00000000, 0x80000000 };
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 0, true), 0u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 1, true), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 2, true), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 31, true), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 32, true), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 63, true), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 64, true), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec3, 95, true), 95u);

    uint32_t vec4[3] = { ~vec3[0], ~vec3[1], ~vec3[2] };
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 0, false), 0u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 1, false), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 2, false), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 31, false), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 32, false), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 63, false), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 64, false), 95u);
    EXPECT_EQ(Support::bitVectorIndexOf(vec4, 95, false), 95u);
  }

  INFO("Support::BitWordIterator<uint32_t>");
  {
    Support::BitWordIterator<uint32_t> it(0x80000F01u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 0u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 8u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 9u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 10u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 11u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_FALSE(it.hasNext());

    // No bits set.
    it.init(0x00000000u);
    EXPECT_FALSE(it.hasNext());

    // Only first bit set.
    it.init(0x00000001u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 0u);
    EXPECT_FALSE(it.hasNext());

    // Only last bit set (special case).
    it.init(0x80000000u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_FALSE(it.hasNext());
  }

  INFO("Support::BitWordIterator<uint64_t>");
  {
    Support::BitWordIterator<uint64_t> it(uint64_t(1) << 63);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_FALSE(it.hasNext());
  }

  INFO("Support::BitVectorIterator<uint32_t>");
  {
    // Border cases.
    static const uint32_t bitsNone[] = { 0xFFFFFFFFu };
    Support::BitVectorIterator<uint32_t> it(bitsNone, 0);

    EXPECT_FALSE(it.hasNext());
    it.init(bitsNone, 0, 1);
    EXPECT_FALSE(it.hasNext());
    it.init(bitsNone, 0, 128);
    EXPECT_FALSE(it.hasNext());

    static const uint32_t bits1[] = { 0x80000008u, 0x80000001u, 0x00000000u, 0x80000000u, 0x00000000u, 0x00000000u, 0x00003000u };
    it.init(bits1, ASMJIT_ARRAY_SIZE(bits1));

    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 3u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 32u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 127u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 204u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 205u);
    EXPECT_FALSE(it.hasNext());

    it.init(bits1, ASMJIT_ARRAY_SIZE(bits1), 4);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 31u);

    it.init(bits1, ASMJIT_ARRAY_SIZE(bits1), 64);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 127u);

    it.init(bits1, ASMJIT_ARRAY_SIZE(bits1), 127);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 127u);

    static const uint32_t bits2[] = { 0x80000000u, 0x80000000u, 0x00000000u, 0x80000000u };
    it.init(bits2, ASMJIT_ARRAY_SIZE(bits2));

    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 127u);
    EXPECT_FALSE(it.hasNext());

    static const uint32_t bits3[] = { 0x00000000u, 0x00000000u, 0x00000000u, 0x00000000u };
    it.init(bits3, ASMJIT_ARRAY_SIZE(bits3));
    EXPECT_FALSE(it.hasNext());

    static const uint32_t bits4[] = { 0x00000000u, 0x00000000u, 0x00000000u, 0x80000000u };
    it.init(bits4, ASMJIT_ARRAY_SIZE(bits4));
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 127u);
    EXPECT_FALSE(it.hasNext());
  }

  INFO("Support::BitVectorIterator<uint64_t>");
  {
    static const uint64_t bits1[] = { 0x80000000u, 0x80000000u, 0x00000000u, 0x80000000u };
    Support::BitVectorIterator<uint64_t> it(bits1, ASMJIT_ARRAY_SIZE(bits1));

    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 31u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 95u);
    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 223u);
    EXPECT_FALSE(it.hasNext());

    static const uint64_t bits2[] = { 0x8000000000000000u, 0, 0, 0 };
    it.init(bits2, ASMJIT_ARRAY_SIZE(bits2));

    EXPECT_TRUE(it.hasNext());
    EXPECT_EQ(it.next(), 63u);
    EXPECT_FALSE(it.hasNext());
  }
}

static void testSorting() noexcept {
  INFO("Support::qSort() - Testing qsort and isort of predefined arrays");
  {
    constexpr size_t kArraySize = 11;

    int ref_[kArraySize] = { -4, -2, -1, 0, 1, 9, 12, 13, 14, 19, 22 };
    int arr1[kArraySize] = { 0, 1, -1, 19, 22, 14, -4, 9, 12, 13, -2 };
    int arr2[kArraySize];

    memcpy(arr2, arr1, kArraySize * sizeof(int));

    Support::iSort(arr1, kArraySize);
    Support::qSort(arr2, kArraySize);
    testArrays(arr1, ref_, kArraySize);
    testArrays(arr2, ref_, kArraySize);
  }

  INFO("Support::qSort() - Testing qsort and isort of artificial arrays");
  {
    constexpr size_t kArraySize = 200;

    int arr1[kArraySize];
    int arr2[kArraySize];
    int ref_[kArraySize];

    for (size_t size = 2; size < kArraySize; size++) {
      for (size_t i = 0; i < size; i++) {
        arr1[i] = int(size - 1 - i);
        arr2[i] = int(size - 1 - i);
        ref_[i] = int(i);
      }

      Support::iSort(arr1, size);
      Support::qSort(arr2, size);
      testArrays(arr1, ref_, size);
      testArrays(arr2, ref_, size);
    }
  }

  INFO("Support::qSort() - Testing qsort and isort with an unstable compare function");
  {
    constexpr size_t kArraySize = 5;

    float arr1[kArraySize] = { 1.0f, 0.0f, 3.0f, -1.0f, std::numeric_limits<float>::quiet_NaN() };
    float arr2[kArraySize] = { };

    memcpy(arr2, arr1, kArraySize * sizeof(float));

    // We don't test as it's undefined where the NaN would be.
    Support::iSort(arr1, kArraySize);
    Support::qSort(arr2, kArraySize);
  }
}

UNIT(support) {
  testAlignment();
  testBitUtils();
  testIntUtils();
  testReadWrite();
  testBitVector();
  testSorting();
}
#endif

ASMJIT_END_NAMESPACE
