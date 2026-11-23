// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/algorithm.h>
#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_NAMESPACE

template<typename T>
static void test_eq_arrays(const T* a, const T* b, size_t size) noexcept {
  for (size_t i = 0; i < size; i++) {
    EXPECT_EQ(a[i], b[i])
      .message("Mismatch at %u", unsigned(i));
  }
}

TEST_CASE(axl_algorithm_lower_bound) {
  static const int arr[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12 };
  Span arr_span = Span<const int>::from_array(arr);

  EXPECT_EQ(axl::lower_bound(Span<const int>(arr, 0), 0), 0u);
  EXPECT_EQ(axl::lower_bound(arr_span, -11000), 0u);
  EXPECT_EQ(axl::lower_bound(arr_span, 0), 0u);
  EXPECT_EQ(axl::lower_bound(arr_span, 1), 1u);
  EXPECT_EQ(axl::lower_bound(arr_span, 2), 2u);
  EXPECT_EQ(axl::lower_bound(arr_span, 3), 3u);
  EXPECT_EQ(axl::lower_bound(arr_span, 4), 4u);
  EXPECT_EQ(axl::lower_bound(arr_span, 5), 5u);
  EXPECT_EQ(axl::lower_bound(arr_span, 6), 6u);
  EXPECT_EQ(axl::lower_bound(arr_span, 10), 10u);
  EXPECT_EQ(axl::lower_bound(arr_span, 11), 11u);
  EXPECT_EQ(axl::lower_bound(arr_span, 12), 11u);
  EXPECT_EQ(axl::lower_bound(arr_span, 11000), ASMJIT_ARRAY_SIZE(arr));
}

TEST_CASE(axl_algorithm_sort) {
  {
    constexpr size_t kArraySize = 20;

    int ref_[kArraySize] = {-23, -22, -11, -4, -2, -1, 0, 1, 5, 8, 9, 12, 13, 14, 19, 22, 33, 44, 0xFFFF, 0xFFFFFF};
    int arr1[kArraySize] = {0, 44, 0xFFFFFF, 1, -1, 19, 5, 22, 0xFFFF, 14, 33, -22, -4, 9, 12, 13, -2, -11, 8, -23};
    int arr2[kArraySize];

    memcpy(arr2, arr1, kArraySize * sizeof(int));

    axl::insertion_sort(Span<int>::from_array(arr1));
    axl::sort(Span<int>::from_array(arr2));

    test_eq_arrays(arr1, ref_, kArraySize);
    test_eq_arrays(arr2, ref_, kArraySize);
  }

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

      axl::insertion_sort(Span<int>(arr1, size));
      axl::sort(Span<int>(arr2, size));
      test_eq_arrays(arr1, ref_, size);
      test_eq_arrays(arr2, ref_, size);
    }
  }

  {
    constexpr size_t kArraySize = 5;

    float arr1[kArraySize] = {1.0f, 0.0f, 3.0f, -1.0f, axl::quiet_nan<float>()};
    float arr2[kArraySize] = {};
    memcpy(arr2, arr1, kArraySize * sizeof(float));

    // We don't test as it's undefined where the NaN would be.
    axl::insertion_sort(Span<float>(arr1, kArraySize));
    axl::sort(Span<float>(arr2, kArraySize));
  }
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
