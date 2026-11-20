// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_vector.h>
#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_NAMESPACE

template<typename T>
static void test_arena_vector(axl::Arena& arena, const char* type_name) {
  constexpr uint32_t kMiB = 1024 * 1024;

  size_t i;
  size_t kMax = 100000;

  axl::ArenaVector<T> vec;

  TEST_LOG("ArenaVector<%s> basic tests", type_name);
  EXPECT_EQ(vec.append(arena, 0), axl::kSuccess);
  EXPECT_FALSE(vec.is_empty());
  EXPECT_EQ(vec.size(), 1u);
  EXPECT_GE(vec.capacity(), 1u);
  EXPECT_EQ(vec.index_of(0), size_t(0));
  EXPECT_EQ(vec.index_of(-11), SIZE_MAX);

  vec.clear();
  EXPECT_TRUE(vec.is_empty());
  EXPECT_EQ(vec.size(), 0u);
  EXPECT_EQ(vec.index_of(0), SIZE_MAX);

  for (i = 0; i < kMax; i++) {
    EXPECT_EQ(vec.append(arena, T(i)), axl::kSuccess);
  }
  EXPECT_FALSE(vec.is_empty());
  EXPECT_EQ(vec.size(), size_t(kMax));
  EXPECT_EQ(vec.index_of(T(0)), size_t(0));
  EXPECT_EQ(vec.index_of(T(kMax - 1)), uint32_t(kMax - 1));

  EXPECT_EQ(vec.begin()[0], 0);
  EXPECT_EQ(vec.end()[-1], T(kMax - 1));

  TEST_LOG("ArenaVector<%s>::operator=(ArenaVector<%s>&&)", type_name, type_name);
  axl::ArenaVector<T> moved_vec(axl::move(vec));
  EXPECT_EQ(vec.data(), nullptr);
  EXPECT_EQ(vec.size(), 0u);
  EXPECT_EQ(vec.capacity(), 0u);

  moved_vec.release(arena);

  TEST_LOG("ArenaVector<%s>::reserve_grow()", type_name);
  for (uint32_t j = 8; j < 40 / sizeof(T); j += 8) {
    EXPECT_EQ(vec.reserve_grow(arena, j * kMiB), axl::kSuccess);
    EXPECT_GE(vec.capacity(), j * kMiB);
  }
}

template<typename T>
static void test_arena_vector_capacity(axl::Arena& arena, const char* type_name) {
  axl::ArenaVector<T> vec;

  TEST_LOG("ArenaVector<%s> capacity (growing) test", type_name);

  for (size_t i = 0; i < 10000000; i++) {
    size_t old_capacity = vec.capacity();
    EXPECT_EQ(vec.append(arena, T(i)), axl::kSuccess);

    if (vec.capacity() != old_capacity) {
      TEST_LOG("  Increasing capacity from %zu to %zu (vector size=%zu)", old_capacity, vec.capacity(), vec.size());
    }
  }
}

TEST_CASE(axl_arena_vector) {
  axl::Arena arena(8192);

  test_arena_vector<int32_t>(arena, "int32_t");
  test_arena_vector_capacity<int32_t>(arena, "int32_t");

  test_arena_vector<int64_t>(arena, "int64_t");
  test_arena_vector_capacity<int64_t>(arena, "int64_t");
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
