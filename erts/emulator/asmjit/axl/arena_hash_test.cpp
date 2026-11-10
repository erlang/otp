// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_hash.h>

ASMJIT_BEGIN_NAMESPACE

struct MyHashNode : public axl::ArenaHashNode {
  uint32_t _key;

  inline MyHashNode(uint32_t key) noexcept
    : axl::ArenaHashNode(key), _key(key) {}
};

struct MyKeyMatcher {
  inline MyKeyMatcher(uint32_t key) noexcept
    : _key(key) {}

  inline uint32_t hash_code() const noexcept { return _key; }
  inline bool matches(const MyHashNode* node) const noexcept { return node->_key == _key; }

  uint32_t _key;
};

TEST_CASE(axl_arena_hash) {
  uint32_t kCount = ZeroTest::test_runner()->cmd_line().has_arg("--quick") ? 1000 : 10000;

  axl::Arena arena(4096);
  axl::ArenaHash<MyHashNode> hash_table;

  uint32_t key;
  TEST_LOG("Inserting %u elements to HashTable", unsigned(kCount));
  for (key = 0; key < kCount; key++) {
    hash_table.insert(arena, arena.new_oneshot<MyHashNode>(key));
  }

  uint32_t count = kCount;
  TEST_LOG("Removing %u elements from HashTable and validating each operation", unsigned(kCount));
  do {
    MyHashNode* node;

    for (key = 0; key < count; key++) {
      node = hash_table.get(MyKeyMatcher(key));
      EXPECT_NOT_NULL(node);
      EXPECT_EQ(node->_key, key);
    }

    {
      count--;
      node = hash_table.get(MyKeyMatcher(count));
      hash_table.remove(arena, node);

      node = hash_table.get(MyKeyMatcher(count));
      EXPECT_NULL(node);
    }
  } while (count);

  EXPECT_TRUE(hash_table.is_empty());
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
