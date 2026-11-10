// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_test_p.h>
#if defined(ASMJIT_TEST)

#include <asmjit/axl/arena.h>
#include <asmjit/axl/arena_tree.h>
#include <asmjit/axl/commons.h>

ASMJIT_BEGIN_NAMESPACE

template<typename NodeT>
struct ArenaTreeUnit {
  using Tree = axl::ArenaTree<NodeT>;

  static void verify_tree(Tree& tree) noexcept {
    EXPECT_GT(check_height(static_cast<NodeT*>(tree._root)), 0);
  }

  // Check whether the Red-Black tree is valid.
  static int check_height(NodeT* node) noexcept {
    if (!node) return 1;

    NodeT* ln = node->left();
    NodeT* rn = node->right();

    // Invalid tree.
    EXPECT_TRUE(ln == nullptr || *ln < *node);
    EXPECT_TRUE(rn == nullptr || *node < *rn);

    // Red violation.
    EXPECT_TRUE(!node->is_red() || (!axl::ArenaTreeNode::_is_valid_red(ln) && !axl::ArenaTreeNode::_is_valid_red(rn)));

    // Black violation.
    int lh = check_height(ln);
    int rh = check_height(rn);
    EXPECT_TRUE(!lh || !rh || lh == rh);

    // Only count black links.
    return (lh && rh) ? lh + !node->is_red() : 0;
  }
};

class MyRBNode : public axl::ArenaTreeNodeT<MyRBNode> {
public:
  ASMJIT_NONCOPYABLE(MyRBNode)

  inline explicit MyRBNode(uint32_t key) noexcept
    : _key(key) {}

  uint32_t _key;
};

static inline bool operator<(const MyRBNode& a, const MyRBNode& b) noexcept { return a._key < b._key; }
static inline bool operator<(const MyRBNode& a, uint32_t b) noexcept { return a._key < b; }
static inline bool operator<(uint32_t a, const MyRBNode& b) noexcept { return a < b._key; }

TEST_CASE(axl_arena_tree) {
  uint32_t kCount = ZeroTest::test_runner()->cmd_line().has_arg("--quick") ? 1000 : 10000;

  axl::Arena arena(4096);
  axl::ArenaTree<MyRBNode> rb_tree;

  uint32_t key;
  TEST_LOG("Inserting %u elements to RBTree and validating each operation", unsigned(kCount));
  for (key = 0; key < kCount; key++) {
    rb_tree.insert(arena.new_oneshot<MyRBNode>(key));
    ArenaTreeUnit<MyRBNode>::verify_tree(rb_tree);
  }

  uint32_t count = kCount;
  TEST_LOG("Removing %u elements from RBTree and validating each operation", unsigned(kCount));
  do {
    MyRBNode* node;

    for (key = 0; key < count; key++) {
      node = rb_tree.get(key);
      EXPECT_NOT_NULL(node);
      EXPECT_EQ(node->_key, key);
    }

    node = rb_tree.get(--count);
    rb_tree.remove(node);
    ArenaTreeUnit<MyRBNode>::verify_tree(rb_tree);
  } while (count);

  EXPECT_TRUE(rb_tree.is_empty());
}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_TEST
