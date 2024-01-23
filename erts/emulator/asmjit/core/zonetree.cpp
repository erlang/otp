// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/support.h"
#include "../core/zone.h"
#include "../core/zonetree.h"

ASMJIT_BEGIN_NAMESPACE

// ZoneTreeBase - Tests
// ====================

#if defined(ASMJIT_TEST)
template<typename NodeT>
struct ZoneRBUnit {
  typedef ZoneTree<NodeT> Tree;

  static void verifyTree(Tree& tree) noexcept {
    EXPECT_GT(checkHeight(static_cast<NodeT*>(tree._root)), 0);
  }

  // Check whether the Red-Black tree is valid.
  static int checkHeight(NodeT* node) noexcept {
    if (!node) return 1;

    NodeT* ln = node->left();
    NodeT* rn = node->right();

    // Invalid tree.
    EXPECT_TRUE(ln == nullptr || *ln < *node);
    EXPECT_TRUE(rn == nullptr || *rn > *node);

    // Red violation.
    EXPECT_TRUE(!node->isRed() || (!ZoneTreeNode::_isValidRed(ln) && !ZoneTreeNode::_isValidRed(rn)));

    // Black violation.
    int lh = checkHeight(ln);
    int rh = checkHeight(rn);
    EXPECT_TRUE(!lh || !rh || lh == rh);

    // Only count black links.
    return (lh && rh) ? lh + !node->isRed() : 0;
  }
};

class MyRBNode : public ZoneTreeNodeT<MyRBNode> {
public:
  ASMJIT_NONCOPYABLE(MyRBNode)

  inline explicit MyRBNode(uint32_t key) noexcept
    : _key(key) {}

  inline bool operator<(const MyRBNode& other) const noexcept { return _key < other._key; }
  inline bool operator>(const MyRBNode& other) const noexcept { return _key > other._key; }

  inline bool operator<(uint32_t queryKey) const noexcept { return _key < queryKey; }
  inline bool operator>(uint32_t queryKey) const noexcept { return _key > queryKey; }

  uint32_t _key;
};

UNIT(zone_rbtree) {
  uint32_t kCount = BrokenAPI::hasArg("--quick") ? 1000 : 10000;

  Zone zone(4096);
  ZoneTree<MyRBNode> rbTree;

  uint32_t key;
  INFO("Inserting %u elements to RBTree and validating each operation", unsigned(kCount));
  for (key = 0; key < kCount; key++) {
    rbTree.insert(zone.newT<MyRBNode>(key));
    ZoneRBUnit<MyRBNode>::verifyTree(rbTree);
  }

  uint32_t count = kCount;
  INFO("Removing %u elements from RBTree and validating each operation", unsigned(kCount));
  do {
    MyRBNode* node;

    for (key = 0; key < count; key++) {
      node = rbTree.get(key);
      EXPECT_NOT_NULL(node);
      EXPECT_EQ(node->_key, key);
    }

    node = rbTree.get(--count);
    rbTree.remove(node);
    ZoneRBUnit<MyRBNode>::verifyTree(rbTree);
  } while (count);

  EXPECT_TRUE(rbTree.empty());
}
#endif

ASMJIT_END_NAMESPACE
