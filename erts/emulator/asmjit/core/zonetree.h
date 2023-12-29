// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ZONETREE_H_INCLUDED
#define ASMJIT_CORE_ZONETREE_H_INCLUDED

#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_zone
//! \{

//! RB-Tree node.
//!
//! The color is stored in a least significant bit of the `left` node.
//!
//! WARNING: Always use accessors to access left and right children.
class ZoneTreeNode {
public:
  ASMJIT_NONCOPYABLE(ZoneTreeNode)

  //! \name Constants
  //! \{

  enum : uintptr_t {
    kRedMask = 0x1,
    kPtrMask = ~kRedMask
  };

  //! \}

  //! \name Members
  //! \{

  uintptr_t _rbNodeData[2] {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG ZoneTreeNode() noexcept {}

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG bool isRed() const noexcept { return static_cast<bool>(_rbNodeData[0] & kRedMask); }

  ASMJIT_INLINE_NODEBUG bool hasChild(size_t i) const noexcept { return _rbNodeData[i] > kRedMask; }
  ASMJIT_INLINE_NODEBUG bool hasLeft() const noexcept { return _rbNodeData[0] > kRedMask; }
  ASMJIT_INLINE_NODEBUG bool hasRight() const noexcept { return _rbNodeData[1] != 0; }

  template<typename T = ZoneTreeNode>
  ASMJIT_INLINE_NODEBUG T* child(size_t i) const noexcept { return static_cast<T*>(_getChild(i)); }
  template<typename T = ZoneTreeNode>
  ASMJIT_INLINE_NODEBUG T* left() const noexcept { return static_cast<T*>(_getLeft()); }
  template<typename T = ZoneTreeNode>
  ASMJIT_INLINE_NODEBUG T* right() const noexcept { return static_cast<T*>(_getRight()); }

  //! \}

  //! \cond INTERNAL
  //! \name Internal
  //! \{

  ASMJIT_INLINE_NODEBUG ZoneTreeNode* _getChild(size_t i) const noexcept { return (ZoneTreeNode*)(_rbNodeData[i] & kPtrMask); }
  ASMJIT_INLINE_NODEBUG ZoneTreeNode* _getLeft() const noexcept { return (ZoneTreeNode*)(_rbNodeData[0] & kPtrMask); }
  ASMJIT_INLINE_NODEBUG ZoneTreeNode* _getRight() const noexcept { return (ZoneTreeNode*)(_rbNodeData[1]); }

  ASMJIT_INLINE_NODEBUG void _setChild(size_t i, ZoneTreeNode* node) noexcept { _rbNodeData[i] = (_rbNodeData[i] & kRedMask) | (uintptr_t)node; }
  ASMJIT_INLINE_NODEBUG void _setLeft(ZoneTreeNode* node) noexcept { _rbNodeData[0] = (_rbNodeData[0] & kRedMask) | (uintptr_t)node; }
  ASMJIT_INLINE_NODEBUG void _setRight(ZoneTreeNode* node) noexcept { _rbNodeData[1] = (uintptr_t)node; }

  ASMJIT_INLINE_NODEBUG void _makeRed() noexcept { _rbNodeData[0] |= kRedMask; }
  ASMJIT_INLINE_NODEBUG void _makeBlack() noexcept { _rbNodeData[0] &= kPtrMask; }

  //! Tests whether the node is RED (RED node must be non-null and must have RED flag set).
  static ASMJIT_INLINE_NODEBUG bool _isValidRed(ZoneTreeNode* node) noexcept { return node && node->isRed(); }

  //! \}
  //! \endcond
};

//! RB-Tree node casted to `NodeT`.
template<typename NodeT>
class ZoneTreeNodeT : public ZoneTreeNode {
public:
  ASMJIT_NONCOPYABLE(ZoneTreeNodeT)

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG ZoneTreeNodeT() noexcept
    : ZoneTreeNode() {}

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG NodeT* child(size_t i) const noexcept { return static_cast<NodeT*>(_getChild(i)); }
  ASMJIT_INLINE_NODEBUG NodeT* left() const noexcept { return static_cast<NodeT*>(_getLeft()); }
  ASMJIT_INLINE_NODEBUG NodeT* right() const noexcept { return static_cast<NodeT*>(_getRight()); }

  //! \}
};

//! RB-Tree.
template<typename NodeT>
class ZoneTree {
public:
  ASMJIT_NONCOPYABLE(ZoneTree)

  typedef NodeT Node;
  NodeT* _root {};

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG ZoneTree() noexcept {}
  ASMJIT_INLINE_NODEBUG ZoneTree(ZoneTree&& other) noexcept
    : _root(other._root) {}
  ASMJIT_INLINE_NODEBUG void reset() noexcept { _root = nullptr; }

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG bool empty() const noexcept { return _root == nullptr; }
  ASMJIT_INLINE_NODEBUG NodeT* root() const noexcept { return static_cast<NodeT*>(_root); }

  //! \}

  //! \name Utilities
  //! \{

  ASMJIT_INLINE_NODEBUG void swap(ZoneTree& other) noexcept {
    std::swap(_root, other._root);
  }

  template<typename CompareT = Support::Compare<Support::SortOrder::kAscending>>
  void insert(NodeT* ASMJIT_NONNULL(node), const CompareT& cmp = CompareT()) noexcept {
    // Node to insert must not contain garbage.
    ASMJIT_ASSERT(!node->hasLeft());
    ASMJIT_ASSERT(!node->hasRight());
    ASMJIT_ASSERT(!node->isRed());

    if (!_root) {
      _root = node;
      return;
    }

    ZoneTreeNode head;           // False root node,
    head._setRight(_root);       // having root on the right.

    ZoneTreeNode* g = nullptr;   // Grandparent.
    ZoneTreeNode* p = nullptr;   // Parent.
    ZoneTreeNode* t = &head;     // Iterator.
    ZoneTreeNode* q = _root;     // Query.

    size_t dir = 0;              // Direction for accessing child nodes.
    size_t last = 0;             // Not needed to initialize, but makes some tools happy.

    node->_makeRed();            // New nodes are always red and violations fixed appropriately.

    // Search down the tree.
    for (;;) {
      if (!q) {
        // Insert new node at the bottom.
        q = node;
        p->_setChild(dir, node);
      }
      else if (_isValidRed(q->_getLeft()) && _isValidRed(q->_getRight())) {
        // Color flip.
        q->_makeRed();
        q->_getLeft()->_makeBlack();
        q->_getRight()->_makeBlack();
      }

      // Fix red violation.
      if (_isValidRed(q) && _isValidRed(p)) {
        ASMJIT_ASSUME(g != nullptr);
        ASMJIT_ASSUME(p != nullptr);
        t->_setChild(t->_getRight() == g,
                     q == p->_getChild(last) ? _singleRotate(g, !last) : _doubleRotate(g, !last));
      }

      // Stop if found.
      if (q == node)
        break;

      last = dir;
      dir = cmp(*static_cast<NodeT*>(q), *static_cast<NodeT*>(node)) < 0;

      // Update helpers.
      if (g) t = g;

      g = p;
      p = q;
      q = q->_getChild(dir);
    }

    // Update root and make it black.
    _root = static_cast<NodeT*>(head._getRight());
    _root->_makeBlack();
  }

  //! Remove node from RBTree.
  template<typename CompareT = Support::Compare<Support::SortOrder::kAscending>>
  void remove(ZoneTreeNode* ASMJIT_NONNULL(node), const CompareT& cmp = CompareT()) noexcept {
    ZoneTreeNode head;           // False root node,
    head._setRight(_root);       // having root on the right.

    ZoneTreeNode* g = nullptr;   // Grandparent.
    ZoneTreeNode* p = nullptr;   // Parent.
    ZoneTreeNode* q = &head;     // Query.

    ZoneTreeNode* f  = nullptr;  // Found item.
    ZoneTreeNode* gf = nullptr;  // Found grandparent.
    size_t dir = 1;              // Direction (0 or 1).

    // Search and push a red down.
    while (q->hasChild(dir)) {
      size_t last = dir;

      // Update helpers.
      g = p;
      p = q;
      q = q->_getChild(dir);
      dir = cmp(*static_cast<NodeT*>(q), *static_cast<NodeT*>(node)) < 0;

      // Save found node.
      if (q == node) {
        f = q;
        gf = g;
      }

      // Push the red node down.
      if (!_isValidRed(q) && !_isValidRed(q->_getChild(dir))) {
        if (_isValidRed(q->_getChild(!dir))) {
          ZoneTreeNode* child = _singleRotate(q, dir);
          p->_setChild(last, child);
          p = child;
        }
        else if (!_isValidRed(q->_getChild(!dir)) && p->_getChild(!last)) {
          ZoneTreeNode* s = p->_getChild(!last);
          if (!_isValidRed(s->_getChild(!last)) && !_isValidRed(s->_getChild(last))) {
            // Color flip.
            p->_makeBlack();
            s->_makeRed();
            q->_makeRed();
          }
          else {
            ASMJIT_ASSUME(g != nullptr);
            ASMJIT_ASSUME(s != nullptr);

            size_t dir2 = g->_getRight() == p;
            ZoneTreeNode* child = g->_getChild(dir2);

            if (_isValidRed(s->_getChild(last))) {
              child = _doubleRotate(p, last);
              g->_setChild(dir2, child);
            }
            else if (_isValidRed(s->_getChild(!last))) {
              child = _singleRotate(p, last);
              g->_setChild(dir2, child);
            }

            // Ensure correct coloring.
            q->_makeRed();
            child->_makeRed();
            child->_getLeft()->_makeBlack();
            child->_getRight()->_makeBlack();
          }
        }
      }
    }

    // Replace and remove.
    ASMJIT_ASSERT(f != nullptr);
    ASMJIT_ASSERT(f != &head);
    ASMJIT_ASSERT(q != &head);

    p->_setChild(p->_getRight() == q,
                 q->_getChild(q->_getLeft() == nullptr));

    // NOTE: The original algorithm used a trick to just copy 'key/value' to `f` and mark `q` for deletion. But this
    // is unacceptable here as we really want to destroy the passed `node`. So, we have to make sure that we have
    // really removed `f` and not `q`.
    if (f != q) {
      ASMJIT_ASSERT(f != &head);
      ASMJIT_ASSERT(f != gf);

      ZoneTreeNode* n = gf ? gf : &head;
      dir = (n == &head) ? 1  : cmp(*static_cast<NodeT*>(n), *static_cast<NodeT*>(node)) < 0;

      for (;;) {
        if (n->_getChild(dir) == f) {
          n->_setChild(dir, q);
          // RAW copy, including the color.
          q->_rbNodeData[0] = f->_rbNodeData[0];
          q->_rbNodeData[1] = f->_rbNodeData[1];
          break;
        }

        n = n->_getChild(dir);

        // Cannot be true as we know that it must reach `f` in few iterations.
        ASMJIT_ASSERT(n != nullptr);
        dir = cmp(*static_cast<NodeT*>(n), *static_cast<NodeT*>(node)) < 0;
      }
    }

    // Update root and make it black.
    _root = static_cast<NodeT*>(head._getRight());
    if (_root) _root->_makeBlack();
  }

  template<typename KeyT, typename CompareT = Support::Compare<Support::SortOrder::kAscending>>
  inline NodeT* get(const KeyT& key, const CompareT& cmp = CompareT()) const noexcept {
    ZoneTreeNode* node = _root;
    while (node) {
      auto result = cmp(*static_cast<const NodeT*>(node), key);
      if (result == 0) break;

      // Go left or right depending on the `result`.
      node = node->_getChild(result < 0);
    }
    return static_cast<NodeT*>(node);
  }

  //! \}

  //! \cond INTERNAL
  //! \name Internal
  //! \{

  static inline bool _isValidRed(ZoneTreeNode* node) noexcept { return ZoneTreeNode::_isValidRed(node); }

  //! Single rotation.
  static inline ZoneTreeNode* _singleRotate(ZoneTreeNode* ASMJIT_NONNULL(root), size_t dir) noexcept {
    ZoneTreeNode* save = root->_getChild(!dir);
    ASMJIT_ASSUME(save != nullptr);

    ZoneTreeNode* saveChild = save->_getChild(dir);
    root->_setChild(!dir, saveChild);
    save->_setChild( dir, root);
    root->_makeRed();
    save->_makeBlack();
    return save;
  }

  //! Double rotation.
  static inline ZoneTreeNode* _doubleRotate(ZoneTreeNode* ASMJIT_NONNULL(root), size_t dir) noexcept {
    ZoneTreeNode* child = root->_getChild(!dir);
    ASMJIT_ASSUME(child != nullptr);

    root->_setChild(!dir, _singleRotate(child, !dir));
    return _singleRotate(root, dir);
  }

  //! \}
  //! \endcond
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_ZONETREE_H_INCLUDED
