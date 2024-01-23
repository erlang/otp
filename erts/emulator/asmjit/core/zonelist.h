// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ZONELIST_H_INCLUDED
#define ASMJIT_CORE_ZONELIST_H_INCLUDED

#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_zone
//! \{

//! Node used by \ref ZoneList template.
template<typename NodeT>
class ZoneListNode {
public:
  ASMJIT_NONCOPYABLE(ZoneListNode)

  //! \name Constants
  //! \{

  enum : size_t {
    kNodeIndexPrev = 0,
    kNodeIndexNext = 1
  };

  //! \}

  //! \name Members
  //! \{

  NodeT* _listNodes[2];

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG ZoneListNode() noexcept
    : _listNodes { nullptr, nullptr } {}

  ASMJIT_INLINE_NODEBUG ZoneListNode(ZoneListNode&& other) noexcept
    : _listNodes { other._listNodes[0], other._listNodes[1] } {}

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG bool hasPrev() const noexcept { return _listNodes[kNodeIndexPrev] != nullptr; }
  ASMJIT_INLINE_NODEBUG bool hasNext() const noexcept { return _listNodes[kNodeIndexNext] != nullptr; }

  ASMJIT_INLINE_NODEBUG NodeT* prev() const noexcept { return _listNodes[kNodeIndexPrev]; }
  ASMJIT_INLINE_NODEBUG NodeT* next() const noexcept { return _listNodes[kNodeIndexNext]; }

  //! \}
};

//! Zone allocated list container that uses nodes of `NodeT` type.
template <typename NodeT>
class ZoneList {
public:
  ASMJIT_NONCOPYABLE(ZoneList)

  //! \name Constants
  //! \{

  enum : size_t {
    kNodeIndexFirst = 0,
    kNodeIndexLast = 1
  };

  //! \}

  //! \name Members
  //! \{

  NodeT* _nodes[2] {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG ZoneList() noexcept {}

  ASMJIT_INLINE_NODEBUG ZoneList(ZoneList&& other) noexcept
    : _nodes { other._nodes[0], other._nodes[1] } {}

  ASMJIT_INLINE_NODEBUG void reset() noexcept {
    _nodes[0] = nullptr;
    _nodes[1] = nullptr;
  }

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG bool empty() const noexcept { return _nodes[0] == nullptr; }
  ASMJIT_INLINE_NODEBUG NodeT* first() const noexcept { return _nodes[kNodeIndexFirst]; }
  ASMJIT_INLINE_NODEBUG NodeT* last() const noexcept { return _nodes[kNodeIndexLast]; }

  //! \}

  //! \name Utilities
  //! \{

  ASMJIT_INLINE_NODEBUG void swap(ZoneList& other) noexcept {
    std::swap(_nodes[0], other._nodes[0]);
    std::swap(_nodes[1], other._nodes[1]);
  }

  // Can be used to both append and prepend.
  inline void _addNode(NodeT* node, size_t dir) noexcept {
    NodeT* prev = _nodes[dir];

    node->_listNodes[!dir] = prev;
    _nodes[dir] = node;
    if (prev)
      prev->_listNodes[dir] = node;
    else
      _nodes[!dir] = node;
  }

  // Can be used to both append and prepend.
  inline void _insertNode(NodeT* ref, NodeT* node, size_t dir) noexcept {
    ASMJIT_ASSERT(ref != nullptr);

    NodeT* prev = ref;
    NodeT* next = ref->_listNodes[dir];

    prev->_listNodes[dir] = node;
    if (next)
      next->_listNodes[!dir] = node;
    else
      _nodes[dir] = node;

    node->_listNodes[!dir] = prev;
    node->_listNodes[ dir] = next;
  }

  ASMJIT_INLINE_NODEBUG void append(NodeT* node) noexcept { _addNode(node, kNodeIndexLast); }
  ASMJIT_INLINE_NODEBUG void prepend(NodeT* node) noexcept { _addNode(node, kNodeIndexFirst); }

  ASMJIT_INLINE_NODEBUG void insertAfter(NodeT* ref, NodeT* node) noexcept { _insertNode(ref, node, NodeT::kNodeIndexNext); }
  ASMJIT_INLINE_NODEBUG void insertBefore(NodeT* ref, NodeT* node) noexcept { _insertNode(ref, node, NodeT::kNodeIndexPrev); }

  inline NodeT* unlink(NodeT* node) noexcept {
    NodeT* prev = node->prev();
    NodeT* next = node->next();

    if (prev) { prev->_listNodes[1] = next; node->_listNodes[0] = nullptr; } else { _nodes[0] = next; }
    if (next) { next->_listNodes[0] = prev; node->_listNodes[1] = nullptr; } else { _nodes[1] = prev; }

    node->_listNodes[0] = nullptr;
    node->_listNodes[1] = nullptr;

    return node;
  }

  inline NodeT* popFirst() noexcept {
    NodeT* node = _nodes[0];
    ASMJIT_ASSERT(node != nullptr);

    NodeT* next = node->next();
    _nodes[0] = next;

    if (next) {
      next->_listNodes[0] = nullptr;
      node->_listNodes[1] = nullptr;
    }
    else {
      _nodes[1] = nullptr;
    }

    return node;
  }

  inline NodeT* pop() noexcept {
    NodeT* node = _nodes[1];
    ASMJIT_ASSERT(node != nullptr);

    NodeT* prev = node->prev();
    _nodes[1] = prev;

    if (prev) {
      prev->_listNodes[1] = nullptr;
      node->_listNodes[0] = nullptr;
    }
    else {
      _nodes[0] = nullptr;
    }

    return node;
  }

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_ZONELIST_H_INCLUDED
