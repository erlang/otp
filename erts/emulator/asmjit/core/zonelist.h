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

  inline ZoneListNode() noexcept
    : _listNodes { nullptr, nullptr } {}

  inline ZoneListNode(ZoneListNode&& other) noexcept
    : _listNodes { other._listNodes[0], other._listNodes[1] } {}

  //! \}

  //! \name Accessors
  //! \{

  inline bool hasPrev() const noexcept { return _listNodes[kNodeIndexPrev] != nullptr; }
  inline bool hasNext() const noexcept { return _listNodes[kNodeIndexNext] != nullptr; }

  inline NodeT* prev() const noexcept { return _listNodes[kNodeIndexPrev]; }
  inline NodeT* next() const noexcept { return _listNodes[kNodeIndexNext]; }

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

  NodeT* _nodes[2];

  //! \}

  //! \name Construction & Destruction
  //! \{

  inline ZoneList() noexcept
    : _nodes { nullptr, nullptr } {}

  inline ZoneList(ZoneList&& other) noexcept
    : _nodes { other._nodes[0], other._nodes[1] } {}

  inline void reset() noexcept {
    _nodes[0] = nullptr;
    _nodes[1] = nullptr;
  }

  //! \}

  //! \name Accessors
  //! \{

  inline bool empty() const noexcept { return _nodes[0] == nullptr; }
  inline NodeT* first() const noexcept { return _nodes[kNodeIndexFirst]; }
  inline NodeT* last() const noexcept { return _nodes[kNodeIndexLast]; }

  //! \}

  //! \name Utilities
  //! \{

  inline void swap(ZoneList& other) noexcept {
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

  inline void append(NodeT* node) noexcept { _addNode(node, kNodeIndexLast); }
  inline void prepend(NodeT* node) noexcept { _addNode(node, kNodeIndexFirst); }

  inline void insertAfter(NodeT* ref, NodeT* node) noexcept { _insertNode(ref, node, NodeT::kNodeIndexNext); }
  inline void insertBefore(NodeT* ref, NodeT* node) noexcept { _insertNode(ref, node, NodeT::kNodeIndexPrev); }

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
