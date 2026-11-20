// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_AXL_INPLACE_BIT_SET_H_INCLUDED
#define ASMJIT_AXL_INPLACE_BIT_SET_H_INCLUDED

#include <asmjit/axl/bit_set_ops.h>
#include <asmjit/axl/commons.h>
#include <asmjit/axl/inplace_array.h>

ASMJIT_BEGIN_SUB_NAMESPACE(axl)

//! \addtogroup asmjit_axl
//! \{

//! Inline bit-array that uses a fixed storage.
template<size_t N>
class InplaceBitSet {
public:
  //! \cond INTERNAL
  //! \name Constants
  //! \{

  static inline constexpr size_t kNumBitWords = (N + bit_size_of<BitWord> - 1) / bit_size_of<BitWord>;
  static inline constexpr size_t kNumBits = kNumBitWords * bit_size_of<BitWord>;

  //! \}
  //! \endcond

  //! \name Types
  //! \{

  //! Type of the underlying array used to store all bits.
  using UnderlyingArray = InplaceArray<BitWord, kNumBitWords>;
  //! Iterator that can iterate all bits set.
  using Iterator = BitVectorIterator<BitWord>;

  //! \}

  //! \name Members
  //! \{

  //! Underlying storage.
  UnderlyingArray _bits {};

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_CONSTEXPR InplaceBitSet() noexcept {}
  ASMJIT_INLINE_CONSTEXPR InplaceBitSet(const InplaceBitSet& other) noexcept = default;

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Copy operator.
  ASMJIT_INLINE_CONSTEXPR InplaceBitSet& operator=(const InplaceBitSet& other) noexcept = default;

  //! Tests whether this bit is equal with `other`, which means it has the same bits set to 1.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR bool operator==(const InplaceBitSet& other) const noexcept { return _bits.equals(other._bits); }

  //! Tests whether this bit isn't equal with `other`, which means it doesn't have the same bits set to 1 as `other`.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR bool operator!=(const InplaceBitSet& other) const noexcept { return !_bits.equals(other._bits); }

  //! \}

  //! \name Equality & Comparison
  //! \{

  //! Tests whether this bit array has the same bits set as `other`.
  ASMJIT_INLINE_CONSTEXPR bool equals(const InplaceBitSet& other) const noexcept { return _bits.equals(other._bits); }

  //! \}

  //! \name Accessors
  //! \{

  template<typename Operator>
  ASMJIT_INLINE BitWord aggregate(BitWord initial_value = 0) const noexcept { return _bits.template reduce<Operator>(initial_value); }

  //! Returns a non-owning span of the underlying array of BitWords (const).
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG Span<const BitWord> span() const noexcept { return Span<const BitWord>(_bits._data, kNumBitWords); }

  //! Returns a non-owning span of the underlying array of BitWords (mutable).
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG Span<BitWord> span_mut() noexcept { return Span<BitWord>(_bits._data, kNumBitWords); }

  //! Returns true if there are no bits set.
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR bool is_empty() const noexcept { return aggregate<Or>(0) == 0; }

  //! Returns the underlying array used by InplaceBitSet as \ref BitWord[] (const).
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG const BitWord* data() const noexcept { return _bits.data(); }

  //! Returns the underlying array used by InplaceBitSet as \ref BitWord[].
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG BitWord* data_mut() noexcept { return _bits.data(); }

  //! Returns the number of BitWords returned by \ref data().
  [[nodiscard]]
  ASMJIT_INLINE_CONSTEXPR size_t bit_word_count() const noexcept { return kNumBitWords; }

  //! Returns \ref BitVectorIterator, that can be used to iterate over all bits set efficiently.
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG Iterator iterator() const noexcept { return Iterator(_bits.span()); }

  //! Tests whether the bit at `index` is set to 1.
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has(auto index) const noexcept {
    ASMJIT_ASSERT(size_t(index) < kNumBits);

    size_t idx = size_t(index) / bit_size_of<BitWord>;
    size_t bit = size_t(index) % bit_size_of<BitWord>;

    return bool((_bits[idx] >> bit) & 0x1);
  }

  //! \cond NONE
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has_any(auto index) const noexcept { return has(index); }
  //! \endcond

  //! Tests whether at least on bit at the given index is set to 1.
  template<typename IndexT, typename... Args>
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has_any(const IndexT& index, Args&&... other_indexes) const noexcept {
    return bool(unsigned(has(index)) | unsigned(has_any(forward<Args>(other_indexes)...)));
  }

  //! Tests whether all bits at the given index are set to 1.
  [[nodiscard]]
  ASMJIT_INLINE_NODEBUG bool has_all(const InplaceBitSet& other) const noexcept {
    uint32_t result = 1;
    for (size_t i = 0; i < kNumBitWords; i++) {
      result &= uint32_t((_bits[i] & other._bits[i]) == other._bits[i]);
    }
    return bool(result);
  }

  //! \}

  //! \name Manipulation
  //! \{

  //! Clears all bits set.
  ASMJIT_INLINE_NODEBUG void reset() noexcept { _bits.fill(0); }

  //! Sets bit at `index` to 1.
  template<typename IndexT>
  ASMJIT_INLINE void set(const IndexT& index) noexcept {
    ASMJIT_ASSERT(as_std_uint(index) < kNumBits);
    _bits[as_std_uint(index) / bit_size_of<BitWord>] |= shl_wrap(BitWord(1), index);
  }

  //! Sets all bits at `index` and `other_indexes` to 1.
  template<typename IndexT, typename... Args>
  ASMJIT_INLINE void set(const IndexT& index, Args&&... other_indexes) noexcept {
    set(index);
    set(forward<Args>(other_indexes)...);
  }

  template<typename IndexT>
  ASMJIT_INLINE void set_if(bool condition, const IndexT& index) noexcept {
    ASMJIT_ASSERT(as_std_uint(index) < kNumBits);

    size_t idx = size_t(index) / bit_size_of<BitWord>;
    size_t bit = size_t(index) % bit_size_of<BitWord>;

    _bits[idx] |= BitWord(condition) << bit;
  }

  template<typename IndexT, typename... Args>
  ASMJIT_INLINE void set_if(bool condition, const IndexT& index, Args&&... other_indexes) noexcept {
    set_if(condition, index);
    set_if(condition, forward<Args>(other_indexes)...);
  }

  //! Sets bit at `index` to 0.
  template<typename IndexT>
  ASMJIT_INLINE void clear(const IndexT& index) noexcept {
    ASMJIT_ASSERT(as_std_uint(index) < kNumBits);
    _bits[as_std_uint(index) / bit_size_of<BitWord>] &= ~shl_wrap(BitWord(1), index);
  }

  //! Sets all bits at `index` and `other_indexes` to 0.
  template<typename IndexT, typename... Args>
  ASMJIT_INLINE void clear(const IndexT& index, Args&&... other_indexes) noexcept {
    clear(index);
    clear(forward<Args>(other_indexes)...);
  }

  template<typename Operator>
  ASMJIT_INLINE void combine(const InplaceBitSet& other) noexcept { _bits.template combine<Operator>(other._bits); }

  //! \}
};

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_AXL_INPLACE_BIT_SET_H_INCLUDED
