// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_UJIT_UNI_CONDITION_H_INCLUDED
#define ASMJIT_UJIT_UNI_CONDITION_H_INCLUDED

#include <asmjit/ujit/ujit_base.h>
#include <asmjit/ujit/uni_op.h>

#if !defined(ASMJIT_NO_UJIT)

ASMJIT_BEGIN_SUB_NAMESPACE(ujit)

//! \addtogroup asmjit_ujit
//! \{

//! Condition represents either a condition or an assignment operation that can be checked.
class UniCondition {
public:
  //! \name Members
  //! \{

  UniOpCond op;
  CondCode cond;
  Operand a;
  Operand b;

  //! \}

  //! \name Construction & Destruction
  //! \{

  ASMJIT_INLINE_NODEBUG UniCondition(UniOpCond op, CondCode cond, const Operand& a, const Operand& b) noexcept
    : op(op),
      cond(cond),
      a(a),
      b(b) {}

  ASMJIT_INLINE_NODEBUG UniCondition(const UniCondition& other) noexcept = default;

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG UniCondition& operator=(const UniCondition& other) noexcept = default;

  //! \}
};

//! Constructs a condition that would be `true` when `a = (a & b)` becomes zero.
static ASMJIT_INLINE UniCondition and_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAnd, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a & b)` becomes zero.
static ASMJIT_INLINE UniCondition and_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAnd, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a & b)` becomes zero.
static ASMJIT_INLINE UniCondition and_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAnd, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a & b)` becomes non-zero.
static ASMJIT_INLINE UniCondition and_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAnd, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a & b)` becomes non-zero.
static ASMJIT_INLINE UniCondition and_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAnd, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a & b)` becomes non-zero.
static ASMJIT_INLINE UniCondition and_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAnd, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when `a = (a | b)` becomes zero.
static ASMJIT_INLINE UniCondition or_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignOr, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a | b)` becomes zero.
static ASMJIT_INLINE UniCondition or_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignOr, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a | b)` becomes zero.
static ASMJIT_INLINE UniCondition or_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignOr, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a | b)` becomes non-zero.
static ASMJIT_INLINE UniCondition or_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignOr, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a | b)` becomes non-zero.
static ASMJIT_INLINE UniCondition or_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignOr, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a | b)` becomes non-zero.
static ASMJIT_INLINE UniCondition or_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignOr, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when `a = (a ^ b)` becomes zero.
static ASMJIT_INLINE UniCondition xor_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignXor, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a ^ b)` becomes zero.
static ASMJIT_INLINE UniCondition xor_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignXor, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a ^ b)` becomes zero.
static ASMJIT_INLINE UniCondition xor_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignXor, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a ^ b)` becomes non-zero.
static ASMJIT_INLINE UniCondition xor_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignXor, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a ^ b)` becomes non-zero.
static ASMJIT_INLINE UniCondition xor_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignXor, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a ^ b)` becomes non-zero.
static ASMJIT_INLINE UniCondition xor_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignXor, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when `a = (a + b)` becomes zero.
static ASMJIT_INLINE UniCondition add_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` becomes zero.
static ASMJIT_INLINE UniCondition add_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` becomes zero.
static ASMJIT_INLINE UniCondition add_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` becomes non-zero.
static ASMJIT_INLINE UniCondition add_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` becomes non-zero.
static ASMJIT_INLINE UniCondition add_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` becomes non-zero.
static ASMJIT_INLINE UniCondition add_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when `a = (a + b)` wraps (sets carry flag).
static ASMJIT_INLINE UniCondition add_c(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kCarry, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` wraps (sets carry flag).
static ASMJIT_INLINE UniCondition add_c(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kCarry, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` wraps (sets carry flag).
static ASMJIT_INLINE UniCondition add_c(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kCarry, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` doesn't wrap (doesn't set carry flag).
static ASMJIT_INLINE UniCondition add_nc(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotCarry, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` doesn't wrap (doesn't set carry flag).
static ASMJIT_INLINE UniCondition add_nc(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotCarry, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` doesn't wrap (doesn't set carry flag).
static ASMJIT_INLINE UniCondition add_nc(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotCarry, a, b); }

//! Constructs a condition that would be `true` when `a = (a + b)` ends with the msb/sign bit set.
static ASMJIT_INLINE UniCondition add_s(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` ends with the msb/sign bit set.
static ASMJIT_INLINE UniCondition add_s(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` ends with the msb/sign bit set.
static ASMJIT_INLINE UniCondition add_s(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` ends with the msb/sign bit unset.
static ASMJIT_INLINE UniCondition add_ns(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` ends with the msb/sign bit unset.
static ASMJIT_INLINE UniCondition add_ns(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a + b)` ends with the msb/sign bit unset.
static ASMJIT_INLINE UniCondition add_ns(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignAdd, CondCode::kNotSign, a, b); }

//! Constructs a condition that would be `true` when `a = (a - b)` becomes zero.
static ASMJIT_INLINE UniCondition sub_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` becomes zero.
static ASMJIT_INLINE UniCondition sub_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` becomes zero.
static ASMJIT_INLINE UniCondition sub_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` becomes non-zero.
static ASMJIT_INLINE UniCondition sub_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` becomes non-zero.
static ASMJIT_INLINE UniCondition sub_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` becomes non-zero.
static ASMJIT_INLINE UniCondition sub_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when `a = (a - b)` wraps.
static ASMJIT_INLINE UniCondition sub_c(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedLT, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` wraps.
static ASMJIT_INLINE UniCondition sub_c(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedLT, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` wraps.
static ASMJIT_INLINE UniCondition sub_c(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedLT, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` doesn't wrap.
static ASMJIT_INLINE UniCondition sub_nc(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedGE, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` doesn't wrap.
static ASMJIT_INLINE UniCondition sub_nc(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedGE, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` doesn't wrap.
static ASMJIT_INLINE UniCondition sub_nc(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedGE, a, b); }

//! Constructs a condition that would be `true` when `a = (a - b)` ends with the msb/sign bit set.
static ASMJIT_INLINE UniCondition sub_s(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` ends with the msb/sign bit set.
static ASMJIT_INLINE UniCondition sub_s(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` ends with the msb/sign bit set.
static ASMJIT_INLINE UniCondition sub_s(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` ends with the msb/sign bit unset.
static ASMJIT_INLINE UniCondition sub_ns(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kNotSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` ends with the msb/sign bit unset.
static ASMJIT_INLINE UniCondition sub_ns(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kNotSign, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` ends with the msb/sign bit unset.
static ASMJIT_INLINE UniCondition sub_ns(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kNotSign, a, b); }

//! Constructs a condition that would be `true` when `a = (a - b)` is greater than zero (and didn't wrap-around).
static ASMJIT_INLINE UniCondition sub_ugt(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedGT, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` is greater than zero (and didn't wrap-around).
static ASMJIT_INLINE UniCondition sub_ugt(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedGT, a, b); }
//! Constructs a condition that would be `true` when `a = (a - b)` is greater than zero (and didn't wrap-around).
static ASMJIT_INLINE UniCondition sub_ugt(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignSub, CondCode::kUnsignedGT, a, b); }

//! Constructs a condition that would be `true` when `a = (a << b)` becomes zero.
static ASMJIT_INLINE UniCondition shr_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignShr, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a << b)` becomes zero.
static ASMJIT_INLINE UniCondition shr_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignShr, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a << b)` becomes zero.
static ASMJIT_INLINE UniCondition shr_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignShr, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a << b)` becomes non-zero.
static ASMJIT_INLINE UniCondition shr_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kAssignShr, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a << b)` becomes non-zero.
static ASMJIT_INLINE UniCondition shr_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kAssignShr, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a = (a << b)` becomes non-zero.
static ASMJIT_INLINE UniCondition shr_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kAssignShr, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when `a == b)`.
static ASMJIT_INLINE UniCondition cmp_eq(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kEqual, a, b); }
//! Constructs a condition that would be `true` when `a == b)`.
static ASMJIT_INLINE UniCondition cmp_eq(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kEqual, a, b); }
//! Constructs a condition that would be `true` when `a == b)`.
static ASMJIT_INLINE UniCondition cmp_eq(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kEqual, a, b); }

//! Constructs a condition that would be `true` when `a != b)`.
static ASMJIT_INLINE UniCondition cmp_ne(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kNotEqual, a, b); }
//! Constructs a condition that would be `true` when `a != b)`.
static ASMJIT_INLINE UniCondition cmp_ne(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kNotEqual, a, b); }
//! Constructs a condition that would be `true` when `a != b)`.
static ASMJIT_INLINE UniCondition cmp_ne(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kNotEqual, a, b); }

//! Constructs a condition that would be `true` when `a < b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_lt(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedLT, a, b); }
//! Constructs a condition that would be `true` when `a < b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_lt(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedLT, a, b); }
//! Constructs a condition that would be `true` when `a < b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_lt(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedLT, a, b); }

//! Constructs a condition that would be `true` when `a <= b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_le(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedLE, a, b); }
//! Constructs a condition that would be `true` when `a <= b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_le(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedLE, a, b); }
//! Constructs a condition that would be `true` when `a <= b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_le(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedLE, a, b); }

//! Constructs a condition that would be `true` when `a > b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_gt(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedGT, a, b); }
//! Constructs a condition that would be `true` when `a > b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_gt(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedGT, a, b); }
//! Constructs a condition that would be `true` when `a > b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_gt(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedGT, a, b); }

//! Constructs a condition that would be `true` when `a >= b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_ge(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedGE, a, b); }
//! Constructs a condition that would be `true` when `a >= b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_ge(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedGE, a, b); }
//! Constructs a condition that would be `true` when `a >= b` (signed comparison).
static ASMJIT_INLINE UniCondition scmp_ge(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kSignedGE, a, b); }

//! Constructs a condition that would be `true` when `a < b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_lt(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedLT, a, b); }
//! Constructs a condition that would be `true` when `a < b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_lt(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedLT, a, b); }
//! Constructs a condition that would be `true` when `a < b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_lt(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedLT, a, b); }

//! Constructs a condition that would be `true` when `a <= b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_le(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedLE, a, b); }
//! Constructs a condition that would be `true` when `a <= b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_le(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedLE, a, b); }
//! Constructs a condition that would be `true` when `a <= b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_le(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedLE, a, b); }

//! Constructs a condition that would be `true` when `a > b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_gt(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedGT, a, b); }
//! Constructs a condition that would be `true` when `a > b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_gt(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedGT, a, b); }
//! Constructs a condition that would be `true` when `a > b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_gt(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedGT, a, b); }

//! Constructs a condition that would be `true` when `a >= b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_ge(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedGE, a, b); }
//! Constructs a condition that would be `true` when `a >= b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_ge(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedGE, a, b); }
//! Constructs a condition that would be `true` when `a >= b` (unsigned comparison).
static ASMJIT_INLINE UniCondition ucmp_ge(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kUnsignedGE, a, b); }

//! Constructs a condition that would be `true` when `a` is zero.
static ASMJIT_INLINE UniCondition test_z(const UniGp& a) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kEqual, a, Imm(0)); }
//! Constructs a condition that would be `true` when `a` is non-zero.
static ASMJIT_INLINE UniCondition test_nz(const UniGp& a) noexcept { return UniCondition(UniOpCond::kCompare, CondCode::kNotEqual, a, Imm(0)); }

//! Constructs a condition that would be `true` when `a & b` is zero.
static ASMJIT_INLINE UniCondition test_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kTest, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a & b` is zero.
static ASMJIT_INLINE UniCondition test_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kTest, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a & b` is zero.
static ASMJIT_INLINE UniCondition test_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kTest, CondCode::kZero, a, b); }
//! Constructs a condition that would be `true` when `a & b` is non-zero.
static ASMJIT_INLINE UniCondition test_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kTest, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a & b` is non-zero.
static ASMJIT_INLINE UniCondition test_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kTest, CondCode::kNotZero, a, b); }
//! Constructs a condition that would be `true` when `a & b` is non-zero.
static ASMJIT_INLINE UniCondition test_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kTest, CondCode::kNotZero, a, b); }

//! Constructs a condition that would be `true` when a bit in `a` at `b` is zero (`((a >> b) & 1) == 0`).
static ASMJIT_INLINE UniCondition bt_z(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kBitTest, CondCode::kBTZero, a, b); }
//! Constructs a condition that would be `true` when a bit in `a` at `b` is zero (`((a >> b) & 1) == 0`).
static ASMJIT_INLINE UniCondition bt_z(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kBitTest, CondCode::kBTZero, a, b); }
//! Constructs a condition that would be `true` when a bit in `a` at `b` is zero (`((a >> b) & 1) == 0`).
static ASMJIT_INLINE UniCondition bt_z(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kBitTest, CondCode::kBTZero, a, b); }
//! Constructs a condition that would be `true` when a bit in `a` at `b` is non-zero (`((a >> b) & 1) == 1`).
static ASMJIT_INLINE UniCondition bt_nz(const UniGp& a, const UniGp& b) noexcept { return UniCondition(UniOpCond::kBitTest, CondCode::kBTNotZero, a, b); }
//! Constructs a condition that would be `true` when a bit in `a` at `b` is non-zero (`((a >> b) & 1) == 1`).
static ASMJIT_INLINE UniCondition bt_nz(const UniGp& a, const Mem& b) noexcept { return UniCondition(UniOpCond::kBitTest, CondCode::kBTNotZero, a, b); }
//! Constructs a condition that would be `true` when a bit in `a` at `b` is non-zero (`((a >> b) & 1) == 1`).
static ASMJIT_INLINE UniCondition bt_nz(const UniGp& a, const Imm& b) noexcept { return UniCondition(UniOpCond::kBitTest, CondCode::kBTNotZero, a, b); }

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // !ASMJIT_NO_UJIT
#endif // ASMJIT_UJIT_UNI_CONDITION_H_INCLUDED
