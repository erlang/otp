// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A32OPERAND_H_INCLUDED
#define ASMJIT_ARM_A32OPERAND_H_INCLUDED

#include "../arm/armoperand.h"
#include "../arm/a32globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a32)

//! \addtogroup asmjit_a32
//! \{

class VecS;
class VecD;
class VecQ;

using arm::Reg;
using arm::Mem;

//! General purpose register (AArch32).
class Gp : public Reg {
public:
  ASMJIT_DEFINE_FINAL_REG(Gp, Reg, RegTraits<RegType::kARM_GpW>);

  //! Special register id.
  enum Id : uint32_t {
    //! Frame pointer register id.
    kIdFP = 11u,
    //! Stack register id.
    kIdSP = 13u,
    //! Link register id.
    kIdLR = 14u,
    //! Program counter register id.
    kIdPC = 15u
  };

  ASMJIT_INLINE_NODEBUG constexpr bool isFP() const noexcept { return id() == kIdFP; }
  ASMJIT_INLINE_NODEBUG constexpr bool isSP() const noexcept { return id() == kIdSP; }
  ASMJIT_INLINE_NODEBUG constexpr bool isLR() const noexcept { return id() == kIdLR; }
  ASMJIT_INLINE_NODEBUG constexpr bool isPC() const noexcept { return id() == kIdPC; }
};

using arm::BaseVec;
using arm::Shift;
using arm::ShiftOp;

using arm::lsl;
using arm::lsr;
using arm::asr;
using arm::ror;
using arm::rrx;

//! Vector register (AArch32).
class Vec : public BaseVec {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Vec, BaseVec)

  //! Creates a cloned register with element access.
  ASMJIT_INLINE_NODEBUG Vec at(uint32_t elementIndex) const noexcept {
    return Vec((signature() & ~kSignatureRegElementIndexMask) | (elementIndex << kSignatureRegElementIndexShift) | kSignatureRegElementFlagMask, id());
  }

  //! Cast this register to a 32-bit S register.
  ASMJIT_INLINE_NODEBUG VecS s() const noexcept;
  //! Cast this register to a 64-bit D register.
  ASMJIT_INLINE_NODEBUG VecD d() const noexcept;
  //! Cast this register to a 128-bit Q register.
  ASMJIT_INLINE_NODEBUG VecQ q() const noexcept;

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature _makeElementAccessSignature(uint32_t elementType, uint32_t elementIndex) noexcept {
    return OperandSignature{
      uint32_t(RegTraits<RegType::kARM_VecV>::kSignature)      |
      uint32_t(kSignatureRegElementFlagMask)                   |
      uint32_t(elementType << kSignatureRegElementTypeShift)   |
      uint32_t(elementIndex << kSignatureRegElementIndexShift)};
  }
};

//! 32-bit VFP/SIMD register.
class VecS : public Vec {
public:
  ASMJIT_DEFINE_FINAL_REG(VecS, Vec, RegTraits<RegType::kARM_VecS>)

  //! Creates a cloned register with element access.
  ASMJIT_INLINE_NODEBUG VecS at(uint32_t elementIndex) const noexcept {
    return VecS((signature() & ~kSignatureRegElementIndexMask) | (elementIndex << kSignatureRegElementIndexShift) | kSignatureRegElementFlagMask, id());
  }
};

//! 64-bit VFP/SIMD register.
class VecD : public Vec {
public:
  ASMJIT_DEFINE_FINAL_REG(VecD, Vec, RegTraits<RegType::kARM_VecD>)

  //! Creates a cloned register with element access.
  ASMJIT_INLINE_NODEBUG VecD at(uint32_t elementIndex) const noexcept {
    return VecD((signature() & ~kSignatureRegElementIndexMask) | (elementIndex << kSignatureRegElementIndexShift) | kSignatureRegElementFlagMask, id());
  }
};

//! 128-bit vector register.
class VecQ : public Vec {
public:
  ASMJIT_DEFINE_FINAL_REG(VecQ, Vec, RegTraits<RegType::kARM_VecQ>)

  //! Creates a cloned register with element access.
  ASMJIT_INLINE_NODEBUG VecQ at(uint32_t elementIndex) const noexcept {
    return VecQ((signature() & ~kSignatureRegElementIndexMask) | (elementIndex << kSignatureRegElementIndexShift) | kSignatureRegElementFlagMask, id());
  }
};

ASMJIT_INLINE_NODEBUG VecS Vec::s() const noexcept { return VecS(id()); }
ASMJIT_INLINE_NODEBUG VecD Vec::d() const noexcept { return VecD(id()); }
ASMJIT_INLINE_NODEBUG VecQ Vec::q() const noexcept { return VecQ(id()); }

//! Register-list of 32-bit GP registers.
class GpList : public RegListT<Gp> {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    kSignature = Signature::fromOpType(OperandType::kRegList).bits() | (Gp::kSignature & ~Signature::kOpTypeMask)
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a dummy register-list operand.
  ASMJIT_INLINE_NODEBUG constexpr GpList() noexcept
    : RegListT<Gp>(Signature{kSignature}, RegMask(0)) {}

  //! Creates a register-list operand which is the same as `other` .
  ASMJIT_INLINE_NODEBUG constexpr GpList(const GpList& other) noexcept
    : RegListT<Gp>(other) {}

  //! Creates a register-list operand initialized to the given `regMask`.
  ASMJIT_INLINE_NODEBUG explicit constexpr GpList(RegMask regMask) noexcept
    : RegListT<Gp>(Signature{kSignature}, regMask) {}

  //! Creates a register-list operand initialized to `regs`.
  ASMJIT_INLINE_NODEBUG explicit GpList(std::initializer_list<Gp> regs) noexcept
    : RegListT(Signature{kSignature}, regs) {}

  ASMJIT_INLINE_NODEBUG explicit GpList(Globals::NoInit_) noexcept
    : RegListT<Gp>(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG GpList& operator=(const GpList& other) noexcept = default;

  ASMJIT_INLINE_NODEBUG GpList& operator|=(const GpList& other) noexcept { addList(other); return *this; }
  ASMJIT_INLINE_NODEBUG GpList& operator&=(const GpList& other) noexcept { andList(other); return *this; }
  ASMJIT_INLINE_NODEBUG GpList& operator^=(const GpList& other) noexcept { xorList(other); return *this; }

  ASMJIT_INLINE_NODEBUG GpList operator|(const GpList& other) const noexcept { return GpList(list() | other.list()); }
  ASMJIT_INLINE_NODEBUG GpList operator&(const GpList& other) const noexcept { return GpList(list() & other.list()); }
  ASMJIT_INLINE_NODEBUG GpList operator^(const GpList& other) const noexcept { return GpList(list() ^ other.list()); }

  //! \}
};

//! Register-list of 32-bit (vector) S registers.
class VecSList : public RegListT<VecS> {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    kSignature = Signature::fromOpType(OperandType::kRegList).bits() | (VecS::kSignature & ~Signature::kOpTypeMask)
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a dummy register-list operand.
  ASMJIT_INLINE_NODEBUG constexpr VecSList() noexcept
    : RegListT<VecS>(Signature{kSignature}, RegMask(0)) {}

  //! Creates a register-list operand which is the same as `other` .
  ASMJIT_INLINE_NODEBUG constexpr VecSList(const VecSList& other) noexcept
    : RegListT<VecS>(other) {}

  //! Creates a register-list operand initialized to the given `regMask`.
  ASMJIT_INLINE_NODEBUG explicit constexpr VecSList(RegMask regMask) noexcept
    : RegListT<VecS>(Signature{kSignature}, regMask) {}

  //! Creates a register-list operand initialized to `regs`.
  ASMJIT_INLINE_NODEBUG explicit VecSList(std::initializer_list<VecS> regs) noexcept
    : RegListT(Signature{kSignature}, regs) {}

  ASMJIT_INLINE_NODEBUG explicit VecSList(Globals::NoInit_) noexcept
    : RegListT<VecS>(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG VecSList& operator=(const VecSList& other) noexcept = default;

  ASMJIT_INLINE_NODEBUG VecSList& operator|=(const VecSList& other) noexcept { addList(other); return *this; }
  ASMJIT_INLINE_NODEBUG VecSList& operator&=(const VecSList& other) noexcept { andList(other); return *this; }
  ASMJIT_INLINE_NODEBUG VecSList& operator^=(const VecSList& other) noexcept { xorList(other); return *this; }

  ASMJIT_INLINE_NODEBUG VecSList operator|(const VecSList& other) const noexcept { return VecSList(list() | other.list()); }
  ASMJIT_INLINE_NODEBUG VecSList operator&(const VecSList& other) const noexcept { return VecSList(list() & other.list()); }
  ASMJIT_INLINE_NODEBUG VecSList operator^(const VecSList& other) const noexcept { return VecSList(list() ^ other.list()); }

  //! \}
};

//! Register-list of 32-bit (vector) D registers.
class VecDList : public RegListT<VecD> {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    kSignature = Signature::fromOpType(OperandType::kRegList).bits() | (VecD::kSignature & ~Signature::kOpTypeMask)
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a dummy register-list operand.
  ASMJIT_INLINE_NODEBUG constexpr VecDList() noexcept
    : RegListT<VecD>(Signature{kSignature}, RegMask(0)) {}

  //! Creates a register-list operand which is the same as `other` .
  ASMJIT_INLINE_NODEBUG constexpr VecDList(const VecDList& other) noexcept
    : RegListT<VecD>(other) {}

  //! Creates a register-list operand initialized to the given `regMask`.
  ASMJIT_INLINE_NODEBUG explicit constexpr VecDList(RegMask regMask) noexcept
    : RegListT<VecD>(Signature{kSignature}, regMask) {}

  //! Creates a register-list operand initialized to `regs`.
  ASMJIT_INLINE_NODEBUG explicit VecDList(std::initializer_list<VecD> regs) noexcept
    : RegListT(Signature{kSignature}, regs) {}

  ASMJIT_INLINE_NODEBUG explicit VecDList(Globals::NoInit_) noexcept
    : RegListT<VecD>(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG VecDList& operator=(const VecDList& other) noexcept = default;

  ASMJIT_INLINE_NODEBUG VecDList& operator|=(const VecDList& other) noexcept { addList(other); return *this; }
  ASMJIT_INLINE_NODEBUG VecDList& operator&=(const VecDList& other) noexcept { andList(other); return *this; }
  ASMJIT_INLINE_NODEBUG VecDList& operator^=(const VecDList& other) noexcept { xorList(other); return *this; }

  ASMJIT_INLINE_NODEBUG VecDList operator|(const VecDList& other) const noexcept { return VecDList(list() | other.list()); }
  ASMJIT_INLINE_NODEBUG VecDList operator&(const VecDList& other) const noexcept { return VecDList(list() & other.list()); }
  ASMJIT_INLINE_NODEBUG VecDList operator^(const VecDList& other) const noexcept { return VecDList(list() ^ other.list()); }

  //! \}
};

//! Constructs a `LSL #value` shift of a register (logical shift left).
static constexpr inline Gp lsl(const Gp& reg) noexcept { return Gp(reg.signature().replacedValue<OperandSignature::kPredicateMask>(uint32_t(ShiftOp::kLSL)), reg.id()); }
//! Constructs a `LSR #value` shift of a register (logical shift right).
static constexpr inline Gp lsr(const Gp& reg) noexcept { return Gp(reg.signature().replacedValue<OperandSignature::kPredicateMask>(uint32_t(ShiftOp::kLSR)), reg.id()); }
//! Constructs a `ASR #value` shift of a register (arithmetic shift right).
static constexpr inline Gp asr(const Gp& reg) noexcept { return Gp(reg.signature().replacedValue<OperandSignature::kPredicateMask>(uint32_t(ShiftOp::kASR)), reg.id()); }
//! Constructs a `ROR #value` shift of a register (rotate right).
static constexpr inline Gp ror(const Gp& reg) noexcept { return Gp(reg.signature().replacedValue<OperandSignature::kPredicateMask>(uint32_t(ShiftOp::kROR)), reg.id()); }
//! Constructs a `sop #value` shift of a register.
static constexpr inline Gp shift(const Gp& reg, ShiftOp sop) noexcept { return Gp(reg.signature().replacedValue<OperandSignature::kPredicateMask>(uint32_t(sop)), reg.id()); }

#ifndef _DOXYGEN
namespace regs {
#endif

//! Creates a 32-bit R register operand (ARM).
//!
//! \note R register is the same as W (word) register. W registers are used specifically by AArch64, but in AsmJit
//! they refer to the same thing and they have the same interface. People that target both architectures may favor
//! using just w() as that's a universal way to refer to a 32-bit AArch32/AArch64 GP register.
static ASMJIT_INLINE_NODEBUG constexpr Gp r(uint32_t id) noexcept { return Gp(id); }
//! Creates a 32-bit S register operand.
static ASMJIT_INLINE_NODEBUG constexpr VecS s(uint32_t id) noexcept { return VecS(id); }
//! Creates a 64-bit D register operand.
static ASMJIT_INLINE_NODEBUG constexpr VecD d(uint32_t id) noexcept { return VecD(id); }
//! Creates a 1282-bit V register operand.
static ASMJIT_INLINE_NODEBUG constexpr VecQ q(uint32_t id) noexcept { return VecQ(id); }

static constexpr Gp r0 = Gp(0);
static constexpr Gp r1 = Gp(1);
static constexpr Gp r2 = Gp(2);
static constexpr Gp r3 = Gp(3);
static constexpr Gp r4 = Gp(4);
static constexpr Gp r5 = Gp(5);
static constexpr Gp r6 = Gp(6);
static constexpr Gp r7 = Gp(7);
static constexpr Gp r8 = Gp(8);
static constexpr Gp r9 = Gp(9);
static constexpr Gp r10 = Gp(10);
static constexpr Gp r11 = Gp(11);
static constexpr Gp r12 = Gp(12);
static constexpr Gp r13 = Gp(13);
static constexpr Gp r14 = Gp(14);
static constexpr Gp r15 = Gp(15);

static constexpr Gp fp = Gp(Gp::kIdFP);
static constexpr Gp sp = Gp(Gp::kIdSP);
static constexpr Gp lr = Gp(Gp::kIdLR);
static constexpr Gp pc = Gp(Gp::kIdPC);

static constexpr VecS s0 = VecS(0);
static constexpr VecS s1 = VecS(1);
static constexpr VecS s2 = VecS(2);
static constexpr VecS s3 = VecS(3);
static constexpr VecS s4 = VecS(4);
static constexpr VecS s5 = VecS(5);
static constexpr VecS s6 = VecS(6);
static constexpr VecS s7 = VecS(7);
static constexpr VecS s8 = VecS(8);
static constexpr VecS s9 = VecS(9);
static constexpr VecS s10 = VecS(10);
static constexpr VecS s11 = VecS(11);
static constexpr VecS s12 = VecS(12);
static constexpr VecS s13 = VecS(13);
static constexpr VecS s14 = VecS(14);
static constexpr VecS s15 = VecS(15);
static constexpr VecS s16 = VecS(16);
static constexpr VecS s17 = VecS(17);
static constexpr VecS s18 = VecS(18);
static constexpr VecS s19 = VecS(19);
static constexpr VecS s20 = VecS(20);
static constexpr VecS s21 = VecS(21);
static constexpr VecS s22 = VecS(22);
static constexpr VecS s23 = VecS(23);
static constexpr VecS s24 = VecS(24);
static constexpr VecS s25 = VecS(25);
static constexpr VecS s26 = VecS(26);
static constexpr VecS s27 = VecS(27);
static constexpr VecS s28 = VecS(28);
static constexpr VecS s29 = VecS(29);
static constexpr VecS s30 = VecS(30);
static constexpr VecS s31 = VecS(31);

static constexpr VecD d0 = VecD(0);
static constexpr VecD d1 = VecD(1);
static constexpr VecD d2 = VecD(2);
static constexpr VecD d3 = VecD(3);
static constexpr VecD d4 = VecD(4);
static constexpr VecD d5 = VecD(5);
static constexpr VecD d6 = VecD(6);
static constexpr VecD d7 = VecD(7);
static constexpr VecD d8 = VecD(8);
static constexpr VecD d9 = VecD(9);
static constexpr VecD d10 = VecD(10);
static constexpr VecD d11 = VecD(11);
static constexpr VecD d12 = VecD(12);
static constexpr VecD d13 = VecD(13);
static constexpr VecD d14 = VecD(14);
static constexpr VecD d15 = VecD(15);
static constexpr VecD d16 = VecD(16);
static constexpr VecD d17 = VecD(17);
static constexpr VecD d18 = VecD(18);
static constexpr VecD d19 = VecD(19);
static constexpr VecD d20 = VecD(20);
static constexpr VecD d21 = VecD(21);
static constexpr VecD d22 = VecD(22);
static constexpr VecD d23 = VecD(23);
static constexpr VecD d24 = VecD(24);
static constexpr VecD d25 = VecD(25);
static constexpr VecD d26 = VecD(26);
static constexpr VecD d27 = VecD(27);
static constexpr VecD d28 = VecD(28);
static constexpr VecD d29 = VecD(29);
static constexpr VecD d30 = VecD(30);
static constexpr VecD d31 = VecD(31);

static constexpr VecQ q0 = VecQ(0);
static constexpr VecQ q1 = VecQ(1);
static constexpr VecQ q2 = VecQ(2);
static constexpr VecQ q3 = VecQ(3);
static constexpr VecQ q4 = VecQ(4);
static constexpr VecQ q5 = VecQ(5);
static constexpr VecQ q6 = VecQ(6);
static constexpr VecQ q7 = VecQ(7);
static constexpr VecQ q8 = VecQ(8);
static constexpr VecQ q9 = VecQ(9);
static constexpr VecQ q10 = VecQ(10);
static constexpr VecQ q11 = VecQ(11);
static constexpr VecQ q12 = VecQ(12);
static constexpr VecQ q13 = VecQ(13);
static constexpr VecQ q14 = VecQ(14);
static constexpr VecQ q15 = VecQ(15);

#ifndef _DOXYGEN
} // {regs}

// Make `a32::regs` accessible through `a32` namespace as well.
using namespace regs;
#endif

//! Creates `[base, offset]` memory operand (offset mode) (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset);
}

//! Creates `[base, offset]!` memory operand (pre-index mode) (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_pre(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset, OperandSignature::fromValue<Mem::kSignatureMemOffsetModeMask>(OffsetMode::kPreIndex));
}

//! Creates `[base], offset` memory operand (post-index mode) (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_post(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset, OperandSignature::fromValue<Mem::kSignatureMemOffsetModeMask>(OffsetMode::kPostIndex));
}

//! Creates `[base, index]` memory operand (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index);
}

//! Creates `[base, index]!` memory operand (pre-index mode) (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_pre(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index, OperandSignature::fromValue<Mem::kSignatureMemOffsetModeMask>(OffsetMode::kPreIndex));
}

//! Creates `[base], index` memory operand (post-index mode) (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_post(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index, OperandSignature::fromValue<Mem::kSignatureMemOffsetModeMask>(OffsetMode::kPostIndex));
}

//! Creates `[base, index, SHIFT_OP #shift]` memory operand (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Gp& base, const Gp& index, const Shift& shift) noexcept {
  return Mem(base, index, shift);
}

//! Creates `[base, offset]` memory operand (AArch32).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Label& base, int32_t offset = 0) noexcept {
  return Mem(base, offset);
}

//! \}

ASMJIT_END_SUB_NAMESPACE

//! \cond INTERNAL
ASMJIT_BEGIN_NAMESPACE
ASMJIT_DEFINE_TYPE_ID(a32::Gp, TypeId::kInt32);
ASMJIT_DEFINE_TYPE_ID(a32::VecS, TypeId::kFloat32x1);
ASMJIT_DEFINE_TYPE_ID(a32::VecD, TypeId::kFloat64x1);
ASMJIT_DEFINE_TYPE_ID(a32::VecQ, TypeId::kInt32x4);
ASMJIT_END_NAMESPACE
//! \endcond

#endif // ASMJIT_ARM_A32OPERAND_H_INCLUDED
