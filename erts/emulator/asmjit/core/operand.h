// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_OPERAND_H_INCLUDED
#define ASMJIT_CORE_OPERAND_H_INCLUDED

#include "../core/archcommons.h"
#include "../core/support.h"
#include "../core/type.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_assembler
//! \{

//! Operand type used by \ref Operand_.
enum class OperandType : uint32_t {
  //! Not an operand or not initialized.
  kNone = 0,
  //! Operand is a register.
  kReg = 1,
  //! Operand is a memory.
  kMem = 2,
  //! Operand is a register-list.
  kRegList = 3,
  //! Operand is an immediate value.
  kImm = 4,
  //! Operand is a label.
  kLabel = 5,

  //! Maximum value of `OperandType`.
  kMaxValue = kRegList
};

static_assert(uint32_t(OperandType::kMem) == uint32_t(OperandType::kReg) + 1,
              "AsmJit requires that `OperandType::kMem` equals to `OperandType::kReg + 1`");

//! Register mask is a convenience typedef that describes a mask where each bit describes a physical register id
//! in the same \ref RegGroup. At the moment 32 bits are enough as AsmJit doesn't support any architecture that
//! would provide more than 32 registers for a register group.
typedef uint32_t RegMask;

//! Register type.
//!
//! Provides a unique type that can be used to identify a register or its view.
enum class RegType : uint8_t {
  //! No register - unused, invalid, multiple meanings.
  kNone = 0,

  //! This is not a register type. This value is reserved for a \ref Label that's used in \ref BaseMem as a base.
  //!
  //! Label tag is used as a sub-type, forming a unique signature across all operand types as 0x1 is never associated
  //! with any register type. This means that a memory operand's BASE register can be constructed from virtually any
  //! operand (register vs. label) by just assigning its type (register type or label-tag) and operand id.
  kLabelTag = 1,

  //! Universal type describing program counter (PC) or instruction pointer (IP) register, if the target architecture
  //! actually exposes it as a separate register type, which most modern architectures do.
  kPC = 2,

  //! 8-bit low general purpose register (X86).
  kGp8Lo = 3,
  //! 8-bit high general purpose register (X86).
  kGp8Hi = 4,
  //! 16-bit general purpose register (X86).
  kGp16 = 5,
  //! 32-bit general purpose register (X86|AArch32|AArch64).
  kGp32 = 6,
  //! 64-bit general purpose register (X86|AArch64).
  kGp64 = 7,
  //! 8-bit view of a vector register (AArch64).
  kVec8 = 8,
  //! 16-bit view of a vector register (AArch64).
  kVec16 = 9,
  //! 32-bit view of a vector register (AArch32|AArch64).
  kVec32 = 10,
  //! 64-bit view of a vector register (AArch32|AArch64).
  //!
  //! \note This is never used for MMX registers on X86, MMX registers have its own category.
  kVec64 = 11,
  //! 128-bit view of a vector register (X86|AArch32|AArch64).
  kVec128 = 12,
  //! 256-bit view of a vector register (X86).
  kVec256 = 13,
  //! 512-bit view of a vector register (X86).
  kVec512 = 14,
  //! 1024-bit view of a vector register (future).
  kVec1024 = 15,
  //! View of a vector register, which width is implementation specific (AArch64).
  kVecNLen = 16,

  //! Mask register (X86).
  kMask = 17,

  //! Start of architecture dependent register types.
  kExtra = 18,

  // X86 Specific Register Types
  // ---------------------------

  //! Instruction pointer (RIP), only addressable in \ref x86::Mem in 64-bit targets.
  kX86_Rip = kPC,
  //! Low GPB register (AL, BL, CL, DL, ...).
  kX86_GpbLo = kGp8Lo,
  //! High GPB register (AH, BH, CH, DH only).
  kX86_GpbHi = kGp8Hi,
  //! GPW register.
  kX86_Gpw = kGp16,
  //! GPD register.
  kX86_Gpd = kGp32,
  //! GPQ register (64-bit).
  kX86_Gpq = kGp64,
  //! XMM register (SSE+).
  kX86_Xmm = kVec128,
  //! YMM register (AVX+).
  kX86_Ymm = kVec256,
  //! ZMM register (AVX512+).
  kX86_Zmm = kVec512,
  //! K register (AVX512+).
  kX86_KReg = kMask,
  //! MMX register.
  kX86_Mm = kExtra + 0,
  //! Segment register (None, ES, CS, SS, DS, FS, GS).
  kX86_SReg = kExtra + 1,
  //! Control register (CR).
  kX86_CReg = kExtra + 2,
  //! Debug register (DR).
  kX86_DReg = kExtra + 3,
  //! FPU (x87) register.
  kX86_St = kExtra + 4,
  //! Bound register (BND).
  kX86_Bnd = kExtra + 5,
  //! TMM register (AMX_TILE)
  kX86_Tmm = kExtra + 6,

  // ARM Specific Register Types
  // ---------------------------

  //! Program pointer (PC) register (AArch64).
  kARM_PC = kPC,
  //! 32-bit general purpose register (R or W).
  kARM_GpW = kGp32,
  //! 64-bit general purpose register (X).
  kARM_GpX = kGp64,
  //! 8-bit view of VFP/ASIMD register (B).
  kARM_VecB = kVec8,
  //! 16-bit view of VFP/ASIMD register (H).
  kARM_VecH = kVec16,
  //! 32-bit view of VFP/ASIMD register (S).
  kARM_VecS = kVec32,
  //! 64-bit view of VFP/ASIMD register (D).
  kARM_VecD = kVec64,
  //! 128-bit view of VFP/ASIMD register (Q).
  kARM_VecQ = kVec128,
  //! 128-bit view of VFP/ASIMD register (V).
  kARM_VecV = kVec128,

  //! Maximum value of `RegType`.
  kMaxValue = 31
};
ASMJIT_DEFINE_ENUM_COMPARE(RegType)

//! Register group.
//!
//! Provides a unique value that identifies groups of registers and their views.
enum class RegGroup : uint8_t {
  //! General purpose register group compatible with all backends.
  kGp = 0,
  //! Vector register group compatible with all backends.
  //!
  //! Describes X86 XMM|YMM|ZMM registers ARM/AArch64 V registers.
  kVec = 1,

  //! Mask register group compatible with all backends that can use masking.
  kMask = 2,
  //! Extra virtual group #3 that can be used by Compiler for register allocation.
  kExtraVirt3 = 3,

  //! Program counter group.
  kPC = 4,

  //! Extra non-virtual group that can be used by registers not managed by Compiler.
  kExtraNonVirt = 5,

  // X86 Specific Register Groups
  // ----------------------------

  //! K register group (KReg) - maps to \ref RegGroup::kMask (X86, X86_64).
  kX86_K = kMask,
  //! MMX register group (MM) - maps to \ref RegGroup::kExtraVirt3 (X86, X86_64).
  kX86_MM = kExtraVirt3,

  //! Instruction pointer (X86, X86_64).
  kX86_Rip = kPC,
  //! Segment register group (X86, X86_64).
  kX86_SReg = kExtraNonVirt + 0,
  //! CR register group (X86, X86_64).
  kX86_CReg = kExtraNonVirt + 1,
  //! DR register group (X86, X86_64).
  kX86_DReg = kExtraNonVirt + 2,
  //! FPU register group (X86, X86_64).
  kX86_St = kExtraNonVirt + 3,
  //! BND register group (X86, X86_64).
  kX86_Bnd = kExtraNonVirt + 4,
  //! TMM register group (X86, X86_64).
  kX86_Tmm = kExtraNonVirt + 5,

  //! First group - only used in loops.
  k0 = 0,
  //! Last value of a virtual register that is managed by \ref BaseCompiler.
  kMaxVirt = Globals::kNumVirtGroups - 1,
  //! Maximum value of `RegGroup`.
  kMaxValue = 15
};
ASMJIT_DEFINE_ENUM_COMPARE(RegGroup)

typedef Support::EnumValues<RegGroup, RegGroup::kGp, RegGroup::kMaxVirt> RegGroupVirtValues;

//! Operand signature is a 32-bit number describing \ref Operand and some of its payload.
//!
//! In AsmJit operand signature is used to store additional payload of register, memory, and immediate operands.
//! In practice the biggest pressure on OperandSignature is from \ref BaseMem and architecture specific memory
//! operands that need to store additional payload that cannot be stored elsewhere as values of all other members
//! are fully specified by \ref BaseMem.
struct OperandSignature {
  //! \name Constants
  //! \{

  enum : uint32_t {
    // Operand type (3 least significant bits).
    // |........|........|........|.....XXX|
    kOpTypeShift = 0,
    kOpTypeMask = 0x07u << kOpTypeShift,

    // Register type (5 bits).
    // |........|........|........|XXXXX...|
    kRegTypeShift = 3,
    kRegTypeMask = 0x1Fu << kRegTypeShift,

    // Register group (4 bits).
    // |........|........|....XXXX|........|
    kRegGroupShift = 8,
    kRegGroupMask = 0x0Fu << kRegGroupShift,

    // Memory base type (5 bits).
    // |........|........|........|XXXXX...|
    kMemBaseTypeShift = 3,
    kMemBaseTypeMask = 0x1Fu << kMemBaseTypeShift,

    // Memory index type (5 bits).
    // |........|........|...XXXXX|........|
    kMemIndexTypeShift = 8,
    kMemIndexTypeMask = 0x1Fu << kMemIndexTypeShift,

    // Memory base+index combined (10 bits).
    // |........|........|...XXXXX|XXXXX...|
    kMemBaseIndexShift = 3,
    kMemBaseIndexMask = 0x3FFu << kMemBaseIndexShift,

    // This memory operand represents a home-slot or stack (Compiler) (1 bit).
    // |........|........|..X.....|........|
    kMemRegHomeShift = 13,
    kMemRegHomeFlag = 0x01u << kMemRegHomeShift,

    // Immediate type (1 bit).
    // |........|........|........|....X...|
    kImmTypeShift = 3,
    kImmTypeMask = 0x01u << kImmTypeShift,

    // Predicate used by either registers or immediate values (4 bits).
    // |........|XXXX....|........|........|
    kPredicateShift = 20,
    kPredicateMask = 0x0Fu << kPredicateShift,

    // Operand size (8 most significant bits).
    // |XXXXXXXX|........|........|........|
    kSizeShift = 24,
    kSizeMask = 0xFFu << kSizeShift
  };

  //! \}

  //! \name Members
  //! \{

  uint32_t _bits;

  //! \}

  //! \name Overloaded Operators
  //!
  //! Overloaded operators make `OperandSignature` behave like regular integer.
  //!
  //! \{

  ASMJIT_INLINE_NODEBUG constexpr bool operator!() const noexcept { return _bits == 0; }
  ASMJIT_INLINE_NODEBUG constexpr explicit operator bool() const noexcept { return _bits != 0; }

  ASMJIT_INLINE_NODEBUG OperandSignature& operator|=(uint32_t x) noexcept { _bits |= x; return *this; }
  ASMJIT_INLINE_NODEBUG OperandSignature& operator&=(uint32_t x) noexcept { _bits &= x; return *this; }
  ASMJIT_INLINE_NODEBUG OperandSignature& operator^=(uint32_t x) noexcept { _bits ^= x; return *this; }

  ASMJIT_INLINE_NODEBUG OperandSignature& operator|=(const OperandSignature& other) noexcept { return operator|=(other._bits); }
  ASMJIT_INLINE_NODEBUG OperandSignature& operator&=(const OperandSignature& other) noexcept { return operator&=(other._bits); }
  ASMJIT_INLINE_NODEBUG OperandSignature& operator^=(const OperandSignature& other) noexcept { return operator^=(other._bits); }

  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator~() const noexcept { return OperandSignature{~_bits}; }

  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator|(uint32_t x) const noexcept { return OperandSignature{_bits | x}; }
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator&(uint32_t x) const noexcept { return OperandSignature{_bits & x}; }
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator^(uint32_t x) const noexcept { return OperandSignature{_bits ^ x}; }

  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator|(const OperandSignature& other) const noexcept { return OperandSignature{_bits | other._bits}; }
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator&(const OperandSignature& other) const noexcept { return OperandSignature{_bits & other._bits}; }
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature operator^(const OperandSignature& other) const noexcept { return OperandSignature{_bits ^ other._bits}; }

  ASMJIT_INLINE_NODEBUG constexpr bool operator==(uint32_t x) const noexcept { return _bits == x; }
  ASMJIT_INLINE_NODEBUG constexpr bool operator!=(uint32_t x) const noexcept { return _bits != x; }

  ASMJIT_INLINE_NODEBUG constexpr bool operator==(const OperandSignature& other) const noexcept { return _bits == other._bits; }
  ASMJIT_INLINE_NODEBUG constexpr bool operator!=(const OperandSignature& other) const noexcept { return _bits != other._bits; }

  //! \}

  //! \name Accessors
  //! \{

  ASMJIT_INLINE_NODEBUG void reset() noexcept { _bits = 0; }

  ASMJIT_INLINE_NODEBUG constexpr uint32_t bits() const noexcept { return _bits; }
  ASMJIT_INLINE_NODEBUG void setBits(uint32_t bits) noexcept { _bits = bits; }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG constexpr bool hasField() const noexcept {
    return (_bits & kFieldMask) != 0;
  }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG constexpr bool hasField(uint32_t value) const noexcept {
    return (_bits & kFieldMask) != value << Support::ConstCTZ<kFieldMask>::value;
  }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG constexpr uint32_t getField() const noexcept {
    return (_bits >> Support::ConstCTZ<kFieldMask>::value) & (kFieldMask >> Support::ConstCTZ<kFieldMask>::value);
  }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG void setField(uint32_t value) noexcept {
    ASMJIT_ASSERT(((value << Support::ConstCTZ<kFieldMask>::value) & ~kFieldMask) == 0);
    _bits = (_bits & ~kFieldMask) | (value << Support::ConstCTZ<kFieldMask>::value);
  }

  ASMJIT_INLINE_NODEBUG constexpr OperandSignature subset(uint32_t mask) const noexcept { return OperandSignature{_bits & mask}; }

  template<uint32_t kFieldMask, uint32_t kFieldShift = Support::ConstCTZ<kFieldMask>::value>
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature replacedValue(uint32_t value) const noexcept { return OperandSignature{(_bits & ~kFieldMask) | (value << kFieldShift)}; }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG constexpr bool matchesSignature(const OperandSignature& signature) const noexcept {
    return (_bits & kFieldMask) == signature._bits;
  }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG constexpr bool matchesFields(uint32_t bits) const noexcept {
    return (_bits & kFieldMask) == bits;
  }

  template<uint32_t kFieldMask>
  ASMJIT_INLINE_NODEBUG constexpr bool matchesFields(const OperandSignature& fields) const noexcept {
    return (_bits & kFieldMask) == fields._bits;
  }

  ASMJIT_INLINE_NODEBUG constexpr bool isValid() const noexcept { return _bits != 0; }

  ASMJIT_INLINE_NODEBUG constexpr OperandType opType() const noexcept { return (OperandType)getField<kOpTypeMask>(); }

  ASMJIT_INLINE_NODEBUG constexpr RegType regType() const noexcept { return (RegType)getField<kRegTypeMask>(); }
  ASMJIT_INLINE_NODEBUG constexpr RegGroup regGroup() const noexcept { return (RegGroup)getField<kRegGroupMask>(); }

  ASMJIT_INLINE_NODEBUG constexpr RegType memBaseType() const noexcept { return (RegType)getField<kMemBaseTypeMask>(); }
  ASMJIT_INLINE_NODEBUG constexpr RegType memIndexType() const noexcept { return (RegType)getField<kMemIndexTypeMask>(); }

  ASMJIT_INLINE_NODEBUG constexpr uint32_t predicate() const noexcept { return getField<kPredicateMask>(); }
  ASMJIT_INLINE_NODEBUG constexpr uint32_t size() const noexcept { return getField<kSizeMask>(); }

  ASMJIT_INLINE_NODEBUG void setOpType(OperandType opType) noexcept { setField<kOpTypeMask>(uint32_t(opType)); }
  ASMJIT_INLINE_NODEBUG void setRegType(RegType regType) noexcept { setField<kRegTypeMask>(uint32_t(regType)); }
  ASMJIT_INLINE_NODEBUG void setRegGroup(RegGroup regGroup) noexcept { setField<kRegGroupMask>(uint32_t(regGroup)); }

  ASMJIT_INLINE_NODEBUG void setMemBaseType(RegType baseType) noexcept { setField<kMemBaseTypeMask>(uint32_t(baseType)); }
  ASMJIT_INLINE_NODEBUG void setMemIndexType(RegType indexType) noexcept { setField<kMemIndexTypeMask>(uint32_t(indexType)); }

  ASMJIT_INLINE_NODEBUG void setPredicate(uint32_t predicate) noexcept { setField<kPredicateMask>(predicate); }
  ASMJIT_INLINE_NODEBUG void setSize(uint32_t size) noexcept { setField<kSizeMask>(size); }

  //! \}

  //! \name Static Constructors
  //! \{

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromBits(uint32_t bits) noexcept {
    return OperandSignature{bits};
  }

  template<uint32_t kFieldMask, typename T>
  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromValue(const T& value) noexcept {
    return OperandSignature{uint32_t(value) << Support::ConstCTZ<kFieldMask>::value};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromOpType(OperandType opType) noexcept {
    return OperandSignature{uint32_t(opType) << kOpTypeShift};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromRegType(RegType regType) noexcept {
    return OperandSignature{uint32_t(regType) << kRegTypeShift};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromRegGroup(RegGroup regGroup) noexcept {
    return OperandSignature{uint32_t(regGroup) << kRegGroupShift};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromRegTypeAndGroup(RegType regType, RegGroup regGroup) noexcept {
    return fromRegType(regType) | fromRegGroup(regGroup);
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromMemBaseType(RegType baseType) noexcept {
    return OperandSignature{uint32_t(baseType) << kMemBaseTypeShift};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromMemIndexType(RegType indexType) noexcept {
    return OperandSignature{uint32_t(indexType) << kMemIndexTypeShift};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromPredicate(uint32_t predicate) noexcept {
    return OperandSignature{predicate << kPredicateShift};
  }

  static ASMJIT_INLINE_NODEBUG constexpr OperandSignature fromSize(uint32_t size) noexcept {
    return OperandSignature{size << kSizeShift};
  }

  //! \}
};

//! Base class representing an operand in AsmJit (non-default constructed version).
//!
//! Contains no initialization code and can be used safely to define an array of operands that won't be initialized.
//! This is a \ref Operand base structure designed to be statically initialized, static const, or to be used by user
//! code to define an array of operands without having them default initialized at construction time.
//!
//! The key difference between \ref Operand and \ref Operand_ is:
//!
//! ```
//! Operand_ xArray[10];    // Not initialized, contains garbage.
//! Operand_ yArray[10] {}; // All operands initialized to none explicitly (zero initialized).
//! Operand  yArray[10];    // All operands initialized to none implicitly (zero initialized).
//! ```
struct Operand_ {
  //! \name Types
  //! \{

  typedef OperandSignature Signature;

  //! \}

  //! \name Constants
  //! \{

  // Indexes to `_data` array.
  enum DataIndex : uint32_t {
    kDataMemIndexId = 0,
    kDataMemOffsetLo = 1,

    kDataImmValueLo = ASMJIT_ARCH_LE ? 0 : 1,
    kDataImmValueHi = ASMJIT_ARCH_LE ? 1 : 0
  };

  //! Constants useful for VirtId <-> Index translation.
  enum VirtIdConstants : uint32_t {
    //! Minimum valid packed-id.
    kVirtIdMin = 256,
    //! Maximum valid packed-id, excludes Globals::kInvalidId.
    kVirtIdMax = Globals::kInvalidId - 1,
    //! Count of valid packed-ids.
    kVirtIdCount = uint32_t(kVirtIdMax - kVirtIdMin + 1)
  };

  //! \}

  //! \name Members
  //! \{

  //! Provides operand type and additional payload.
  Signature _signature;
  //! Either base id as used by memory operand or any id as used by others.
  uint32_t _baseId;

  //! Data specific to the operand type.
  //!
  //! The reason we don't use union is that we have `constexpr` constructors that construct operands and other
  //!`constexpr` functions that return whether another Operand or something else. These cannot generally work with
  //! unions so we also cannot use `union` if we want to be standard compliant.
  uint32_t _data[2];

  //! \}

  //! Tests whether the given `id` is a valid virtual register id. Since AsmJit supports both physical and virtual
  //! registers it must be able to distinguish between these two. The idea is that physical registers are always
  //! limited in size, so virtual identifiers start from `kVirtIdMin` and end at `kVirtIdMax`.
  static ASMJIT_INLINE_NODEBUG bool isVirtId(uint32_t id) noexcept { return id - kVirtIdMin < uint32_t(kVirtIdCount); }
  //! Converts a real-id into a packed-id that can be stored in Operand.
  static ASMJIT_INLINE_NODEBUG uint32_t indexToVirtId(uint32_t id) noexcept { return id + kVirtIdMin; }
  //! Converts a packed-id back to real-id.
  static ASMJIT_INLINE_NODEBUG uint32_t virtIdToIndex(uint32_t id) noexcept { return id - kVirtIdMin; }

  //! \name Construction & Destruction
  //! \{

  //! \cond INTERNAL
  //! Initializes a `BaseReg` operand from `signature` and register `id`.
  ASMJIT_INLINE_NODEBUG void _initReg(const Signature& signature, uint32_t id) noexcept {
    _signature = signature;
    _baseId = id;
    _data[0] = 0;
    _data[1] = 0;
  }
  //! \endcond

  //! Initializes the operand from `other` operand (used by operator overloads).
  ASMJIT_INLINE_NODEBUG void copyFrom(const Operand_& other) noexcept {
    _signature._bits = other._signature._bits;
    _baseId = other._baseId;
    _data[0] = other._data[0];
    _data[1] = other._data[1];
  }

  //! Resets the `Operand` to none.
  //!
  //! None operand is defined the following way:
  //!   - Its signature is zero (OperandType::kNone, and the rest zero as well).
  //!   - Its id is `0`.
  //!   - The reserved8_4 field is set to `0`.
  //!   - The reserved12_4 field is set to zero.
  //!
  //! In other words, reset operands have all members set to zero. Reset operand must match the Operand state
  //! right after its construction. Alternatively, if you have an array of operands, you can simply use `memset()`.
  //!
  //! ```
  //! using namespace asmjit;
  //!
  //! Operand a;
  //! Operand b;
  //! assert(a == b);
  //!
  //! b = x86::eax;
  //! assert(a != b);
  //!
  //! b.reset();
  //! assert(a == b);
  //!
  //! memset(&b, 0, sizeof(Operand));
  //! assert(a == b);
  //! ```
  ASMJIT_INLINE_NODEBUG void reset() noexcept {
    _signature.reset();
    _baseId = 0;
    _data[0] = 0;
    _data[1] = 0;
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Tests whether this operand is the same as `other`.
  ASMJIT_INLINE_NODEBUG constexpr bool operator==(const Operand_& other) const noexcept { return  equals(other); }
  //! Tests whether this operand is not the same as `other`.
  ASMJIT_INLINE_NODEBUG constexpr bool operator!=(const Operand_& other) const noexcept { return !equals(other); }

  //! \}

  //! \name Cast
  //! \{

  //! Casts this operand to `T` type.
  template<typename T>
  ASMJIT_INLINE_NODEBUG T& as() noexcept { return static_cast<T&>(*this); }

  //! Casts this operand to `T` type (const).
  template<typename T>
  ASMJIT_INLINE_NODEBUG const T& as() const noexcept { return static_cast<const T&>(*this); }

  //! \}

  //! \name Equality
  //! \{

  //! Tests whether the operand is 100% equal to `other` operand.
  //!
  //! \note This basically performs a binary comparison, if aby bit is
  //! different the operands are not equal.
  ASMJIT_INLINE_NODEBUG constexpr bool equals(const Operand_& other) const noexcept {
    return bool(unsigned(_signature == other._signature) &
                unsigned(_baseId    == other._baseId   ) &
                unsigned(_data[0]   == other._data[0]  ) &
                unsigned(_data[1]   == other._data[1]  ));
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether the operand's signature matches the signature of the `other` operand.
  ASMJIT_INLINE_NODEBUG constexpr bool hasSignature(const Operand_& other) const noexcept { return _signature == other._signature; }
  //! Tests whether the operand's signature matches the given signature `sign`.
  ASMJIT_INLINE_NODEBUG constexpr bool hasSignature(const Signature& other) const noexcept { return _signature == other; }

  //! Returns operand signature as unsigned 32-bit integer.
  //!
  //! Signature is first 4 bytes of the operand data. It's used mostly for operand checking as it's
  //! much faster to check packed 4 bytes at once than having to check these bytes individually.
  ASMJIT_INLINE_NODEBUG constexpr Signature signature() const noexcept { return _signature; }

  //! Sets the operand signature, see `signature()`.
  //!
  //! \note Improper use of `setSignature()` can lead to hard-to-debug errors.
  ASMJIT_INLINE_NODEBUG void setSignature(const Signature& signature) noexcept { _signature = signature; }
  //! \overload
  ASMJIT_INLINE_NODEBUG void setSignature(uint32_t signature) noexcept { _signature._bits = signature; }

  //! Returns the type of the operand, see `OpType`.
  ASMJIT_INLINE_NODEBUG constexpr OperandType opType() const noexcept { return _signature.opType(); }
  //! Tests whether the operand is none (`OperandType::kNone`).
  ASMJIT_INLINE_NODEBUG constexpr bool isNone() const noexcept { return _signature == Signature::fromBits(0); }
  //! Tests whether the operand is a register (`OperandType::kReg`).
  ASMJIT_INLINE_NODEBUG constexpr bool isReg() const noexcept { return opType() == OperandType::kReg; }
  //! Tests whether the operand is a register-list.
  //!
  //! \note Register-list is currently only used by 32-bit ARM architecture.
  ASMJIT_INLINE_NODEBUG constexpr bool isRegList() const noexcept { return opType() == OperandType::kRegList; }
  //! Tests whether the operand is a memory location (`OperandType::kMem`).
  ASMJIT_INLINE_NODEBUG constexpr bool isMem() const noexcept { return opType() == OperandType::kMem; }
  //! Tests whether the operand is an immediate (`OperandType::kImm`).
  ASMJIT_INLINE_NODEBUG constexpr bool isImm() const noexcept { return opType() == OperandType::kImm; }
  //! Tests whether the operand is a label (`OperandType::kLabel`).
  ASMJIT_INLINE_NODEBUG constexpr bool isLabel() const noexcept { return opType() == OperandType::kLabel; }

  //! Tests whether the operand is a physical register.
  ASMJIT_INLINE_NODEBUG constexpr bool isPhysReg() const noexcept { return isReg() && _baseId < 0xFFu; }
  //! Tests whether the operand is a virtual register.
  ASMJIT_INLINE_NODEBUG constexpr bool isVirtReg() const noexcept { return isReg() && _baseId > 0xFFu; }

  //! Returns the operand id.
  //!
  //! The value returned should be interpreted accordingly to the operand type:
  //!   * None  - Should be `0`.
  //!   * Reg   - Physical or virtual register id.
  //!   * Mem   - Multiple meanings - BASE address (register or label id), or high value of a 64-bit absolute address.
  //!   * Imm   - Should be `0`.
  //!   * Label - Label id if it was created by using `newLabel()` or `Globals::kInvalidId` if the label is invalid or
  //!             not initialized.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t id() const noexcept { return _baseId; }

  //! Tests whether the operand is a register matching the given register `type`.
  ASMJIT_INLINE_NODEBUG constexpr bool isReg(RegType type) const noexcept {
    return _signature.subset(Signature::kOpTypeMask | Signature::kRegTypeMask) == (Signature::fromOpType(OperandType::kReg) | Signature::fromRegType(type));
  }

  //! Tests whether the operand is a register of the provided register group `regGroup`.
  ASMJIT_INLINE_NODEBUG constexpr bool isReg(RegGroup regGroup) const noexcept {
    return _signature.subset(Signature::kOpTypeMask | Signature::kRegGroupMask) == (Signature::fromOpType(OperandType::kReg) | Signature::fromRegGroup(regGroup));
  }

  //! Tests whether the operand is register and of register type `regType` and `regId`.
  ASMJIT_INLINE_NODEBUG constexpr bool isReg(RegType regType, uint32_t regId) const noexcept { return isReg(regType) && _baseId == regId; }
  //! Tests whether the operand is register and of register group `regGroup` and `regId`.
  ASMJIT_INLINE_NODEBUG constexpr bool isReg(RegGroup regGroup, uint32_t regId) const noexcept { return isReg(regGroup) && _baseId == regId; }

  //! Tests whether the register is a general purpose register (any size).
  ASMJIT_INLINE_NODEBUG constexpr bool isGp() const noexcept { return isReg(RegGroup::kGp); }
  //! Tests whether the register is a 32-bit general purpose register.
  ASMJIT_INLINE_NODEBUG constexpr bool isGp32() const noexcept { return isReg(RegType::kGp32); }
  //! Tests whether the register is a 64-bit general purpose register.
  ASMJIT_INLINE_NODEBUG constexpr bool isGp64() const noexcept { return isReg(RegType::kGp64); }

  //! Tests whether the register is a vector register of any size.
  ASMJIT_INLINE_NODEBUG constexpr bool isVec() const noexcept { return isReg(RegGroup::kVec); }
  //! Tests whether the register is an 8-bit vector register or view (AArch64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec8() const noexcept { return isReg(RegType::kVec8); }
  //! Tests whether the register is a 16-bit vector register or view (AArch64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec16() const noexcept { return isReg(RegType::kVec16); }
  //! Tests whether the register is a 32-bit vector register or view (AArch32, AArch64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec32() const noexcept { return isReg(RegType::kVec32); }
  //! Tests whether the register is a 64-bit vector register or view (AArch32, AArch64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec64() const noexcept { return isReg(RegType::kVec64); }
  //! Tests whether the register is a 128-bit vector register or view (AArch32, AArch64, X86, X86_64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec128() const noexcept { return isReg(RegType::kVec128); }
  //! Tests whether the register is a 256-bit vector register or view (X86, X86_64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec256() const noexcept { return isReg(RegType::kVec256); }
  //! Tests whether the register is a 512-bit vector register or view (X86, X86_64).
  ASMJIT_INLINE_NODEBUG constexpr bool isVec512() const noexcept { return isReg(RegType::kVec512); }

  //! Tests whether the register is a mask register of any size.
  ASMJIT_INLINE_NODEBUG constexpr bool isMask() const noexcept { return isReg(RegGroup::kMask); }

  //! Tests whether the operand is a register matching the given register `type`.
  ASMJIT_INLINE_NODEBUG constexpr bool isRegList(RegType type) const noexcept {
    return _signature.subset(Signature::kOpTypeMask | Signature::kRegTypeMask) == (Signature::fromOpType(OperandType::kRegList) | Signature::fromRegType(type));
  }

  //! Tests whether the operand is a register or memory.
  //!
  //! \note This is useful on X86 and X86_64 architectures as many instructions support Reg/Mem operand combination.
  //! So if the user code works with just \ref Operand, it's possible to check whether the operand is either a register
  //! or memory location with a single check.
  ASMJIT_INLINE_NODEBUG constexpr bool isRegOrMem() const noexcept {
    return Support::isBetween<uint32_t>(uint32_t(opType()), uint32_t(OperandType::kReg), uint32_t(OperandType::kMem));
  }

  //! Tests whether the operand is a register, register-list, or memory.
  //!
  //! \note This is useful on 32-bit ARM architecture to check whether an operand references a register. It can be
  //! used in other architectures too, but it would work identically to \ref isRegOrMem() as other architectures
  //! don't provide register lists.
  ASMJIT_INLINE_NODEBUG constexpr bool isRegOrRegListOrMem() const noexcept {
    return Support::isBetween<uint32_t>(uint32_t(opType()), uint32_t(OperandType::kReg), uint32_t(OperandType::kRegList));
  }

  //! \}

  //! \name Accessors (X86 Specific)
  //! \{

  //! Returns a size of a register or an X86 memory operand.
  //!
  //! \remarks At the moment only X86 and X86_64 memory operands have a size - other memory operands can use bits
  //! that represent size as an additional payload. This means that memory size is architecture specific and should
  //! be accessed via \ref x86::Mem::size(). Sometimes when the user knows that the operand is either a register or
  //! memory operand this function can be helpful as it avoids casting, but it only works when it targets X86 and X86_64.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t x86RmSize() const noexcept { return _signature.size(); }

  //! \}

#if !defined(ASMJIT_NO_DEPRECATED)
  ASMJIT_DEPRECATED("hasSize() is no longer portable - use x86RmSize() or x86::Mem::hasSize() instead, if your target is X86/X86_64")
  ASMJIT_INLINE_NODEBUG constexpr bool hasSize() const noexcept { return x86RmSize() != 0u; }

  ASMJIT_DEPRECATED("hasSize() is no longer portable - use x86RmSize() or x86::Mem::hasSize() instead, if your target is X86/X86_64")
  ASMJIT_INLINE_NODEBUG constexpr bool hasSize(uint32_t s) const noexcept { return x86RmSize() == s; }

  ASMJIT_DEPRECATED("size() is no longer portable - use x86RmSize() or x86::Mem::size() instead, if your target is X86/X86_64")
  ASMJIT_INLINE_NODEBUG constexpr uint32_t size() const noexcept { return _signature.getField<Signature::kSizeMask>(); }
#endif
};

//! Base class representing an operand in AsmJit (default constructed version).
class Operand : public Operand_ {
public:
  //! \name Construction & Destruction
  //! \{

  //! Creates `kOpNone` operand having all members initialized to zero.
  ASMJIT_INLINE_NODEBUG constexpr Operand() noexcept
    : Operand_{ Signature::fromOpType(OperandType::kNone), 0u, { 0u, 0u }} {}

  //! Creates a cloned `other` operand.
  ASMJIT_INLINE_NODEBUG constexpr Operand(const Operand& other) noexcept = default;

  //! Creates a cloned `other` operand.
  ASMJIT_INLINE_NODEBUG constexpr explicit Operand(const Operand_& other)
    : Operand_(other) {}

  //! Creates an operand initialized to raw `[u0, u1, u2, u3]` values.
  ASMJIT_INLINE_NODEBUG constexpr Operand(Globals::Init_, const Signature& u0, uint32_t u1, uint32_t u2, uint32_t u3) noexcept
    : Operand_{{u0._bits}, u1, {u2, u3}} {}

  //! Creates an uninitialized operand (dangerous).
  ASMJIT_INLINE_NODEBUG explicit Operand(Globals::NoInit_) noexcept {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG Operand& operator=(const Operand& other) noexcept = default;
  ASMJIT_INLINE_NODEBUG Operand& operator=(const Operand_& other) noexcept { return operator=(static_cast<const Operand&>(other)); }

  //! \}

  //! \name Clone
  //! \{

  //! Clones this operand and returns its copy.
  ASMJIT_INLINE_NODEBUG constexpr Operand clone() const noexcept { return Operand(*this); }

  //! \}
};

static_assert(sizeof(Operand) == 16, "asmjit::Operand must be exactly 16 bytes long");

//! Label (jump target or data location).
//!
//! Label represents a location in code typically used as a jump target, but may be also a reference to some data or
//! a static variable. Label has to be explicitly created by BaseEmitter.
//!
//! Example of using labels:
//!
//! ```
//! // Create some emitter (for example x86::Assembler).
//! x86::Assembler a;
//!
//! // Create Label instance.
//! Label L1 = a.newLabel();
//!
//! // ... your code ...
//!
//! // Using label.
//! a.jump(L1);
//!
//! // ... your code ...
//!
//! // Bind label to the current position, see `BaseEmitter::bind()`.
//! a.bind(L1);
//! ```
class Label : public Operand {
public:
  //! \name Construction & Destruction
  //! \{

  //! Creates a label operand without ID (you must set the ID to make it valid).
  ASMJIT_INLINE_NODEBUG constexpr Label() noexcept
    : Operand(Globals::Init, Signature::fromOpType(OperandType::kLabel), Globals::kInvalidId, 0, 0) {}

  //! Creates a cloned label operand of `other`.
  ASMJIT_INLINE_NODEBUG constexpr Label(const Label& other) noexcept
    : Operand(other) {}

  //! Creates a label operand of the given `id`.
  ASMJIT_INLINE_NODEBUG constexpr explicit Label(uint32_t id) noexcept
    : Operand(Globals::Init, Signature::fromOpType(OperandType::kLabel), id, 0, 0) {}

  ASMJIT_INLINE_NODEBUG explicit Label(Globals::NoInit_) noexcept
    : Operand(Globals::NoInit) {}

  //! Resets the label, will reset all properties and set its ID to `Globals::kInvalidId`.
  ASMJIT_INLINE_NODEBUG void reset() noexcept {
    _signature = Signature::fromOpType(OperandType::kLabel);
    _baseId = Globals::kInvalidId;
    _data[0] = 0;
    _data[1] = 0;
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG Label& operator=(const Label& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether the label was created by CodeHolder and/or an attached emitter.
  ASMJIT_INLINE_NODEBUG constexpr bool isValid() const noexcept { return _baseId != Globals::kInvalidId; }
  //! Sets the label `id`.
  ASMJIT_INLINE_NODEBUG void setId(uint32_t id) noexcept { _baseId = id; }

  //! \}
};

//! \cond INTERNAL
//! Default register traits.
struct BaseRegTraits {
  enum : uint32_t {
    //! \ref TypeId representing this register type, could be \ref TypeId::kVoid if such type doesn't exist.
    kTypeId = uint32_t(TypeId::kVoid),
    //! RegType is not valid by default.
    kValid = 0,

    //! Zero type by default (defaults to None).
    kType = uint32_t(RegType::kNone),
    //! Zero group by default (defaults to GP).
    kGroup = uint32_t(RegGroup::kGp),
    //! No size by default.
    kSize = 0,

    //! Empty signature by default (not even having operand type set to register).
    kSignature = 0
  };
};
//! \endcond

//! Physical or virtual register operand (base).
class BaseReg : public Operand {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    //! None or any register (mostly internal).
    kIdBad = 0xFFu,

    kBaseSignatureMask =
      Signature::kOpTypeMask   |
      Signature::kRegTypeMask  |
      Signature::kRegGroupMask |
      Signature::kSizeMask,

    kTypeNone = uint32_t(RegType::kNone),
    kSignature = Signature::fromOpType(OperandType::kReg).bits()
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a dummy register operand.
  ASMJIT_INLINE_NODEBUG constexpr BaseReg() noexcept
    : Operand(Globals::Init, Signature::fromOpType(OperandType::kReg), kIdBad, 0, 0) {}

  //! Creates a new register operand which is the same as `other` .
  ASMJIT_INLINE_NODEBUG constexpr BaseReg(const BaseReg& other) noexcept
    : Operand(other) {}

  //! Creates a new register operand compatible with `other`, but with a different `id`.
  ASMJIT_INLINE_NODEBUG constexpr BaseReg(const BaseReg& other, uint32_t id) noexcept
    : Operand(Globals::Init, other._signature, id, 0, 0) {}

  //! Creates a register initialized to the given `signature` and `id`.
  ASMJIT_INLINE_NODEBUG constexpr BaseReg(const Signature& signature, uint32_t id) noexcept
    : Operand(Globals::Init, signature, id, 0, 0) {}

  ASMJIT_INLINE_NODEBUG explicit BaseReg(Globals::NoInit_) noexcept
    : Operand(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG BaseReg& operator=(const BaseReg& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns base signature of the register associated with each register type.
  //!
  //! Base signature only contains the operand type, register type, register group, and register size. It doesn't
  //! contain element type, predicate, or other architecture-specific data. Base signature is a signature that is
  //! provided by architecture-specific `RegTraits`, like \ref x86::RegTraits.
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature baseSignature() const noexcept { return _signature & kBaseSignatureMask; }

  //! Tests whether the operand's base signature matches the given signature `sign`.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseSignature(uint32_t signature) const noexcept { return baseSignature() == signature; }
  //! Tests whether the operand's base signature matches the given signature `sign`.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseSignature(const OperandSignature& signature) const noexcept { return baseSignature() == signature; }
  //! Tests whether the operand's base signature matches the base signature of the `other` operand.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseSignature(const BaseReg& other) const noexcept { return baseSignature() == other.baseSignature(); }

  //! Tests whether this register is the same as `other`.
  //!
  //! This is just an optimization. Registers by default only use the first 8 bytes of Operand data, so this method
  //! takes advantage of this knowledge and only compares these 8 bytes. If both operands were created correctly
  //! both \ref equals() and \ref isSame() should give the same answer, however, if any of these two contains garbage
  //! or other metadata in the upper 8 bytes then \ref isSame() may return `true` in cases in which \ref equals()
  //! returns false.
  ASMJIT_INLINE_NODEBUG constexpr bool isSame(const BaseReg& other) const noexcept {
    return (_signature == other._signature) & (_baseId == other._baseId);
  }

  //! Tests whether the register is valid (either virtual or physical).
  ASMJIT_INLINE_NODEBUG constexpr bool isValid() const noexcept { return bool(unsigned(_signature != 0) & unsigned(_baseId != kIdBad)); }

  //! Tests whether this is a physical register.
  ASMJIT_INLINE_NODEBUG constexpr bool isPhysReg() const noexcept { return _baseId < kIdBad; }
  //! Tests whether this is a virtual register.
  ASMJIT_INLINE_NODEBUG constexpr bool isVirtReg() const noexcept { return _baseId > kIdBad; }

  //! Tests whether the register type matches `type` - same as `isReg(type)`, provided for convenience.
  ASMJIT_INLINE_NODEBUG constexpr bool isType(RegType type) const noexcept { return _signature.subset(Signature::kRegTypeMask) == Signature::fromRegType(type); }
  //! Tests whether the register group matches `group`.
  ASMJIT_INLINE_NODEBUG constexpr bool isGroup(RegGroup group) const noexcept { return _signature.subset(Signature::kRegGroupMask) == Signature::fromRegGroup(group); }

  //! Tests whether the register is a general purpose register (any size).
  ASMJIT_INLINE_NODEBUG constexpr bool isGp() const noexcept { return isGroup(RegGroup::kGp); }
  //! Tests whether the register is a vector register of any size.
  ASMJIT_INLINE_NODEBUG constexpr bool isVec() const noexcept { return isGroup(RegGroup::kVec); }
  //! Tests whether the register is a mask register of any size.
  ASMJIT_INLINE_NODEBUG constexpr bool isMask() const noexcept { return isGroup(RegGroup::kMask); }

  using Operand_::isReg;

  //! Same as `isType()`, provided for convenience.
  ASMJIT_INLINE_NODEBUG constexpr bool isReg(RegType rType) const noexcept { return isType(rType); }
  //! Tests whether the register type matches `type` and register id matches `id`.
  ASMJIT_INLINE_NODEBUG constexpr bool isReg(RegType rType, uint32_t id) const noexcept { return isType(rType) && this->id() == id; }

  //! Returns the register type.
  ASMJIT_INLINE_NODEBUG constexpr RegType type() const noexcept { return _signature.regType(); }
  //! Returns the register group.
  ASMJIT_INLINE_NODEBUG constexpr RegGroup group() const noexcept { return _signature.regGroup(); }

  //! Tests whether the register specifies a size (i.e. the size is not zero).
  ASMJIT_INLINE_NODEBUG constexpr bool hasSize() const noexcept { return _signature.hasField<Signature::kSizeMask>(); }
  //! Tests whether the register size matches size `s`.
  ASMJIT_INLINE_NODEBUG constexpr bool hasSize(uint32_t s) const noexcept { return size() == s; }

  //! Returns the size of the register in bytes. If the register size depends on architecture (like `x86::CReg` and
  //! `x86::DReg`) the size returned should be the greatest possible (so it should return 64-bit size in such case).
  ASMJIT_INLINE_NODEBUG constexpr uint32_t size() const noexcept { return _signature.getField<Signature::kSizeMask>(); }

  //! Returns operation predicate of the register (ARM/AArch64).
  //!
  //! The meaning depends on architecture, for example on ARM hardware this describes \ref arm::ShiftOp
  //! of the register.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t predicate() const noexcept { return _signature.getField<Signature::kPredicateMask>(); }

  //! Sets operation predicate of the register to `predicate` (ARM/AArch64).
  //!
  //! The meaning depends on architecture, for example on ARM hardware this describes \ref arm::ShiftOp
  //! of the register.
  ASMJIT_INLINE_NODEBUG void setPredicate(uint32_t predicate) noexcept { _signature.setField<Signature::kPredicateMask>(predicate); }

  //! Resets shift operation type of the register to the default value (ARM/AArch64).
  ASMJIT_INLINE_NODEBUG void resetPredicate() noexcept { _signature.setField<Signature::kPredicateMask>(0); }

  //! Clones the register operand.
  ASMJIT_INLINE_NODEBUG constexpr BaseReg clone() const noexcept { return BaseReg(*this); }

  //! Casts this register to `RegT` by also changing its signature.
  //!
  //! \note Improper use of `cloneAs()` can lead to hard-to-debug errors.
  template<typename RegT>
  ASMJIT_INLINE_NODEBUG constexpr RegT cloneAs() const noexcept { return RegT(Signature(RegT::kSignature), id()); }

  //! Casts this register to `other` by also changing its signature.
  //!
  //! \note Improper use of `cloneAs()` can lead to hard-to-debug errors.
  template<typename RegT>
  ASMJIT_INLINE_NODEBUG constexpr RegT cloneAs(const RegT& other) const noexcept { return RegT(other.signature(), id()); }

  //! Sets the register id to `id`.
  ASMJIT_INLINE_NODEBUG void setId(uint32_t id) noexcept { _baseId = id; }

  //! Sets a 32-bit operand signature based on traits of `RegT`.
  template<typename RegT>
  ASMJIT_INLINE_NODEBUG void setSignatureT() noexcept { _signature = RegT::kSignature; }

  //! Sets the register `signature` and `id`.
  ASMJIT_INLINE_NODEBUG void setSignatureAndId(const OperandSignature& signature, uint32_t id) noexcept {
    _signature = signature;
    _baseId = id;
  }

  //! \}

  //! \name Static Functions
  //! \{

  //! Tests whether the `op` operand is a general purpose register.
  static ASMJIT_INLINE_NODEBUG bool isGp(const Operand_& op) noexcept {
    // Check operand type and register group. Not interested in register type and size.
    return op.signature().subset(Signature::kOpTypeMask | Signature::kRegGroupMask) == (Signature::fromOpType(OperandType::kReg) | Signature::fromRegGroup(RegGroup::kGp));
  }

  //! Tests whether the `op` operand is a vector register.
  static ASMJIT_INLINE_NODEBUG bool isVec(const Operand_& op) noexcept {
    // Check operand type and register group. Not interested in register type and size.
    return op.signature().subset(Signature::kOpTypeMask | Signature::kRegGroupMask) == (Signature::fromOpType(OperandType::kReg) | Signature::fromRegGroup(RegGroup::kVec));
  }

  //! Tests whether the `op` is a general purpose register of the given `id`.
  static ASMJIT_INLINE_NODEBUG bool isGp(const Operand_& op, uint32_t id) noexcept { return bool(unsigned(isGp(op)) & unsigned(op.id() == id)); }
  //! Tests whether the `op` is a vector register of the given `id`.
  static ASMJIT_INLINE_NODEBUG bool isVec(const Operand_& op, uint32_t id) noexcept { return bool(unsigned(isVec(op)) & unsigned(op.id() == id)); }

  //! \}
};

//! RegOnly is 8-byte version of `BaseReg` that allows to store either register or nothing.
//!
//! It's designed to decrease the space consumed by an extra "operand" in \ref BaseEmitter and \ref InstNode.
struct RegOnly {
  //! \name Types
  //! \{

  typedef OperandSignature Signature;

  //! \}

  //! Operand signature - only \ref OperandType::kNone and \ref OperandType::kReg are supported.
  Signature _signature;
  //! Physical or virtual register id.
  uint32_t _id;

  //! \name Construction & Destruction
  //! \{

  //! Initializes the `RegOnly` instance to hold register `signature` and `id`.
  ASMJIT_INLINE_NODEBUG void init(const OperandSignature& signature, uint32_t id) noexcept {
    _signature = signature;
    _id = id;
  }

  ASMJIT_INLINE_NODEBUG void init(const BaseReg& reg) noexcept { init(reg.signature(), reg.id()); }
  ASMJIT_INLINE_NODEBUG void init(const RegOnly& reg) noexcept { init(reg.signature(), reg.id()); }

  //! Resets the `RegOnly` members to zeros (none).
  ASMJIT_INLINE_NODEBUG void reset() noexcept { init(Signature::fromBits(0), 0); }

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether this ExtraReg is none (same as calling `Operand_::isNone()`).
  ASMJIT_INLINE_NODEBUG constexpr bool isNone() const noexcept { return _signature == 0; }
  //! Tests whether the register is valid (either virtual or physical).
  ASMJIT_INLINE_NODEBUG constexpr bool isReg() const noexcept { return _signature != 0; }

  //! Tests whether this is a physical register.
  ASMJIT_INLINE_NODEBUG constexpr bool isPhysReg() const noexcept { return _id < BaseReg::kIdBad; }
  //! Tests whether this is a virtual register (used by `BaseCompiler`).
  ASMJIT_INLINE_NODEBUG constexpr bool isVirtReg() const noexcept { return _id > BaseReg::kIdBad; }

  //! Returns the register signature or 0 if no register is assigned.
  ASMJIT_INLINE_NODEBUG constexpr OperandSignature signature() const noexcept { return _signature; }
  //! Returns the register id.
  //!
  //! \note Always check whether the register is assigned before using the returned identifier as
  //! non-assigned `RegOnly` instance would return zero id, which is still a valid register id.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t id() const noexcept { return _id; }

  //! Sets the register id.
  ASMJIT_INLINE_NODEBUG void setId(uint32_t id) noexcept { _id = id; }

  //! Returns the register type.
  ASMJIT_INLINE_NODEBUG constexpr RegType type() const noexcept { return _signature.regType(); }
  //! Returns the register group.
  ASMJIT_INLINE_NODEBUG constexpr RegGroup group() const noexcept { return _signature.regGroup(); }

  //! \}

  //! \name Utilities
  //! \{

  //! Converts this ExtraReg to a real `RegT` operand.
  template<typename RegT>
  ASMJIT_INLINE_NODEBUG constexpr RegT toReg() const noexcept { return RegT(_signature, _id); }

  //! \}
};

//! \cond INTERNAL
//! Adds a template specialization for `REG_TYPE` into the local `RegTraits`.
#define ASMJIT_DEFINE_REG_TRAITS(REG_TYPE, GROUP, SIZE, TYPE_ID)                         \
template<>                                                                               \
struct RegTraits<REG_TYPE> {                                                             \
  static constexpr uint32_t kValid = 1;                                                  \
  static constexpr RegType kType = REG_TYPE;                                             \
  static constexpr RegGroup kGroup = GROUP;                                              \
  static constexpr uint32_t kSize = SIZE;                                                \
  static constexpr TypeId kTypeId = TYPE_ID;                                             \
                                                                                         \
  static constexpr uint32_t kSignature =                                                 \
    (OperandSignature::fromOpType(OperandType::kReg) |                                   \
     OperandSignature::fromRegType(kType)            |                                   \
     OperandSignature::fromRegGroup(kGroup)          |                                   \
     OperandSignature::fromSize(kSize)).bits();                                          \
                                                                                         \
}

//! Adds constructors and member functions to a class that implements abstract register. Abstract register is register
//! that doesn't have type or signature yet, it's a base class like `x86::Reg` or `arm::Reg`.
#define ASMJIT_DEFINE_ABSTRACT_REG(REG, BASE)                                            \
public:                                                                                  \
  /*! Default constructor that only setups basics. */                                    \
  ASMJIT_INLINE_NODEBUG constexpr REG() noexcept                                         \
    : BASE(Signature{kSignature}, kIdBad) {}                                             \
                                                                                         \
  /*! Makes a copy of the `other` register operand. */                                   \
  ASMJIT_INLINE_NODEBUG constexpr REG(const REG& other) noexcept                         \
    : BASE(other) {}                                                                     \
                                                                                         \
  /*! Makes a copy of the `other` register having id set to `id` */                      \
  ASMJIT_INLINE_NODEBUG constexpr REG(const BaseReg& other, uint32_t id) noexcept        \
    : BASE(other, id) {}                                                                 \
                                                                                         \
  /*! Creates a register based on `signature` and `id`. */                               \
  ASMJIT_INLINE_NODEBUG constexpr REG(const OperandSignature& sgn, uint32_t id) noexcept \
    : BASE(sgn, id) {}                                                                   \
                                                                                         \
  /*! Creates a completely uninitialized REG register operand (garbage). */              \
  ASMJIT_INLINE_NODEBUG explicit REG(Globals::NoInit_) noexcept                          \
    : BASE(Globals::NoInit) {}                                                           \
                                                                                         \
  /*! Creates a new register from register type and id. */                               \
  static ASMJIT_INLINE_NODEBUG REG fromTypeAndId(RegType type, uint32_t id) noexcept {   \
    return REG(signatureOf(type), id);                                                   \
  }                                                                                      \
                                                                                         \
  /*! Clones the register operand. */                                                    \
  ASMJIT_INLINE_NODEBUG constexpr REG clone() const noexcept { return REG(*this); }      \
                                                                                         \
  ASMJIT_INLINE_NODEBUG REG& operator=(const REG& other) noexcept = default;

//! Adds constructors and member functions to a class that implements final register. Final registers MUST HAVE a valid
//! signature.
#define ASMJIT_DEFINE_FINAL_REG(REG, BASE, TRAITS)                                       \
public:                                                                                  \
  static constexpr RegType kThisType = TRAITS::kType;                                    \
  static constexpr RegGroup kThisGroup = TRAITS::kGroup;                                 \
  static constexpr uint32_t kThisSize  = TRAITS::kSize;                                  \
  static constexpr uint32_t kSignature = TRAITS::kSignature;                             \
                                                                                         \
  ASMJIT_DEFINE_ABSTRACT_REG(REG, BASE)                                                  \
                                                                                         \
  /*! Creates a register operand having its id set to `id`. */                           \
  ASMJIT_INLINE_NODEBUG constexpr explicit REG(uint32_t id) noexcept                     \
    : BASE(Signature{kSignature}, id) {}
//! \endcond

//! List of physical registers (base).
//!
//! \note List of registers is only used by some ARM instructions at the moment.
class BaseRegList : public Operand {
public:
  //! \name Constants
  //! \{

  enum : uint32_t {
    kSignature = Signature::fromOpType(OperandType::kRegList).bits()
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a dummy register operand.
  ASMJIT_INLINE_NODEBUG constexpr BaseRegList() noexcept
    : Operand(Globals::Init, Signature::fromOpType(OperandType::kRegList), 0, 0, 0) {}

  //! Creates a new register operand which is the same as `other` .
  ASMJIT_INLINE_NODEBUG constexpr BaseRegList(const BaseRegList& other) noexcept
    : Operand(other) {}

  //! Creates a new register operand compatible with `other`, but with a different `id`.
  ASMJIT_INLINE_NODEBUG constexpr BaseRegList(const BaseRegList& other, RegMask regMask) noexcept
    : Operand(Globals::Init, other._signature, regMask, 0, 0) {}

  //! Creates a register initialized to the given `signature` and `id`.
  ASMJIT_INLINE_NODEBUG constexpr BaseRegList(const Signature& signature, RegMask regMask) noexcept
    : Operand(Globals::Init, signature, regMask, 0, 0) {}

  ASMJIT_INLINE_NODEBUG explicit BaseRegList(Globals::NoInit_) noexcept
    : Operand(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG BaseRegList& operator=(const BaseRegList& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  //! Tests whether the register-list is valid, which means it has a type and at least a single register in the list.
  ASMJIT_INLINE_NODEBUG constexpr bool isValid() const noexcept { return bool(unsigned(_signature != 0u) & unsigned(_baseId != 0u)); }

  //! Tests whether the register type matches `type` - same as `isReg(type)`, provided for convenience.
  ASMJIT_INLINE_NODEBUG constexpr bool isType(RegType type) const noexcept { return _signature.subset(Signature::kRegTypeMask) == Signature::fromRegType(type); }
  //! Tests whether the register group matches `group`.
  ASMJIT_INLINE_NODEBUG constexpr bool isGroup(RegGroup group) const noexcept { return _signature.subset(Signature::kRegGroupMask) == Signature::fromRegGroup(group); }

  //! Tests whether the register is a general purpose register (any size).
  ASMJIT_INLINE_NODEBUG constexpr bool isGp() const noexcept { return isGroup(RegGroup::kGp); }
  //! Tests whether the register is a vector register.
  ASMJIT_INLINE_NODEBUG constexpr bool isVec() const noexcept { return isGroup(RegGroup::kVec); }

  //! Returns the register type.
  ASMJIT_INLINE_NODEBUG constexpr RegType type() const noexcept { return _signature.regType(); }
  //! Returns the register group.
  ASMJIT_INLINE_NODEBUG constexpr RegGroup group() const noexcept { return _signature.regGroup(); }
  //! Returns the size of a single register in this register-list or 0 if unspecified.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t size() const noexcept { return _signature.getField<Signature::kSizeMask>(); }

  //! Returns the register list as a mask, where each bit represents one physical register.
  ASMJIT_INLINE_NODEBUG constexpr RegMask list() const noexcept { return _baseId; }
  //! Sets the register list to `mask`.
  ASMJIT_INLINE_NODEBUG void setList(RegMask mask) noexcept { _baseId = mask; }
  //! Remoes all registers from the register-list by making the underlying register-mask zero.
  ASMJIT_INLINE_NODEBUG void resetList() noexcept { _baseId = 0; }

  //! Adds registers passed by a register `mask` to the register-list.
  ASMJIT_INLINE_NODEBUG void addList(RegMask mask) noexcept { _baseId |= mask; }
  //! Removes registers passed by a register `mask` to the register-list.
  ASMJIT_INLINE_NODEBUG void clearList(RegMask mask) noexcept { _baseId &= ~mask; }
  //! Uses AND operator to combine the current register-list with other register `mask`.
  ASMJIT_INLINE_NODEBUG void andList(RegMask mask) noexcept { _baseId &= mask; }
  //! Uses XOR operator to combine the current register-list with other register `mask`.
  ASMJIT_INLINE_NODEBUG void xorList(RegMask mask) noexcept { _baseId ^= mask; }

  //! Checks whether a physical register `physId` is in the register-list.
  ASMJIT_INLINE_NODEBUG bool hasReg(uint32_t physId) const noexcept { return physId < 32u ? (_baseId & (1u << physId)) != 0 : false; }
  //! Adds a physical register `physId` to the register-list.
  ASMJIT_INLINE_NODEBUG void addReg(uint32_t physId) noexcept { addList(1u << physId); }
  //! Removes a physical register `physId` from the register-list.
  ASMJIT_INLINE_NODEBUG void clearReg(uint32_t physId) noexcept { clearList(1u << physId); }

  //! Clones the register-list operand.
  ASMJIT_INLINE_NODEBUG constexpr BaseRegList clone() const noexcept { return BaseRegList(*this); }

  //! Casts this register to `RegT` by also changing its signature.
  //!
  //! \note Improper use of `cloneAs()` can lead to hard-to-debug errors.
  template<typename RegListT>
  ASMJIT_INLINE_NODEBUG constexpr RegListT cloneAs() const noexcept { return RegListT(Signature(RegListT::kSignature), list()); }

  //! Casts this register to `other` by also changing its signature.
  //!
  //! \note Improper use of `cloneAs()` can lead to hard-to-debug errors.
  template<typename RegListT>
  ASMJIT_INLINE_NODEBUG constexpr RegListT cloneAs(const RegListT& other) const noexcept { return RegListT(other.signature(), list()); }

  //! \}
};

template<typename RegT>
class RegListT : public BaseRegList {
public:
  //! \name Construction & Destruction
  //! \{

  //! Creates a dummy register operand.
  ASMJIT_INLINE_NODEBUG constexpr RegListT() noexcept
    : BaseRegList() {}

  //! Creates a new register operand which is the same as `other` .
  ASMJIT_INLINE_NODEBUG constexpr RegListT(const RegListT& other) noexcept
    : BaseRegList(other) {}

  //! Creates a new register operand compatible with `other`, but with a different `id`.
  ASMJIT_INLINE_NODEBUG constexpr RegListT(const RegListT& other, RegMask regMask) noexcept
    : BaseRegList(other, regMask) {}

  //! Creates a register initialized to the given `signature` and `id`.
  ASMJIT_INLINE_NODEBUG constexpr RegListT(const Signature& signature, RegMask regMask) noexcept
    : BaseRegList(signature, regMask) {}

  //! Creates a register initialized to the given `signature` and `regs`.
  ASMJIT_INLINE_NODEBUG RegListT(const Signature& signature, std::initializer_list<RegT> regs) noexcept
    : BaseRegList(signature, RegMask(0)) { addRegs(regs); }

  ASMJIT_INLINE_NODEBUG explicit RegListT(Globals::NoInit_) noexcept
    : BaseRegList(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG RegListT& operator=(const RegListT& other) noexcept = default;

  //! \}

  //! \name Accessors
  //! \{

  using BaseRegList::addList;
  using BaseRegList::clearList;
  using BaseRegList::andList;
  using BaseRegList::xorList;

  //! Adds registers to this register-list as provided by `other` register-list.
  ASMJIT_INLINE_NODEBUG void addList(const RegListT<RegT>& other) noexcept { addList(other.list()); }
  //! Removes registers contained in `other` register-list.
  ASMJIT_INLINE_NODEBUG void clearList(const RegListT<RegT>& other) noexcept { clearList(other.list()); }
  //! Uses AND operator to combine the current register-list with `other` register-list.
  ASMJIT_INLINE_NODEBUG void andList(const RegListT<RegT>& other) noexcept { andList(other.list()); }
  //! Uses XOR operator to combine the current register-list with `other` register-list.
  ASMJIT_INLINE_NODEBUG void xorList(const RegListT<RegT>& other) noexcept { xorList(other.list()); }

  using BaseRegList::addReg;
  using BaseRegList::clearReg;

  ASMJIT_INLINE_NODEBUG void addReg(const RegT& reg) noexcept {
    if (reg.id() < 32u)
      addReg(reg.id());
  }

  ASMJIT_INLINE_NODEBUG void addRegs(std::initializer_list<RegT> regs) noexcept {
    for (const RegT& reg : regs)
      addReg(reg);
  }

  ASMJIT_INLINE_NODEBUG void clearReg(const RegT& reg) noexcept {
    if (reg.id() < 32u)
      clearReg(reg.id());
  }

  ASMJIT_INLINE_NODEBUG void clearRegs(std::initializer_list<RegT> regs) noexcept {
    for (const RegT& reg : regs)
      clearReg(reg);
  }

  //! \}
};

//! Base class for all memory operands.
//!
//! The data is split into the following parts:
//!
//!   - BASE - Base register or label - requires 36 bits total. 4 bits are used to encode the type of the BASE operand
//!     (label vs. register type) and the remaining 32 bits define the BASE id, which can be a physical or virtual
//!     register index. If BASE type is zero, which is never used as a register type and label doesn't use it as well
//!     then BASE field contains a high DWORD of a possible 64-bit absolute address, which is possible on X64.
//!
//!   - INDEX - Index register (or theoretically Label, which doesn't make sense). Encoding is similar to BASE - it
//!     also requires 36 bits and splits the encoding to INDEX type (4 bits defining the register type) and 32-bit id.
//!
//!   - OFFSET - A relative offset of the address. Basically if BASE is specified the relative displacement adjusts
//!     BASE and an optional INDEX. if BASE is not specified then the OFFSET should be considered as ABSOLUTE address
//!     (at least on X86). In that case its low 32 bits are stored in DISPLACEMENT field and the remaining high 32
//!     bits are stored in BASE.
//!
//!   - OTHER - There is rest 8 bits that can be used for whatever purpose. For example \ref x86::Mem operand uses
//!     these bits to store segment override prefix and index shift (or scale).
class BaseMem : public Operand {
public:
  //! \name Construction & Destruction
  //! \{

  //! Creates a default `BaseMem` operand, that points to [0].
  ASMJIT_INLINE_NODEBUG constexpr BaseMem() noexcept
      : Operand(Globals::Init, Signature::fromOpType(OperandType::kMem), 0, 0, 0) {}

  //! Creates a `BaseMem` operand that is a clone of `other`.
  ASMJIT_INLINE_NODEBUG constexpr BaseMem(const BaseMem& other) noexcept
    : Operand(other) {}

  //! Creates a `BaseMem` operand from `baseReg` and `offset`.
  //!
  //! \note This is an architecture independent constructor that can be used to create an architecture
  //! independent memory operand to be used in portable code that can handle multiple architectures.
  ASMJIT_INLINE_NODEBUG constexpr explicit BaseMem(const BaseReg& baseReg, int32_t offset = 0) noexcept
    : Operand(Globals::Init,
              Signature::fromOpType(OperandType::kMem) | Signature::fromMemBaseType(baseReg.type()),
              baseReg.id(),
              0,
              uint32_t(offset)) {}

  //! \cond INTERNAL
  //! Creates a `BaseMem` operand from 4 integers as used by `Operand_` struct.
  ASMJIT_INLINE_NODEBUG constexpr BaseMem(const OperandSignature& u0, uint32_t baseId, uint32_t indexId, int32_t offset) noexcept
    : Operand(Globals::Init, u0, baseId, indexId, uint32_t(offset)) {}
  //! \endcond

  //! Creates a completely uninitialized `BaseMem` operand.
  ASMJIT_INLINE_NODEBUG explicit BaseMem(Globals::NoInit_) noexcept
    : Operand(Globals::NoInit) {}

  //! Resets the memory operand - after the reset the memory points to [0].
  ASMJIT_INLINE_NODEBUG void reset() noexcept {
    _signature = Signature::fromOpType(OperandType::kMem);
    _baseId = 0;
    _data[0] = 0;
    _data[1] = 0;
  }

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG BaseMem& operator=(const BaseMem& other) noexcept { copyFrom(other); return *this; }

  //! \}

  //! \name Accessors
  //! \{

  //! Clones the memory operand.
  ASMJIT_INLINE_NODEBUG constexpr BaseMem clone() const noexcept { return BaseMem(*this); }

  //! Creates a new copy of this memory operand adjusted by `off`.
  ASMJIT_INLINE_NODEBUG BaseMem cloneAdjusted(int64_t off) const noexcept {
    BaseMem result(*this);
    result.addOffset(off);
    return result;
  }

  //! Tests whether this memory operand is a register home (only used by \ref asmjit_compiler)
  ASMJIT_INLINE_NODEBUG constexpr bool isRegHome() const noexcept { return _signature.hasField<Signature::kMemRegHomeFlag>(); }
  //! Mark this memory operand as register home (only used by \ref asmjit_compiler).
  ASMJIT_INLINE_NODEBUG void setRegHome() noexcept { _signature |= Signature::kMemRegHomeFlag; }
  //! Marks this operand to not be a register home (only used by \ref asmjit_compiler).
  ASMJIT_INLINE_NODEBUG void clearRegHome() noexcept { _signature &= ~Signature::kMemRegHomeFlag; }

  //! Tests whether the memory operand has a BASE register or label specified.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBase() const noexcept {
    return (_signature & Signature::kMemBaseTypeMask) != 0;
  }

  //! Tests whether the memory operand has an INDEX register specified.
  ASMJIT_INLINE_NODEBUG constexpr bool hasIndex() const noexcept {
    return (_signature & Signature::kMemIndexTypeMask) != 0;
  }

  //! Tests whether the memory operand has BASE or INDEX register.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseOrIndex() const noexcept {
    return (_signature & Signature::kMemBaseIndexMask) != 0;
  }

  //! Tests whether the memory operand has BASE and INDEX register.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseAndIndex() const noexcept {
    return (_signature & Signature::kMemBaseTypeMask) != 0 && (_signature & Signature::kMemIndexTypeMask) != 0;
  }

  //! Tests whether the BASE operand is a label.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseLabel() const noexcept {
    return _signature.subset(Signature::kMemBaseTypeMask) == Signature::fromMemBaseType(RegType::kLabelTag);
  }

  //! Tests whether the BASE operand is a register (registers start after `RegType::kLabelTag`).
  ASMJIT_INLINE_NODEBUG constexpr bool hasBaseReg() const noexcept {
    return _signature.subset(Signature::kMemBaseTypeMask).bits() > Signature::fromMemBaseType(RegType::kLabelTag).bits();
  }

  //! Tests whether the INDEX operand is a register (registers start after `RegType::kLabelTag`).
  ASMJIT_INLINE_NODEBUG constexpr bool hasIndexReg() const noexcept {
    return _signature.subset(Signature::kMemIndexTypeMask).bits() > Signature::fromMemIndexType(RegType::kLabelTag).bits();
  }

  //! Returns the type of the BASE register (0 if this memory operand doesn't use the BASE register).
  //!
  //! \note If the returned type is one (a value never associated to a register type) the BASE is not register, but it
  //! is a label. One equals to `kLabelTag`. You should always check `hasBaseLabel()` before using `baseId()` result.
  ASMJIT_INLINE_NODEBUG constexpr RegType baseType() const noexcept { return _signature.memBaseType(); }

  //! Returns the type of an INDEX register (0 if this memory operand doesn't
  //! use the INDEX register).
  ASMJIT_INLINE_NODEBUG constexpr RegType indexType() const noexcept { return _signature.memIndexType(); }

  //! This is used internally for BASE+INDEX validation.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t baseAndIndexTypes() const noexcept { return _signature.getField<Signature::kMemBaseIndexMask>(); }

  //! Returns both BASE (4:0 bits) and INDEX (9:5 bits) types combined into a single value.
  //!
  //! \remarks Returns id of the BASE register or label (if the BASE was specified as label).
  ASMJIT_INLINE_NODEBUG constexpr uint32_t baseId() const noexcept { return _baseId; }

  //! Returns the id of the INDEX register.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t indexId() const noexcept { return _data[kDataMemIndexId]; }

  //! Sets the id of the BASE register (without modifying its type).
  ASMJIT_INLINE_NODEBUG void setBaseId(uint32_t id) noexcept { _baseId = id; }
  //! Sets the register type of the BASE register (without modifying its id).
  ASMJIT_INLINE_NODEBUG void setBaseType(RegType regType) noexcept { _signature.setMemBaseType(regType); }

  //! Sets the id of the INDEX register (without modifying its type).
  ASMJIT_INLINE_NODEBUG void setIndexId(uint32_t id) noexcept { _data[kDataMemIndexId] = id; }
  //! Sets the register type of the INDEX register (without modifying its id).
  ASMJIT_INLINE_NODEBUG void setIndexType(RegType regType) noexcept { _signature.setMemIndexType(regType); }

  //! Sets the base register to type and id of the given `base` operand.
  ASMJIT_INLINE_NODEBUG void setBase(const BaseReg& base) noexcept { return _setBase(base.type(), base.id()); }
  //! Sets the index register to type and id of the given `index` operand.
  ASMJIT_INLINE_NODEBUG void setIndex(const BaseReg& index) noexcept { return _setIndex(index.type(), index.id()); }

  //! \cond INTERNAL
  ASMJIT_INLINE_NODEBUG void _setBase(RegType type, uint32_t id) noexcept {
    _signature.setField<Signature::kMemBaseTypeMask>(uint32_t(type));
    _baseId = id;
  }

  ASMJIT_INLINE_NODEBUG void _setIndex(RegType type, uint32_t id) noexcept {
    _signature.setField<Signature::kMemIndexTypeMask>(uint32_t(type));
    _data[kDataMemIndexId] = id;
  }
  //! \endcond

  //! Resets the memory operand's BASE register or label.
  ASMJIT_INLINE_NODEBUG void resetBase() noexcept { _setBase(RegType::kNone, 0); }
  //! Resets the memory operand's INDEX register.
  ASMJIT_INLINE_NODEBUG void resetIndex() noexcept { _setIndex(RegType::kNone, 0); }

  //! Tests whether the memory operand has a 64-bit offset or absolute address.
  //!
  //! If this is true then `hasBase()` must always report false.
  ASMJIT_INLINE_NODEBUG constexpr bool isOffset64Bit() const noexcept { return baseType() == RegType::kNone; }

  //! Tests whether the memory operand has a non-zero offset or absolute address.
  ASMJIT_INLINE_NODEBUG constexpr bool hasOffset() const noexcept {
    return (_data[kDataMemOffsetLo] | uint32_t(_baseId & Support::bitMaskFromBool<uint32_t>(isOffset64Bit()))) != 0;
  }

  //! Returns either relative offset or absolute address as 64-bit integer.
  ASMJIT_INLINE_NODEBUG constexpr int64_t offset() const noexcept {
    return isOffset64Bit() ? int64_t(uint64_t(_data[kDataMemOffsetLo]) | (uint64_t(_baseId) << 32))
                           : int64_t(int32_t(_data[kDataMemOffsetLo])); // Sign extend 32-bit offset.
  }

  //! Returns a 32-bit low part of a 64-bit offset or absolute address.
  ASMJIT_INLINE_NODEBUG constexpr int32_t offsetLo32() const noexcept { return int32_t(_data[kDataMemOffsetLo]); }
  //! Returns a 32-but high part of a 64-bit offset or absolute address.
  //!
  //! \note This function is UNSAFE and returns garbage if `isOffset64Bit()`
  //! returns false. Never use it blindly without checking it first.
  ASMJIT_INLINE_NODEBUG constexpr int32_t offsetHi32() const noexcept { return int32_t(_baseId); }

  //! Sets a 64-bit offset or an absolute address to `offset`.
  //!
  //! \note This functions attempts to set both high and low parts of a 64-bit offset, however, if the operand has
  //! a BASE register it will store only the low 32 bits of the offset / address as there is no way to store both
  //! BASE and 64-bit offset, and there is currently no architecture that has such capability targeted by AsmJit.
  inline void setOffset(int64_t offset) noexcept {
    uint32_t lo = uint32_t(uint64_t(offset) & 0xFFFFFFFFu);
    uint32_t hi = uint32_t(uint64_t(offset) >> 32);
    uint32_t hiMsk = Support::bitMaskFromBool<uint32_t>(isOffset64Bit());

    _data[kDataMemOffsetLo] = lo;
    _baseId = (hi & hiMsk) | (_baseId & ~hiMsk);
  }
  //! Sets a low 32-bit offset to `offset` (don't use without knowing how BaseMem works).
  inline void setOffsetLo32(int32_t offset) noexcept { _data[kDataMemOffsetLo] = uint32_t(offset); }

  //! Adjusts the offset by `offset`.
  //!
  //! \note This is a fast function that doesn't use the HI 32-bits of a 64-bit offset. Use it only if you know that
  //! there is a BASE register and the offset is only 32 bits anyway.

  //! Adjusts the memory operand offset by a `offset`.
  inline void addOffset(int64_t offset) noexcept {
    if (isOffset64Bit()) {
      int64_t result = offset + int64_t(uint64_t(_data[kDataMemOffsetLo]) | (uint64_t(_baseId) << 32));
      _data[kDataMemOffsetLo] = uint32_t(uint64_t(result) & 0xFFFFFFFFu);
      _baseId                 = uint32_t(uint64_t(result) >> 32);
    }
    else {
      _data[kDataMemOffsetLo] += uint32_t(uint64_t(offset) & 0xFFFFFFFFu);
    }
  }

  //! Adds `offset` to a low 32-bit offset part (don't use without knowing how BaseMem works).
  ASMJIT_INLINE_NODEBUG void addOffsetLo32(int32_t offset) noexcept { _data[kDataMemOffsetLo] += uint32_t(offset); }

  //! Resets the memory offset to zero.
  ASMJIT_INLINE_NODEBUG void resetOffset() noexcept { setOffset(0); }

  //! Resets the lo part of the memory offset to zero (don't use without knowing how BaseMem works).
  ASMJIT_INLINE_NODEBUG void resetOffsetLo32() noexcept { setOffsetLo32(0); }

  //! \}

#if !defined(ASMJIT_NO_DEPRECATED)
  ASMJIT_DEPRECATED("setSize() is no longer portable - use setX86RmSize() or x86::Mem::setSize() instead, if your target is X86/X86_64")
  ASMJIT_INLINE_NODEBUG void setSize(uint32_t size) noexcept { _signature.setField<Signature::kSizeMask>(size); }
#endif
};

//! Type of the an immediate value.
enum class ImmType : uint32_t {
  //! Immediate is integer.
  kInt = 0,
  //! Immediate is a floating point stored as double-precision.
  kDouble = 1
};

//! Immediate operands are encoded with instruction data.
class Imm : public Operand {
public:
  //! \cond INTERNAL
  template<typename T>
  struct IsConstexprConstructibleAsImmType
    : public std::integral_constant<bool, std::is_enum<T>::value ||
                                          std::is_pointer<T>::value ||
                                          std::is_integral<T>::value ||
                                          std::is_function<T>::value> {};

  template<typename T>
  struct IsConvertibleToImmType
    : public std::integral_constant<bool, IsConstexprConstructibleAsImmType<T>::value ||
                                          std::is_floating_point<T>::value> {};
  //! \endcond

  //! \name Construction & Destruction
  //! \{

  //! Creates a new immediate value (initial value is 0).
  ASMJIT_INLINE_NODEBUG constexpr Imm() noexcept
    : Operand(Globals::Init, Signature::fromOpType(OperandType::kImm), 0, 0, 0) {}

  //! Creates a new immediate value from `other`.
  ASMJIT_INLINE_NODEBUG constexpr Imm(const Imm& other) noexcept
    : Operand(other) {}

  //! Creates a new immediate value from ARM/AArch64 specific `shift`.
  ASMJIT_INLINE_NODEBUG constexpr Imm(const arm::Shift& shift) noexcept
    : Operand(Globals::Init,
              Signature::fromOpType(OperandType::kImm) | Signature::fromPredicate(uint32_t(shift.op())),
              0,
              Support::unpackU32At0(shift.value()),
              Support::unpackU32At1(shift.value())) {}

  //! Creates a new signed immediate value, assigning the value to `val` and an architecture-specific predicate
  //! to `predicate`.
  //!
  //! \note Predicate is currently only used by ARM architectures.
  template<typename T, typename = typename std::enable_if<IsConstexprConstructibleAsImmType<typename std::decay<T>::type>::value>::type>
  ASMJIT_INLINE_NODEBUG constexpr Imm(const T& val, const uint32_t predicate = 0) noexcept
    : Operand(Globals::Init,
              Signature::fromOpType(OperandType::kImm) | Signature::fromPredicate(predicate),
              0,
              Support::unpackU32At0(int64_t(val)),
              Support::unpackU32At1(int64_t(val))) {}

  ASMJIT_INLINE_NODEBUG Imm(const float& val, const uint32_t predicate = 0) noexcept
    : Operand(Globals::Init,
              Signature::fromOpType(OperandType::kImm) | Signature::fromPredicate(predicate),
              0,
              0,
              0) { setValue(val); }

  ASMJIT_INLINE_NODEBUG Imm(const double& val, const uint32_t predicate = 0) noexcept
    : Operand(Globals::Init,
              Signature::fromOpType(OperandType::kImm) | Signature::fromPredicate(predicate),
              0,
              0,
              0) { setValue(val); }

  ASMJIT_INLINE_NODEBUG explicit Imm(Globals::NoInit_) noexcept
    : Operand(Globals::NoInit) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  //! Assigns the value of the `other` operand to this immediate.
  ASMJIT_INLINE_NODEBUG Imm& operator=(const Imm& other) noexcept { copyFrom(other); return *this; }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns immediate type.
  ASMJIT_INLINE_NODEBUG constexpr ImmType type() const noexcept { return (ImmType)_signature.getField<Signature::kImmTypeMask>(); }
  //! Sets the immediate type to `type`.
  ASMJIT_INLINE_NODEBUG void setType(ImmType type) noexcept { _signature.setField<Signature::kImmTypeMask>(uint32_t(type)); }
  //! Resets immediate type to \ref ImmType::kInt.
  ASMJIT_INLINE_NODEBUG void resetType() noexcept { setType(ImmType::kInt); }

  //! Returns operation predicate of the immediate.
  //!
  //! The meaning depends on architecture, for example on ARM hardware this describes \ref arm::ShiftOp
  //! of the immediate.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t predicate() const noexcept { return _signature.getField<Signature::kPredicateMask>(); }

  //! Sets operation predicate of the immediate to `predicate`.
  //!
  //! The meaning depends on architecture, for example on ARM hardware this describes \ref arm::ShiftOp
  //! of the immediate.
  ASMJIT_INLINE_NODEBUG void setPredicate(uint32_t predicate) noexcept { _signature.setField<Signature::kPredicateMask>(predicate); }

  //! Resets the shift operation type of the immediate to the default value (no operation).
  ASMJIT_INLINE_NODEBUG void resetPredicate() noexcept { _signature.setField<Signature::kPredicateMask>(0); }

  //! Returns the immediate value as `int64_t`, which is the internal format Imm uses.
  ASMJIT_INLINE_NODEBUG constexpr int64_t value() const noexcept {
    return int64_t((uint64_t(_data[kDataImmValueHi]) << 32) | _data[kDataImmValueLo]);
  }

  //! Tests whether this immediate value is integer of any size.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t isInt() const noexcept { return type() == ImmType::kInt; }
  //! Tests whether this immediate value is a double precision floating point value.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t isDouble() const noexcept { return type() == ImmType::kDouble; }

  //! Tests whether the immediate can be casted to 8-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr bool isInt8() const noexcept { return type() == ImmType::kInt && Support::isInt8(value()); }
  //! Tests whether the immediate can be casted to 8-bit unsigned integer.
  ASMJIT_INLINE_NODEBUG constexpr bool isUInt8() const noexcept { return type() == ImmType::kInt && Support::isUInt8(value()); }
  //! Tests whether the immediate can be casted to 16-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr bool isInt16() const noexcept { return type() == ImmType::kInt && Support::isInt16(value()); }
  //! Tests whether the immediate can be casted to 16-bit unsigned integer.
  ASMJIT_INLINE_NODEBUG constexpr bool isUInt16() const noexcept { return type() == ImmType::kInt && Support::isUInt16(value()); }
  //! Tests whether the immediate can be casted to 32-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr bool isInt32() const noexcept { return type() == ImmType::kInt && Support::isInt32(value()); }
  //! Tests whether the immediate can be casted to 32-bit unsigned integer.
  ASMJIT_INLINE_NODEBUG constexpr bool isUInt32() const noexcept { return type() == ImmType::kInt && _data[kDataImmValueHi] == 0; }

  //! Returns the immediate value casted to `T`.
  //!
  //! The value is masked before it's casted to `T` so the returned value is simply the representation of `T`
  //! considering the original value's lowest bits.
  template<typename T>
  ASMJIT_INLINE_NODEBUG T valueAs() const noexcept { return Support::immediateToT<T>(value()); }

  //! Returns low 32-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr int32_t int32Lo() const noexcept { return int32_t(_data[kDataImmValueLo]); }
  //! Returns high 32-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr int32_t int32Hi() const noexcept { return int32_t(_data[kDataImmValueHi]); }
  //! Returns low 32-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t uint32Lo() const noexcept { return _data[kDataImmValueLo]; }
  //! Returns high 32-bit signed integer.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t uint32Hi() const noexcept { return _data[kDataImmValueHi]; }

  //! Sets immediate value to `val`, the value is casted to a signed 64-bit integer.
  template<typename T>
  ASMJIT_INLINE_NODEBUG void setValue(const T& val) noexcept {
    _setValueInternal(Support::immediateFromT(val), std::is_floating_point<T>::value ? ImmType::kDouble : ImmType::kInt);
  }

  ASMJIT_INLINE_NODEBUG void _setValueInternal(int64_t val, ImmType type) noexcept {
    setType(type);
    _data[kDataImmValueHi] = uint32_t(uint64_t(val) >> 32);
    _data[kDataImmValueLo] = uint32_t(uint64_t(val) & 0xFFFFFFFFu);
  }

  //! \}

  //! \name Utilities
  //! \{

  //! Clones the immediate operand.
  ASMJIT_INLINE_NODEBUG constexpr Imm clone() const noexcept { return Imm(*this); }

  ASMJIT_INLINE_NODEBUG void signExtend8Bits() noexcept { setValue(int64_t(valueAs<int8_t>())); }
  ASMJIT_INLINE_NODEBUG void signExtend16Bits() noexcept { setValue(int64_t(valueAs<int16_t>())); }
  ASMJIT_INLINE_NODEBUG void signExtend32Bits() noexcept { setValue(int64_t(valueAs<int32_t>())); }

  ASMJIT_INLINE_NODEBUG void zeroExtend8Bits() noexcept { setValue(valueAs<uint8_t>()); }
  ASMJIT_INLINE_NODEBUG void zeroExtend16Bits() noexcept { setValue(valueAs<uint16_t>()); }
  ASMJIT_INLINE_NODEBUG void zeroExtend32Bits() noexcept { _data[kDataImmValueHi] = 0u; }

  //! \}
};

//! Creates a new immediate operand.
template<typename T>
static ASMJIT_INLINE_NODEBUG constexpr Imm imm(const T& val) noexcept { return Imm(val); }

//! \}

namespace Globals {
  //! \ingroup asmjit_assembler
  //!
  //! A default-constructed operand of `Operand_::kOpNone` type.
  static constexpr const Operand none;
}

//! \cond INTERNAL
namespace Support {

template<typename T, bool kIsImm>
struct ForwardOpImpl {
  static ASMJIT_INLINE_NODEBUG const T& forward(const T& value) noexcept { return value; }
};

template<typename T>
struct ForwardOpImpl<T, true> {
  static ASMJIT_INLINE_NODEBUG Imm forward(const T& value) noexcept { return Imm(value); }
};

//! Either forwards operand T or returns a new operand that wraps it if T is a type convertible to operand.
//! At the moment this is only used to convert integers, floats, and enumarations to \ref Imm operands.
template<typename T>
struct ForwardOp : public ForwardOpImpl<T, Imm::IsConvertibleToImmType<typename std::decay<T>::type>::value> {};

} // {Support}
//! \endcond

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_OPERAND_H_INCLUDED
