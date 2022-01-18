// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_ARMOPERAND_H_INCLUDED
#define ASMJIT_ARM_ARMOPERAND_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/operand.h"
#include "../core/type.h"
#include "../arm/armglobals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \addtogroup asmjit_arm
//! \{

class Reg;
class Mem;

class Gp;
class GpW;
class GpX;

class Vec;
class VecB;
class VecH;
class VecS;
class VecD;
class VecV;

//! Register traits (ARM/AArch64).
//!
//! Register traits contains information about a particular register type. It's used by asmjit to setup register
//! information on-the-fly and to populate tables that contain register information (this way it's possible to
//! change register types and groups without having to reorder these tables).
template<RegType kRegType>
struct RegTraits : public BaseRegTraits {};

//! \cond
// <--------------------+-----+-------------------------+------------------------+---+---+------------------+
//                      | Reg |        Reg-Type         |        Reg-Group       |Sz |Cnt|      TypeId      |
// <--------------------+-----+-------------------------+------------------------+---+---+------------------+
ASMJIT_DEFINE_REG_TRAITS(GpW  , RegType::kARM_GpW       , RegGroup::kGp          , 4 , 32, TypeId::kInt32   );
ASMJIT_DEFINE_REG_TRAITS(GpX  , RegType::kARM_GpX       , RegGroup::kGp          , 8 , 32, TypeId::kInt64   );
ASMJIT_DEFINE_REG_TRAITS(VecB , RegType::kARM_VecB      , RegGroup::kVec         , 1 , 32, TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(VecH , RegType::kARM_VecH      , RegGroup::kVec         , 2 , 32, TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(VecS , RegType::kARM_VecS      , RegGroup::kVec         , 4 , 32, TypeId::kInt32x1 );
ASMJIT_DEFINE_REG_TRAITS(VecD , RegType::kARM_VecD      , RegGroup::kVec         , 8 , 32, TypeId::kInt32x2 );
ASMJIT_DEFINE_REG_TRAITS(VecV , RegType::kARM_VecV      , RegGroup::kVec         , 16, 32, TypeId::kInt32x4 );
//! \endcond

//! Register (ARM).
class Reg : public BaseReg {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Reg, BaseReg)

  //! Gets whether the register is a `R|W` register (32-bit).
  inline constexpr bool isGpW() const noexcept { return baseSignature() == RegTraits<RegType::kARM_GpW>::kSignature; }
  //! Gets whether the register is an `X` register (64-bit).
  inline constexpr bool isGpX() const noexcept { return baseSignature() == RegTraits<RegType::kARM_GpX>::kSignature; }
  //! Gets whether the register is a VEC-B register (8-bit).
  inline constexpr bool isVecB() const noexcept { return baseSignature() == RegTraits<RegType::kARM_VecB>::kSignature; }
  //! Gets whether the register is a VEC-H register (16-bit).
  inline constexpr bool isVecH() const noexcept { return baseSignature() == RegTraits<RegType::kARM_VecH>::kSignature; }
  //! Gets whether the register is a VEC-S register (32-bit).
  inline constexpr bool isVecS() const noexcept { return baseSignature() == RegTraits<RegType::kARM_VecS>::kSignature; }
  //! Gets whether the register is a VEC-D register (64-bit).
  inline constexpr bool isVecD() const noexcept { return baseSignature() == RegTraits<RegType::kARM_VecD>::kSignature; }
  //! Gets whether the register is a VEC-Q register (128-bit).
  inline constexpr bool isVecQ() const noexcept { return baseSignature() == RegTraits<RegType::kARM_VecV>::kSignature; }

  //! Gets whether the register is either VEC-D (64-bit) or VEC-Q (128-bit).
  inline constexpr bool isVecDOrQ() const noexcept { return uint32_t(type()) - uint32_t(RegType::kARM_VecD) <= 1u; }

  //! Gets whether the register is a VEC-V register (128-bit).
  inline constexpr bool isVecV() const noexcept { return baseSignature() == RegTraits<RegType::kARM_VecV>::kSignature; }

  template<RegType kRegType>
  inline void setRegT(uint32_t id) noexcept {
    setSignature(RegTraits<kRegType>::kSignature);
    setId(id);
  }

  inline void setTypeAndId(RegType type, uint32_t id) noexcept {
    setSignature(signatureOf(type));
    setId(id);
  }

  static inline RegGroup groupOf(RegType type) noexcept { return ArchTraits::byArch(Arch::kAArch64).regTypeToGroup(type); }
  static inline TypeId typeIdOf(RegType type) noexcept { return ArchTraits::byArch(Arch::kAArch64).regTypeToTypeId(type); }
  static inline OperandSignature signatureOf(RegType type) noexcept { return ArchTraits::byArch(Arch::kAArch64).regTypeToSignature(type); }

  template<RegType kRegType>
  static inline RegGroup groupOfT() noexcept { return RegTraits<kRegType>::kGroup; }

  template<RegType kRegType>
  static inline TypeId typeIdOfT() noexcept { return RegTraits<kRegType>::kTypeId; }

  template<RegType kRegType>
  static inline OperandSignature signatureOfT() noexcept { return RegTraits<kRegType>::kSignature; }

  static inline bool isGpW(const Operand_& op) noexcept { return op.as<Reg>().isGpW(); }
  static inline bool isGpX(const Operand_& op) noexcept { return op.as<Reg>().isGpX(); }
  static inline bool isVecB(const Operand_& op) noexcept { return op.as<Reg>().isVecB(); }
  static inline bool isVecH(const Operand_& op) noexcept { return op.as<Reg>().isVecH(); }
  static inline bool isVecS(const Operand_& op) noexcept { return op.as<Reg>().isVecS(); }
  static inline bool isVecD(const Operand_& op) noexcept { return op.as<Reg>().isVecD(); }
  static inline bool isVecQ(const Operand_& op) noexcept { return op.as<Reg>().isVecQ(); }
  static inline bool isVecV(const Operand_& op) noexcept { return op.as<Reg>().isVecV(); }

  static inline bool isGpW(const Operand_& op, uint32_t id) noexcept { return isGpW(op) & (op.id() == id); }
  static inline bool isGpX(const Operand_& op, uint32_t id) noexcept { return isGpX(op) & (op.id() == id); }
  static inline bool isVecB(const Operand_& op, uint32_t id) noexcept { return isVecB(op) & (op.id() == id); }
  static inline bool isVecH(const Operand_& op, uint32_t id) noexcept { return isVecH(op) & (op.id() == id); }
  static inline bool isVecS(const Operand_& op, uint32_t id) noexcept { return isVecS(op) & (op.id() == id); }
  static inline bool isVecD(const Operand_& op, uint32_t id) noexcept { return isVecD(op) & (op.id() == id); }
  static inline bool isVecQ(const Operand_& op, uint32_t id) noexcept { return isVecQ(op) & (op.id() == id); }
  static inline bool isVecV(const Operand_& op, uint32_t id) noexcept { return isVecV(op) & (op.id() == id); }
};

//! General purpose register (ARM).
class Gp : public Reg {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Gp, Reg)

  //! Special register id.
  enum Id : uint32_t {
    //! Register that depends on OS, could be used as TLS offset.
    kIdOs = 18,
    //! Frame pointer.
    kIdFp = 29,
    //! Link register.
    kIdLr = 30,
    //! Stack register id.
    kIdSp = 31,
    //! Zero register id.
    //!
    //! Although zero register has the same id as stack register it has a special treatment, because we need to be
    //! able to distinguish between these two at API level. Some intructions were designed to be used with SP and
    //! some other with ZR - so we need a way to distinguish these two to make sure we emit the right thing.
    //!
    //! The number 63 is not random, when you perform `id & 31` you would always get 31 for both SP and ZR inputs,
    //! which is the identifier used by AArch64 ISA to encode either SP or ZR depending on the instruction.
    kIdZr = 63
  };

  inline constexpr bool isZR() const noexcept { return id() == kIdZr; }
  inline constexpr bool isSP() const noexcept { return id() == kIdSp; }

  //! Cast this register to a 32-bit R|W.
  inline GpW w() const noexcept;
  //! Cast this register to a 64-bit X.
  inline GpX x() const noexcept;
};

//! Vector register (ARM).
class Vec : public Reg {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Vec, Reg)

  //! Additional signature bits used by arm::Vec.
  enum AdditionalBits : uint32_t {
    // Register element type (3 bits).
    // |........|........|.XXX....|........|
    kSignatureRegElementTypeShift = 12,
    kSignatureRegElementTypeMask = 0x07 << kSignatureRegElementTypeShift,

    // Register has element index (1 bit).
    // |........|........|X.......|........|
    kSignatureRegElementFlagShift = 15,
    kSignatureRegElementFlagMask = 0x01 << kSignatureRegElementFlagShift,

    // Register element index (4 bits).
    // |........|....XXXX|........|........|
    kSignatureRegElementIndexShift = 16,
    kSignatureRegElementIndexMask = 0x0F << kSignatureRegElementIndexShift
  };

  //! Element type.
  enum ElementType : uint32_t {
    //! No element type specified.
    kElementTypeNone = 0,
    //! Byte elements (B8 or B16).
    kElementTypeB,
    //! Halfword elements (H4 or H8).
    kElementTypeH,
    //! Singleword elements (S2 or S4).
    kElementTypeS,
    //! Doubleword elements (D2).
    kElementTypeD,
    //! Byte elements grouped by 4 bytes (B4).
    //!
    //! \note This element-type is only used by few instructions.
    kElementTypeB4,
    //! Halfword elements grouped by 2 halfwords (H2).
    //!
    //! \note This element-type is only used by few instructions.
    kElementTypeH2,

    //! Count of element types.
    kElementTypeCount
  };

  //! \cond
  //! Shortcuts.
  enum SignatureReg : uint32_t {
    kSignatureElementB  = kElementTypeB  << kSignatureRegElementTypeShift,
    kSignatureElementH  = kElementTypeH  << kSignatureRegElementTypeShift,
    kSignatureElementS  = kElementTypeS  << kSignatureRegElementTypeShift,
    kSignatureElementD  = kElementTypeD  << kSignatureRegElementTypeShift,
    kSignatureElementB4 = kElementTypeB4 << kSignatureRegElementTypeShift,
    kSignatureElementH2 = kElementTypeH2 << kSignatureRegElementTypeShift
  };
  //! \endcond

  //! Returns whether the register has associated an element type.
  inline constexpr bool hasElementType() const noexcept { return _signature.hasField<kSignatureRegElementTypeMask>(); }
  //! Returns whether the register has element index (it's an element index access).
  inline constexpr bool hasElementIndex() const noexcept { return _signature.hasField<kSignatureRegElementFlagMask>(); }
  //! Returns whether the reggister has element type or element index (or both).
  inline constexpr bool hasElementTypeOrIndex() const noexcept { return _signature.hasField<kSignatureRegElementTypeMask | kSignatureRegElementFlagMask>(); }

  //! Returns element type of the register.
  inline constexpr uint32_t elementType() const noexcept { return _signature.getField<kSignatureRegElementTypeMask>(); }
  //! Sets element type of the register to `elementType`.
  inline void setElementType(uint32_t elementType) noexcept { _signature.setField<kSignatureRegElementTypeMask>(elementType); }
  //! Resets element type to none.
  inline void resetElementType() noexcept { _signature.setField<kSignatureRegElementTypeMask>(0); }

  //! Returns element index of the register.
  inline constexpr uint32_t elementIndex() const noexcept { return _signature.getField<kSignatureRegElementIndexMask>(); }
  //! Sets element index of the register to `elementType`.
  inline void setElementIndex(uint32_t elementIndex) noexcept {
    _signature |= kSignatureRegElementFlagMask;
    _signature.setField<kSignatureRegElementIndexMask>(elementIndex);
  }
  //! Resets element index of the register.
  inline void resetElementIndex() noexcept {
    _signature &= ~(kSignatureRegElementFlagMask | kSignatureRegElementIndexMask);
  }

  inline constexpr bool isVecB8() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecD>::kSignature | kSignatureElementB); }
  inline constexpr bool isVecH4() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecD>::kSignature | kSignatureElementH); }
  inline constexpr bool isVecS2() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecD>::kSignature | kSignatureElementS); }
  inline constexpr bool isVecD1() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecD>::kSignature); }

  inline constexpr bool isVecB16() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecV>::kSignature | kSignatureElementB); }
  inline constexpr bool isVecH8() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecV>::kSignature | kSignatureElementH); }
  inline constexpr bool isVecS4() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecV>::kSignature | kSignatureElementS); }
  inline constexpr bool isVecD2() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecV>::kSignature | kSignatureElementD); }
  inline constexpr bool isVecB4x4() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecV>::kSignature | kSignatureElementB4); }
  inline constexpr bool isVecH2x4() const noexcept { return _signature.subset(kBaseSignatureMask | kSignatureRegElementTypeMask) == (RegTraits<RegType::kARM_VecV>::kSignature | kSignatureElementH2); }

  //! Creates a cloned register with element access.
  inline Vec at(uint32_t elementIndex) const noexcept {
    return Vec((signature() & ~kSignatureRegElementIndexMask) | (elementIndex << kSignatureRegElementIndexShift) | kSignatureRegElementFlagMask, id());
  }

  //! Cast this register to an 8-bit B register (scalar).
  inline VecB b() const noexcept;
  //! Cast this register to a 16-bit H register (scalar).
  inline VecH h() const noexcept;
  //! Cast this register to a 32-bit S register (scalar).
  inline VecS s() const noexcept;
  //! Cast this register to a 64-bit D register (scalar).
  inline VecD d() const noexcept;
  //! Cast this register to a 128-bit Q register (scalar).
  inline VecV q() const noexcept;
  //! Cast this register to a 128-bit V register.
  inline VecV v() const noexcept;

  //! Cast this register to a 128-bit V.B[elementIndex] register.
  inline VecV b(uint32_t elementIndex) const noexcept;
  //! Cast this register to a 128-bit V.H[elementIndex] register.
  inline VecV h(uint32_t elementIndex) const noexcept;
  //! Cast this register to a 128-bit V.S[elementIndex] register.
  inline VecV s(uint32_t elementIndex) const noexcept;
  //! Cast this register to a 128-bit V.D[elementIndex] register.
  inline VecV d(uint32_t elementIndex) const noexcept;
  //! Cast this register to a 128-bit V.H2[elementIndex] register.
  inline VecV h2(uint32_t elementIndex) const noexcept;
  //! Cast this register to a 128-bit V.B4[elementIndex] register.
  inline VecV b4(uint32_t elementIndex) const noexcept;

  //! Cast this register to V.8B.
  inline VecD b8() const noexcept;
  //! Cast this register to V.16B.
  inline VecV b16() const noexcept;
  //! Cast this register to V.2H.
  inline VecS h2() const noexcept;
  //! Cast this register to V.4H.
  inline VecD h4() const noexcept;
  //! Cast this register to V.8H.
  inline VecV h8() const noexcept;
  //! Cast this register to V.2S.
  inline VecD s2() const noexcept;
  //! Cast this register to V.4S.
  inline VecV s4() const noexcept;
  //! Cast this register to V.2D.
  inline VecV d2() const noexcept;

  static inline constexpr OperandSignature _makeElementAccessSignature(uint32_t elementType, uint32_t elementIndex) noexcept {
    return OperandSignature{
      uint32_t(RegTraits<RegType::kARM_VecV>::kSignature)      |
      uint32_t(kSignatureRegElementFlagMask)                   |
      uint32_t(elementType << kSignatureRegElementTypeShift)   |
      uint32_t(elementIndex << kSignatureRegElementIndexShift)};
  }
};

//! 32-bit GPW (AArch64) and/or GPR (ARM/AArch32) register.
class GpW : public Gp { ASMJIT_DEFINE_FINAL_REG(GpW, Gp, RegTraits<RegType::kARM_GpW>) };
//! 64-bit GPX (AArch64) register.
class GpX : public Gp { ASMJIT_DEFINE_FINAL_REG(GpX, Gp, RegTraits<RegType::kARM_GpX>) };

//! 8-bit view (S) of VFP/SIMD register.
class VecB : public Vec { ASMJIT_DEFINE_FINAL_REG(VecB, Vec, RegTraits<RegType::kARM_VecB>) };
//! 16-bit view (S) of VFP/SIMD register.
class VecH : public Vec { ASMJIT_DEFINE_FINAL_REG(VecH, Vec, RegTraits<RegType::kARM_VecH>) };
//! 32-bit view (S) of VFP/SIMD register.
class VecS : public Vec { ASMJIT_DEFINE_FINAL_REG(VecS, Vec, RegTraits<RegType::kARM_VecS>) };
//! 64-bit view (D) of VFP/SIMD register.
class VecD : public Vec { ASMJIT_DEFINE_FINAL_REG(VecD, Vec, RegTraits<RegType::kARM_VecD>) };
//! 128-bit vector register (Q or V).
class VecV : public Vec { ASMJIT_DEFINE_FINAL_REG(VecV, Vec, RegTraits<RegType::kARM_VecV>) };

inline GpW Gp::w() const noexcept { return GpW(id()); }
inline GpX Gp::x() const noexcept { return GpX(id()); }

inline VecB Vec::b() const noexcept { return VecB(id()); }
inline VecH Vec::h() const noexcept { return VecH(id()); }
inline VecS Vec::s() const noexcept { return VecS(id()); }
inline VecD Vec::d() const noexcept { return VecD(id()); }
inline VecV Vec::q() const noexcept { return VecV(id()); }
inline VecV Vec::v() const noexcept { return VecV(id()); }

inline VecV Vec::b(uint32_t elementIndex) const noexcept { return VecV(_makeElementAccessSignature(kElementTypeB, elementIndex), id()); }
inline VecV Vec::h(uint32_t elementIndex) const noexcept { return VecV(_makeElementAccessSignature(kElementTypeH, elementIndex), id()); }
inline VecV Vec::s(uint32_t elementIndex) const noexcept { return VecV(_makeElementAccessSignature(kElementTypeS, elementIndex), id()); }
inline VecV Vec::d(uint32_t elementIndex) const noexcept { return VecV(_makeElementAccessSignature(kElementTypeD, elementIndex), id()); }
inline VecV Vec::h2(uint32_t elementIndex) const noexcept { return VecV(_makeElementAccessSignature(kElementTypeH2, elementIndex), id()); }
inline VecV Vec::b4(uint32_t elementIndex) const noexcept { return VecV(_makeElementAccessSignature(kElementTypeB4, elementIndex), id()); }

inline VecD Vec::b8() const noexcept { return VecD(OperandSignature{VecD::kSignature | kSignatureElementB}, id()); }
inline VecS Vec::h2() const noexcept { return VecS(OperandSignature{VecS::kSignature | kSignatureElementH}, id()); }
inline VecD Vec::h4() const noexcept { return VecD(OperandSignature{VecD::kSignature | kSignatureElementH}, id()); }
inline VecD Vec::s2() const noexcept { return VecD(OperandSignature{VecD::kSignature | kSignatureElementS}, id()); }
inline VecV Vec::b16() const noexcept { return VecV(OperandSignature{VecV::kSignature | kSignatureElementB}, id()); }
inline VecV Vec::h8() const noexcept { return VecV(OperandSignature{VecV::kSignature | kSignatureElementH}, id()); }
inline VecV Vec::s4() const noexcept { return VecV(OperandSignature{VecV::kSignature | kSignatureElementS}, id()); }
inline VecV Vec::d2() const noexcept { return VecV(OperandSignature{VecV::kSignature | kSignatureElementD}, id()); }

#ifndef _DOXYGEN
namespace regs {
#endif

//! Creates a 32-bit W register operand (ARM/AArch64).
static inline constexpr GpW w(uint32_t id) noexcept { return GpW(id); }
//! Creates a 64-bit X register operand (AArch64).
static inline constexpr GpX x(uint32_t id) noexcept { return GpX(id); }
//! Creates a 32-bit S register operand (ARM/AArch64).
static inline constexpr VecS s(uint32_t id) noexcept { return VecS(id); }
//! Creates a 64-bit D register operand (ARM/AArch64).
static inline constexpr VecD d(uint32_t id) noexcept { return VecD(id); }
//! Creates a 1282-bit V register operand (ARM/AArch64).
static inline constexpr VecV v(uint32_t id) noexcept { return VecV(id); }

#ifndef _DOXYGEN
} // {regs}

// Make `arm::regs` accessible through `arm` namespace as well.
using namespace regs;
#endif

//! Memory operand (ARM).
class Mem : public BaseMem {
public:
  //! \cond INTERNAL
  //! Additional bits of operand's signature used by `arm::Mem`.
  enum AdditionalBits : uint32_t {
    // Index shift value (5 bits).
    // |........|.....XXX|XX......|........|
    kSignatureMemShiftValueShift = 14,
    kSignatureMemShiftValueMask = 0x1Fu << kSignatureMemShiftValueShift,

    // Shift operation type (4 bits).
    // |........|XXXX....|........|........|
    kSignatureMemPredicateShift = 20,
    kSignatureMemPredicateMask = 0x0Fu << kSignatureMemPredicateShift
  };
  //! \endcond

  //! Memory offset mode.
  //!
  //! Additional constants that can be used with the `predicate`.
  enum OffsetMode : uint32_t {
    //! Pre-index "[BASE, #Offset {, <shift>}]!" with write-back.
    kOffsetPreIndex = 0xE,
    //! Post-index "[BASE], #Offset {, <shift>}" with write-back.
    kOffsetPostIndex = 0xF
  };

  //! \name Construction & Destruction
  //! \{

  //! Construct a default `Mem` operand, that points to [0].
  inline constexpr Mem() noexcept
    : BaseMem() {}

  inline constexpr Mem(const Mem& other) noexcept
    : BaseMem(other) {}

  inline explicit Mem(Globals::NoInit_) noexcept
    : BaseMem(Globals::NoInit) {}

  inline constexpr Mem(const Signature& signature, uint32_t baseId, uint32_t indexId, int32_t offset) noexcept
    : BaseMem(signature, baseId, indexId, offset) {}

  inline constexpr explicit Mem(const Label& base, int32_t off = 0, Signature signature = Signature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(RegType::kLabelTag) |
              signature, base.id(), 0, off) {}

  inline constexpr explicit Mem(const BaseReg& base, int32_t off = 0, Signature signature = Signature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(base.type()) |
              signature, base.id(), 0, off) {}

  inline constexpr Mem(const BaseReg& base, const BaseReg& index, Signature signature = Signature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(base.type()) |
              Signature::fromMemIndexType(index.type()) |
              signature, base.id(), index.id(), 0) {}

  inline constexpr Mem(const BaseReg& base, const BaseReg& index, const Shift& shift, Signature signature = Signature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(base.type()) |
              Signature::fromMemIndexType(index.type()) |
              Signature::fromValue<kSignatureMemPredicateMask>(uint32_t(shift.op())) |
              Signature::fromValue<kSignatureMemShiftValueMask>(shift.value()) |
              signature, base.id(), index.id(), 0) {}

  inline constexpr Mem(uint64_t base, Signature signature = Signature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              signature, uint32_t(base >> 32), 0, int32_t(uint32_t(base & 0xFFFFFFFFu))) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  inline Mem& operator=(const Mem& other) noexcept = default;

  //! \}

  //! \name ARM Specific Features
  //! \{

  //! Clones the memory operand.
  inline constexpr Mem clone() const noexcept { return Mem(*this); }
  //! Gets new memory operand adjusted by `off`.
  inline Mem cloneAdjusted(int64_t off) const noexcept {
    Mem result(*this);
    result.addOffset(off);
    return result;
  }

  using BaseMem::setIndex;

  inline void setIndex(const BaseReg& index, uint32_t shift) noexcept {
    setIndex(index);
    setShift(shift);
  }

  //! Gets whether the memory operand has shift (aka scale) constant.
  inline constexpr bool hasShift() const noexcept { return _signature.hasField<kSignatureMemShiftValueMask>(); }
  //! Gets the memory operand's shift (aka scale) constant.
  inline constexpr uint32_t shift() const noexcept { return _signature.getField<kSignatureMemShiftValueMask>(); }
  //! Sets the memory operand's shift (aka scale) constant.
  inline void setShift(uint32_t shift) noexcept { _signature.setField<kSignatureMemShiftValueMask>(shift); }
  //! Resets the memory operand's shift (aka scale) constant to zero.
  inline void resetShift() noexcept { _signature.setField<kSignatureMemShiftValueMask>(0); }

  //! Gets memory predicate (shift mode or offset mode), see \ref ShiftOp and \ref OffsetMode.
  inline constexpr uint32_t predicate() const noexcept { return _signature.getField<kSignatureMemPredicateMask>(); }
  //! Sets memory predicate to `predicate`, see `Mem::ShiftOp`.
  inline void setPredicate(uint32_t predicate) noexcept { _signature.setField<kSignatureMemPredicateMask>(predicate); }
  //! Resets shift mode to LSL (default).
  inline void resetPredicate() noexcept { _signature.setField<kSignatureMemPredicateMask>(0); }

  inline constexpr bool isFixedOffset() const noexcept { return predicate() < kOffsetPreIndex; }
  inline constexpr bool isPreOrPost() const noexcept { return predicate() >= kOffsetPreIndex; }
  inline constexpr bool isPreIndex() const noexcept { return predicate() == kOffsetPreIndex; }
  inline constexpr bool isPostIndex() const noexcept { return predicate() == kOffsetPostIndex; }

  inline void resetToFixedOffset() noexcept { resetPredicate(); }
  inline void makePreIndex() noexcept { setPredicate(kOffsetPreIndex); }
  inline void makePostIndex() noexcept { setPredicate(kOffsetPostIndex); }

  inline Mem pre() const noexcept {
    Mem result(*this);
    result.setPredicate(kOffsetPreIndex);
    return result;
  }

  inline Mem pre(int64_t off) const noexcept {
    Mem result(*this);
    result.setPredicate(kOffsetPreIndex);
    result.addOffset(off);
    return result;
  }

  inline Mem post() const noexcept {
    Mem result(*this);
    result.setPredicate(kOffsetPreIndex);
    return result;
  }

  inline Mem post(int64_t off) const noexcept {
    Mem result(*this);
    result.setPredicate(kOffsetPostIndex);
    result.addOffset(off);
    return result;
  }

  //! \}
};

//! Creates `[base.reg, offset]` memory operand (offset mode).
static inline constexpr Mem ptr(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset);
}

//! Creates `[base.reg, offset]!` memory operand (pre-index mode).
static inline constexpr Mem ptr_pre(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset, OperandSignature::fromValue<Mem::kSignatureMemPredicateMask>(Mem::kOffsetPreIndex));
}

//! Creates `[base.reg], offset` memory operand (post-index mode).
static inline constexpr Mem ptr_post(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset, OperandSignature::fromValue<Mem::kSignatureMemPredicateMask>(Mem::kOffsetPostIndex));
}

//! Creates `[base.reg, index]` memory operand.
static inline constexpr Mem ptr(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index);
}

//! Creates `[base.reg], index` memory operand (post-index mode).
static inline constexpr Mem ptr_post(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index, OperandSignature::fromValue<Mem::kSignatureMemPredicateMask>(Mem::kOffsetPostIndex));
}

//! Creates `[base.reg, index, SHIFT_OP #shift]` memory operand.
static inline constexpr Mem ptr(const Gp& base, const Gp& index, const Shift& shift) noexcept {
  return Mem(base, index, shift);
}

//! Creates `[base + offset]` memory operand.
static inline constexpr Mem ptr(const Label& base, int32_t offset = 0) noexcept {
  return Mem(base, offset);
}

// TODO: [ARM] PC + offset address.
#if 0
//! Creates `[PC + offset]` (relative) memory operand.
static inline constexpr Mem ptr(const PC& pc, int32_t offset = 0) noexcept {
  return Mem(pc, offset);
}
#endif

//! Creates `[base]` absolute memory operand.
//!
//! \note The concept of absolute memory operands doesn't exist on ARM, the ISA only provides PC relative addressing.
//! Absolute memory operands can only be used if it's known that the PC relative offset is encodable and that it
//! would be within the limits. Absolute address is also often output from disassemblers, so AsmJit support it so it
//! can assemble it back.
static inline constexpr Mem ptr(uint64_t base) noexcept { return Mem(base); }

//! \}

ASMJIT_END_SUB_NAMESPACE

//! \cond INTERNAL
ASMJIT_BEGIN_NAMESPACE
ASMJIT_DEFINE_TYPE_ID(arm::GpW, TypeId::kInt32);
ASMJIT_DEFINE_TYPE_ID(arm::GpX, TypeId::kInt64);
ASMJIT_DEFINE_TYPE_ID(arm::VecS, TypeId::kFloat32x1);
ASMJIT_DEFINE_TYPE_ID(arm::VecD, TypeId::kFloat64x1);
ASMJIT_DEFINE_TYPE_ID(arm::VecV, TypeId::kInt32x4);
ASMJIT_END_NAMESPACE
//! \endcond

#endif // ASMJIT_ARM_ARMOPERAND_H_INCLUDED
