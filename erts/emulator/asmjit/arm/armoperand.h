// AsmJit - Machine code generation for C++
//
//  * Official AsmJit Home Page: https://asmjit.com
//  * Official Github Repository: https://github.com/asmjit/asmjit
//
// Copyright (c) 2008-2020 The AsmJit Authors
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would be
//    appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//    misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.

#ifndef ASMJIT_ARM_ARMOPERAND_H_INCLUDED
#define ASMJIT_ARM_ARMOPERAND_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/operand.h"
#include "../core/type.h"
#include "../arm/armglobals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(arm)

//! \addtogroup asmjit_arm
//! \{

// ============================================================================
// [Forward Declarations]
// ============================================================================

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

// ============================================================================
// [asmjit::arm::RegTraits]
// ============================================================================

//! Register traits (ARM/AArch64).
//!
//! Register traits contains information about a particular register type. It's
//! used by asmjit to setup register information on-the-fly and to populate
//! tables that contain register information (this way it's possible to change
//! register types and groups without having to reorder these tables).
template<uint32_t REG_TYPE>
struct RegTraits : public BaseRegTraits {};

//! \cond
// <--------------------+-----+----------------------+---------------------+---+---+----------------+
//                      | Reg |       Reg-Type       |      Reg-Group      |Sz |Cnt|     TypeId     |
// <--------------------+-----+----------------------+---------------------+---+---+----------------+
ASMJIT_DEFINE_REG_TRAITS(GpW  , BaseReg::kTypeGp32   , BaseReg::kGroupGp   , 4 , 32, Type::kIdI32   );
ASMJIT_DEFINE_REG_TRAITS(GpX  , BaseReg::kTypeGp64   , BaseReg::kGroupGp   , 8 , 32, Type::kIdI64   );
ASMJIT_DEFINE_REG_TRAITS(VecB , BaseReg::kTypeVec8   , BaseReg::kGroupVec  , 1 , 32, Type::kIdVoid  );
ASMJIT_DEFINE_REG_TRAITS(VecH , BaseReg::kTypeVec16  , BaseReg::kGroupVec  , 2 , 32, Type::kIdVoid  );
ASMJIT_DEFINE_REG_TRAITS(VecS , BaseReg::kTypeVec32  , BaseReg::kGroupVec  , 4 , 32, Type::kIdI32x1 );
ASMJIT_DEFINE_REG_TRAITS(VecD , BaseReg::kTypeVec64  , BaseReg::kGroupVec  , 8 , 32, Type::kIdI32x2 );
ASMJIT_DEFINE_REG_TRAITS(VecV , BaseReg::kTypeVec128 , BaseReg::kGroupVec  , 16, 32, Type::kIdI32x4 );
//! \endcond

// ============================================================================
// [asmjit::arm::Reg]
// ============================================================================

//! Register (ARM).
class Reg : public BaseReg {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Reg, BaseReg)

  //! Register type.
  enum RegType : uint32_t {
    //! No register type or invalid register.
    kTypeNone = BaseReg::kTypeNone,
    //! 32-bit general purpose register (R or W).
    kTypeGpW = BaseReg::kTypeGp32,
    //! 64-bit general purpose register (X).
    kTypeGpX = BaseReg::kTypeGp64,
    //! 8-bit view of VFP/ASIMD register (B).
    kTypeVecB = BaseReg::kTypeVec8,
    //! 16-bit view of VFP/ASIMD register (H).
    kTypeVecH = BaseReg::kTypeVec16,
    //! 32-bit view of VFP/ASIMD register (S).
    kTypeVecS = BaseReg::kTypeVec32,
    //! 64-bit view of VFP/ASIMD register (D).
    kTypeVecD = BaseReg::kTypeVec64,
    //! 128-bit view of VFP/ASIMD register (Q|V).
    kTypeVecV = BaseReg::kTypeVec128,
    //! Program pointer (PC) (AArch64).
    kTypePC = BaseReg::kTypeIP,
    //! Count of register types.
    kTypeCount
  };

  //! Register group.
  enum RegGroup : uint32_t {
    //! General purpose register group.
    kGroupGp = BaseReg::kGroupGp,
    //! Vector (VFP/ASIMD) register group.
    kGroupVec = BaseReg::kGroupVec,
    //! Count of all ARM register groups.
    kGroupCount
  };

  //! Gets whether the register is a `R|W` register (32-bit).
  constexpr bool isGpW() const noexcept { return hasBaseSignature(RegTraits<kTypeGpW>::kSignature); }
  //! Gets whether the register is an `X` register (64-bit).
  constexpr bool isGpX() const noexcept { return hasBaseSignature(RegTraits<kTypeGpX>::kSignature); }
  //! Gets whether the register is a VEC-B register (8-bit).
  constexpr bool isVecB() const noexcept { return hasBaseSignature(RegTraits<kTypeVecB>::kSignature); }
  //! Gets whether the register is a VEC-H register (16-bit).
  constexpr bool isVecH() const noexcept { return hasBaseSignature(RegTraits<kTypeVecH>::kSignature); }
  //! Gets whether the register is a VEC-S register (32-bit).
  constexpr bool isVecS() const noexcept { return hasBaseSignature(RegTraits<kTypeVecS>::kSignature); }
  //! Gets whether the register is a VEC-D register (64-bit).
  constexpr bool isVecD() const noexcept { return hasBaseSignature(RegTraits<kTypeVecD>::kSignature); }
  //! Gets whether the register is a VEC-Q register (128-bit).
  constexpr bool isVecQ() const noexcept { return hasBaseSignature(RegTraits<kTypeVecV>::kSignature); }

  //! Gets whether the register is either VEC-D (64-bit) or VEC-Q (128-bit).
  constexpr bool isVecDOrQ() const noexcept { return type() - Reg::kTypeVecD <= 1u; }

  //! Gets whether the register is a VEC-V register (128-bit).
  constexpr bool isVecV() const noexcept { return hasBaseSignature(RegTraits<kTypeVecV>::kSignature); }

  template<uint32_t REG_TYPE>
  inline void setRegT(uint32_t id) noexcept {
    setSignature(RegTraits<REG_TYPE>::kSignature);
    setId(id);
  }

  inline void setTypeAndId(uint32_t rType, uint32_t id) noexcept {
    ASMJIT_ASSERT(rType < kTypeCount);
    setSignature(signatureOf(rType));
    setId(id);
  }

  static inline uint32_t groupOf(uint32_t rType) noexcept { return _archTraits[Environment::kArchAArch64].regTypeToGroup(rType); }
  static inline uint32_t typeIdOf(uint32_t rType) noexcept { return _archTraits[Environment::kArchAArch64].regTypeToTypeId(rType); }
  static inline uint32_t signatureOf(uint32_t rType) noexcept { return _archTraits[Environment::kArchAArch64].regTypeToSignature(rType); }

  template<uint32_t REG_TYPE>
  static inline uint32_t groupOfT() noexcept { return RegTraits<REG_TYPE>::kGroup; }

  template<uint32_t REG_TYPE>
  static inline uint32_t typeIdOfT() noexcept { return RegTraits<REG_TYPE>::kTypeId; }

  template<uint32_t REG_TYPE>
  static inline uint32_t signatureOfT() noexcept { return RegTraits<REG_TYPE>::kSignature; }

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
    //! Although zero register has the same id as stack register we treat it
    //! special. The reason is that we may still want to map stack register
    //! to a virtual register in some case, but we would never need to do that
    //! mapping with zero register.
    kIdZr = 63
  };

  constexpr bool isZR() const noexcept { return id() == kIdZr; }
  constexpr bool isSP() const noexcept { return id() == kIdSp; }

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
    //! Byte elements groped by 4 bytes (B4).
    //!
    //! \note This element-type is only used by few instructions.
    kElementTypeB4,
    //! Halfword elements groped by 2 halfwords (H2).
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
  constexpr bool hasElementType() const noexcept { return elementType() != 0; }
  //! Returns whether the register has element index (it's an element index access).
  constexpr bool hasElementIndex() const noexcept { return (_signature & kSignatureRegElementFlagMask) != 0; }
  //! Returns whether the reggister has element type or element index (or both).
  constexpr bool hasElementTypeOrIndex() const noexcept { return (_signature & (kSignatureRegElementFlagMask | kSignatureRegElementTypeMask)) != 0; }

  //! Returns element type of the register.
  constexpr uint32_t elementType() const noexcept { return _getSignaturePart<kSignatureRegElementTypeMask>(); }
  //! Sets element type of the register to `elementType`.
  inline void setElementType(uint32_t elementType) noexcept { _setSignaturePart<kSignatureRegElementTypeMask>(elementType); }
  //! Resets element type to none.
  inline void resetElementType() noexcept { _setSignaturePart<kSignatureRegElementTypeMask>(0); }

  //! Returns element index of the register.
  constexpr uint32_t elementIndex() const noexcept { return _getSignaturePart<kSignatureRegElementIndexMask>(); }
  //! Sets element index of the register to `elementType`.
  inline void setElementIndex(uint32_t elementIndex) noexcept {
    _signature |= kSignatureRegElementFlagMask;
    _setSignaturePart<kSignatureRegElementIndexMask>(elementIndex);
  }
  //! Resets element index of the register.
  inline void resetElementIndex() noexcept {
    _signature &= ~(kSignatureRegElementFlagMask | kSignatureRegElementIndexMask);
  }

  constexpr bool isVecB8() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecD>::kSignature | kSignatureElementB); }
  constexpr bool isVecH4() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecD>::kSignature | kSignatureElementH); }
  constexpr bool isVecS2() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecD>::kSignature | kSignatureElementS); }
  constexpr bool isVecD1() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecD>::kSignature); }

  constexpr bool isVecB16() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecV>::kSignature | kSignatureElementB); }
  constexpr bool isVecH8() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecV>::kSignature | kSignatureElementH); }
  constexpr bool isVecS4() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecV>::kSignature | kSignatureElementS); }
  constexpr bool isVecD2() const noexcept { return _hasSignaturePart<kBaseSignature | kSignatureRegElementTypeMask>(RegTraits<kTypeVecV>::kSignature | kSignatureElementD); }

  //! Creates a cloned register with element access.
  inline Vec at(uint32_t elementIndex) const noexcept {
    return fromSignatureAndId(
      (signature() & ~kSignatureRegElementIndexMask) | (elementIndex << kSignatureRegElementIndexShift) | kSignatureRegElementFlagMask,
      id());
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
  //! Cast this register to a 128-bit V.B4[elementIndex] register.
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

  static constexpr uint32_t _makeElementAccessSignature(uint32_t elementType, uint32_t elementIndex) noexcept {
    return uint32_t(RegTraits<kTypeVecV>::kSignature)               |
           uint32_t(kSignatureRegElementFlagMask)                   |
           uint32_t(elementType << kSignatureRegElementTypeShift)   |
           uint32_t(elementIndex << kSignatureRegElementIndexShift) ;
  }
};

//! 32-bit GPW (AArch64) and/or GPR (ARM/AArch32) register.
class GpW : public Gp { ASMJIT_DEFINE_FINAL_REG(GpW, Gp, RegTraits<kTypeGpW>) };
//! 64-bit GPX (AArch64) register.
class GpX : public Gp { ASMJIT_DEFINE_FINAL_REG(GpX, Gp, RegTraits<kTypeGpX>) };

//! 8-bit view (S) of VFP/SIMD register.
class VecB : public Vec { ASMJIT_DEFINE_FINAL_REG(VecB, Vec, RegTraits<kTypeVecB>) };
//! 16-bit view (S) of VFP/SIMD register.
class VecH : public Vec { ASMJIT_DEFINE_FINAL_REG(VecH, Vec, RegTraits<kTypeVecH>) };
//! 32-bit view (S) of VFP/SIMD register.
class VecS : public Vec { ASMJIT_DEFINE_FINAL_REG(VecS, Vec, RegTraits<kTypeVecS>) };
//! 64-bit view (D) of VFP/SIMD register.
class VecD : public Vec { ASMJIT_DEFINE_FINAL_REG(VecD, Vec, RegTraits<kTypeVecD>) };
//! 128-bit vector register (Q or V).
class VecV : public Vec { ASMJIT_DEFINE_FINAL_REG(VecV, Vec, RegTraits<kTypeVecV>) };

inline GpW Gp::w() const noexcept { return GpW(id()); }
inline GpX Gp::x() const noexcept { return GpX(id()); }

inline VecB Vec::b() const noexcept { return VecB(id()); }
inline VecH Vec::h() const noexcept { return VecH(id()); }
inline VecS Vec::s() const noexcept { return VecS(id()); }
inline VecD Vec::d() const noexcept { return VecD(id()); }
inline VecV Vec::q() const noexcept { return VecV(id()); }
inline VecV Vec::v() const noexcept { return VecV(id()); }

inline VecV Vec::b(uint32_t elementIndex) const noexcept { return VecV::fromSignatureAndId(_makeElementAccessSignature(kElementTypeB, elementIndex), id()); }
inline VecV Vec::h(uint32_t elementIndex) const noexcept { return VecV::fromSignatureAndId(_makeElementAccessSignature(kElementTypeH, elementIndex), id()); }
inline VecV Vec::s(uint32_t elementIndex) const noexcept { return VecV::fromSignatureAndId(_makeElementAccessSignature(kElementTypeS, elementIndex), id()); }
inline VecV Vec::d(uint32_t elementIndex) const noexcept { return VecV::fromSignatureAndId(_makeElementAccessSignature(kElementTypeD, elementIndex), id()); }
inline VecV Vec::h2(uint32_t elementIndex) const noexcept { return VecV::fromSignatureAndId(_makeElementAccessSignature(kElementTypeH2, elementIndex), id()); }
inline VecV Vec::b4(uint32_t elementIndex) const noexcept { return VecV::fromSignatureAndId(_makeElementAccessSignature(kElementTypeB4, elementIndex), id()); }

inline VecD Vec::b8() const noexcept { return VecD::fromSignatureAndId(VecD::kSignature | kSignatureElementB, id()); }
inline VecS Vec::h2() const noexcept { return VecS::fromSignatureAndId(VecS::kSignature | kSignatureElementH, id()); }
inline VecD Vec::h4() const noexcept { return VecD::fromSignatureAndId(VecD::kSignature | kSignatureElementH, id()); }
inline VecD Vec::s2() const noexcept { return VecD::fromSignatureAndId(VecD::kSignature | kSignatureElementS, id()); }
inline VecV Vec::b16() const noexcept { return VecV::fromSignatureAndId(VecV::kSignature | kSignatureElementB, id()); }
inline VecV Vec::h8() const noexcept { return VecV::fromSignatureAndId(VecV::kSignature | kSignatureElementH, id()); }
inline VecV Vec::s4() const noexcept { return VecV::fromSignatureAndId(VecV::kSignature | kSignatureElementS, id()); }
inline VecV Vec::d2() const noexcept { return VecV::fromSignatureAndId(VecV::kSignature | kSignatureElementD, id()); }

#ifndef _DOXYGEN
namespace regs {
#endif

//! Creates a 32-bit W register operand (ARM/AArch64).
static constexpr GpW w(uint32_t id) noexcept { return GpW(id); }
//! Creates a 64-bit X register operand (AArch64).
static constexpr GpX x(uint32_t id) noexcept { return GpX(id); }
//! Creates a 32-bit S register operand (ARM/AArch64).
static constexpr VecS s(uint32_t id) noexcept { return VecS(id); }
//! Creates a 64-bit D register operand (ARM/AArch64).
static constexpr VecD d(uint32_t id) noexcept { return VecD(id); }
//! Creates a 1282-bit V register operand (ARM/AArch64).
static constexpr VecV v(uint32_t id) noexcept { return VecV(id); }

#ifndef _DOXYGEN
} // {regs}

// Make `arm::regs` accessible through `arm` namespace as well.
using namespace regs;
#endif

// ============================================================================
// [asmjit::arm::Mem]
// ============================================================================

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

  // --------------------------------------------------------------------------
  // [Construction / Destruction]
  // --------------------------------------------------------------------------

  //! Construct a default `Mem` operand, that points to [0].
  constexpr Mem() noexcept : BaseMem() {}
  constexpr Mem(const Mem& other) noexcept : BaseMem(other) {}

  constexpr explicit Mem(const Label& base, int32_t off = 0, uint32_t flags = 0) noexcept
    : BaseMem(Decomposed { Label::kLabelTag, base.id(), 0, 0, off, 0, flags }) {}

  constexpr explicit Mem(const BaseReg& base, int32_t off = 0, uint32_t flags = 0) noexcept
    : BaseMem(Decomposed { base.type(), base.id(), 0, 0, off, 0, flags }) {}

  constexpr Mem(const BaseReg& base, const BaseReg& index) noexcept
    : BaseMem(Decomposed { base.type(), base.id(), index.type(), index.id(), 0, 0, 0 }) {}

  constexpr Mem(const BaseReg& base, const BaseReg& index, const Shift& shift, uint32_t flags = 0) noexcept
    : BaseMem(Decomposed { base.type(), base.id(), index.type(), index.id(), 0, 0, flags | (shift.op() << kSignatureMemPredicateShift) | (shift.value() << kSignatureMemShiftValueShift) }) {}

  constexpr Mem(uint64_t base, uint32_t flags = 0) noexcept
    : BaseMem(Decomposed { 0, uint32_t(base >> 32), 0, 0, int32_t(uint32_t(base & 0xFFFFFFFFu)), 0, flags }) {}

  constexpr Mem(Globals::Init_, uint32_t u0, uint32_t u1, uint32_t u2, uint32_t u3) noexcept
    : BaseMem(Globals::Init, u0, u1, u2, u3) {}

  inline explicit Mem(Globals::NoInit_) noexcept : BaseMem(Globals::NoInit) {}

  // --------------------------------------------------------------------------
  // [Arm Specific]
  // --------------------------------------------------------------------------

  //! Clones the memory operand.
  constexpr Mem clone() const noexcept { return Mem(*this); }
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
  constexpr bool hasShift() const noexcept { return _hasSignaturePart<kSignatureMemShiftValueMask>(); }
  //! Gets the memory operand's shift (aka scale) constant.
  constexpr uint32_t shift() const noexcept { return _getSignaturePart<kSignatureMemShiftValueMask>(); }
  //! Sets the memory operand's shift (aka scale) constant.
  inline void setShift(uint32_t shift) noexcept { _setSignaturePart<kSignatureMemShiftValueMask>(shift); }
  //! Resets the memory operand's shift (aka scale) constant to zero.
  inline void resetShift() noexcept { _setSignaturePart<kSignatureMemShiftValueMask>(0); }

  //! Gets memory predicate (shift mode or offset mode), see \ref Predicate::ShiftOp and \ref OffsetMode.
  constexpr uint32_t predicate() const noexcept { return _getSignaturePart<kSignatureMemPredicateMask>(); }
  //! Sets memory predicate to `predicate`, see `Mem::ShiftOp`.
  inline void setPredicate(uint32_t predicate) noexcept { _setSignaturePart<kSignatureMemPredicateMask>(predicate); }
  //! Resets shift mode to LSL (default).
  inline void resetPredicate() noexcept { _setSignaturePart<kSignatureMemPredicateMask>(0); }

  constexpr bool isFixedOffset() const noexcept { return predicate() < kOffsetPreIndex; }
  constexpr bool isPreOrPost() const noexcept { return predicate() >= kOffsetPreIndex; }
  constexpr bool isPreIndex() const noexcept { return predicate() == kOffsetPreIndex; }
  constexpr bool isPostIndex() const noexcept { return predicate() == kOffsetPostIndex; }

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

  // --------------------------------------------------------------------------
  // [Operator Overload]
  // --------------------------------------------------------------------------

  inline Mem& operator=(const Mem& other) noexcept = default;
};

//! Creates `[base.reg, offset]` memory operand (offset mode).
static constexpr Mem ptr(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset);
}

//! Creates `[base.reg, offset]!` memory operand (pre-index mode).
static constexpr Mem ptr_pre(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset, Mem::kOffsetPreIndex << Mem::kSignatureMemPredicateShift);
}

//! Creates `[base.reg], offset` memory operand (post-index mode).
static constexpr Mem ptr_post(const Gp& base, int32_t offset = 0) noexcept {
  return Mem(base, offset, Mem::kOffsetPostIndex << Mem::kSignatureMemPredicateShift);
}

//! Creates `[base.reg, index]` memory operand.
static constexpr Mem ptr(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index);
}

//! Creates `[base.reg], index` memory operand (post-index mode).
static constexpr Mem ptr_post(const Gp& base, const Gp& index) noexcept {
  return Mem(base, index, Shift(Shift::kOpLSL, 0), Mem::kOffsetPostIndex << Mem::kSignatureMemPredicateShift);
}

//! Creates `[base.reg, index, SHIFT_OP #shift]` memory operand.
static constexpr Mem ptr(const Gp& base, const Gp& index, const Shift& shift) noexcept {
  return Mem(base, index, shift);
}

//! Creates `[base + offset]` memory operand.
static constexpr Mem ptr(const Label& base, int32_t offset = 0) noexcept {
  return Mem(base, offset);
}

// TODO: [ARM] PC + offset address.
#if 0
//! Creates `[PC + offset]` (relative) memory operand.
static constexpr Mem ptr(const PC& pc, int32_t offset = 0) noexcept {
  return Mem(pc, offset);
}
#endif

//! Creates `[base]` absolute memory operand.
//!
//! \note The concept of absolute memory operands doesn't exist on ARM, the ISA
//! only provides PC relative addressing. Absolute memory operands can only be
//! used if it's known that the PC relative offset is encodable and that it
//! would be within the limits. Absolute address is also often output from
//! disassemblers, so AsmJit support it so it can assemble it back.
static constexpr Mem ptr(uint64_t base) noexcept { return Mem(base); }

//! \}

ASMJIT_END_SUB_NAMESPACE

// ============================================================================
// [asmjit::Type::IdOfT<arm::Reg>]
// ============================================================================

//! \cond INTERNAL
ASMJIT_BEGIN_NAMESPACE
ASMJIT_DEFINE_TYPE_ID(arm::GpW, kIdI32);
ASMJIT_DEFINE_TYPE_ID(arm::GpX, kIdI64);
ASMJIT_DEFINE_TYPE_ID(arm::VecS, kIdF32x1);
ASMJIT_DEFINE_TYPE_ID(arm::VecD, kIdF64x1);
ASMJIT_DEFINE_TYPE_ID(arm::VecV, kIdI32x4);
ASMJIT_END_NAMESPACE
//! \endcond

#endif // ASMJIT_ARM_ARMOPERAND_H_INCLUDED
