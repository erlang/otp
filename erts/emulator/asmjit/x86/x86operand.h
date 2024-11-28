// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_X86_X86OPERAND_H_INCLUDED
#define ASMJIT_X86_X86OPERAND_H_INCLUDED

#include "../core/archtraits.h"
#include "../core/operand.h"
#include "../core/type.h"
#include "../x86/x86globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

//! \addtogroup asmjit_x86
//! \{

class Reg;
class Mem;

class Gp;
class Gpb;
class GpbLo;
class GpbHi;
class Gpw;
class Gpd;
class Gpq;
class Vec;
class Xmm;
class Ymm;
class Zmm;
class Mm;
class KReg;
class SReg;
class CReg;
class DReg;
class St;
class Bnd;
class Tmm;
class Rip;

//! Register traits (X86).
//!
//! Register traits contains information about a particular register type. It's used by asmjit to setup register
//! information on-the-fly and to populate tables that contain register information (this way it's possible to change
//! register types and groups without having to reorder these tables).
template<RegType kRegType>
struct RegTraits : public BaseRegTraits {};

//! \cond
// <--------------------+------------------------+------------------------+---+------------------+
//                      |       Reg-Type         |        Reg-Group       |Sz |      TypeId      |
// <--------------------+------------------------+------------------------+---+------------------+
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Rip       , RegGroup::kX86_Rip     , 0 , TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_GpbLo     , RegGroup::kGp          , 1 , TypeId::kInt8    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_GpbHi     , RegGroup::kGp          , 1 , TypeId::kInt8    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Gpw       , RegGroup::kGp          , 2 , TypeId::kInt16   );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Gpd       , RegGroup::kGp          , 4 , TypeId::kInt32   );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Gpq       , RegGroup::kGp          , 8 , TypeId::kInt64   );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Xmm       , RegGroup::kVec         , 16, TypeId::kInt32x4 );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Ymm       , RegGroup::kVec         , 32, TypeId::kInt32x8 );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Zmm       , RegGroup::kVec         , 64, TypeId::kInt32x16);
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_KReg      , RegGroup::kX86_K       , 0 , TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Mm        , RegGroup::kX86_MM      , 8 , TypeId::kMmx64   );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_SReg      , RegGroup::kX86_SReg    , 2 , TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_CReg      , RegGroup::kX86_CReg    , 0 , TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_DReg      , RegGroup::kX86_DReg    , 0 , TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_St        , RegGroup::kX86_St      , 10, TypeId::kFloat80 );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Bnd       , RegGroup::kX86_Bnd     , 16, TypeId::kVoid    );
ASMJIT_DEFINE_REG_TRAITS(RegType::kX86_Tmm       , RegGroup::kX86_Tmm     , 0 , TypeId::kVoid    );
//! \endcond

//! Register (X86).
class Reg : public BaseReg {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Reg, BaseReg)

  //! Tests whether the register is a GPB register (8-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isGpb() const noexcept { return size() == 1; }
  //! Tests whether the register is a low GPB register (8-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isGpbLo() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_GpbLo>::kSignature); }
  //! Tests whether the register is a high GPB register (8-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isGpbHi() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_GpbHi>::kSignature); }
  //! Tests whether the register is a GPW register (16-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isGpw() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Gpw>::kSignature); }
  //! Tests whether the register is a GPD register (32-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isGpd() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Gpd>::kSignature); }
  //! Tests whether the register is a GPQ register (64-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isGpq() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Gpq>::kSignature); }

  //! Tests whether the register is a 32-bit general purpose register, alias of \ref isGpd().
  ASMJIT_INLINE_NODEBUG constexpr bool isGp32() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Gpd>::kSignature); }
  //! Tests whether the register is a 64-bit general purpose register, alias of \ref isGpq()
  ASMJIT_INLINE_NODEBUG constexpr bool isGp64() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Gpq>::kSignature); }

  //! Tests whether the register is an XMM register (128-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isXmm() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Xmm>::kSignature); }
  //! Tests whether the register is a YMM register (256-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isYmm() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Ymm>::kSignature); }
  //! Tests whether the register is a ZMM register (512-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isZmm() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Zmm>::kSignature); }

  //! Tests whether the register is a 128-bit vector register, alias of \ref isXmm().
  ASMJIT_INLINE_NODEBUG constexpr bool isVec128() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Xmm>::kSignature); }
  //! Tests whether the register is a 256-bit vector register, alias of \ref isYmm().
  ASMJIT_INLINE_NODEBUG constexpr bool isVec256() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Ymm>::kSignature); }
  //! Tests whether the register is a 512-bit vector register, alias of \ref isZmm().
  ASMJIT_INLINE_NODEBUG constexpr bool isVec512() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Zmm>::kSignature); }

  //! Tests whether the register is an MMX register (64-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isMm() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Mm>::kSignature); }
  //! Tests whether the register is a K register (64-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isKReg() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_KReg>::kSignature); }
  //! Tests whether the register is a segment register.
  ASMJIT_INLINE_NODEBUG constexpr bool isSReg() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_SReg>::kSignature); }
  //! Tests whether the register is a control register.
  ASMJIT_INLINE_NODEBUG constexpr bool isCReg() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_CReg>::kSignature); }
  //! Tests whether the register is a debug register.
  ASMJIT_INLINE_NODEBUG constexpr bool isDReg() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_DReg>::kSignature); }
  //! Tests whether the register is an FPU register (80-bit).
  ASMJIT_INLINE_NODEBUG constexpr bool isSt() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_St>::kSignature); }
  //! Tests whether the register is a bound register.
  ASMJIT_INLINE_NODEBUG constexpr bool isBnd() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Bnd>::kSignature); }
  //! Tests whether the register is a TMM register.
  ASMJIT_INLINE_NODEBUG constexpr bool isTmm() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Tmm>::kSignature); }
  //! Tests whether the register is RIP.
  ASMJIT_INLINE_NODEBUG constexpr bool isRip() const noexcept { return hasBaseSignature(RegTraits<RegType::kX86_Rip>::kSignature); }

  template<RegType REG_TYPE>
  ASMJIT_INLINE_NODEBUG void setRegT(uint32_t rId) noexcept {
    setSignature(OperandSignature{RegTraits<REG_TYPE>::kSignature});
    setId(rId);
  }

  ASMJIT_INLINE_NODEBUG void setTypeAndId(RegType type, uint32_t id) noexcept {
    setSignature(signatureOf(type));
    setId(id);
  }

  static ASMJIT_INLINE_NODEBUG RegGroup groupOf(RegType type) noexcept { return ArchTraits::byArch(Arch::kX86).regTypeToGroup(type); }
  static ASMJIT_INLINE_NODEBUG TypeId typeIdOf(RegType type) noexcept { return ArchTraits::byArch(Arch::kX86).regTypeToTypeId(type); }
  static ASMJIT_INLINE_NODEBUG OperandSignature signatureOf(RegType type) noexcept { return ArchTraits::byArch(Arch::kX86).regTypeToSignature(type); }

  template<RegType REG_TYPE>
  static ASMJIT_INLINE_NODEBUG RegGroup groupOfT() noexcept { return RegGroup(RegTraits<REG_TYPE>::kGroup); }

  template<RegType REG_TYPE>
  static ASMJIT_INLINE_NODEBUG TypeId typeIdOfT() noexcept { return TypeId(RegTraits<REG_TYPE>::kTypeId); }

  template<RegType REG_TYPE>
  static ASMJIT_INLINE_NODEBUG OperandSignature signatureOfT() noexcept { return OperandSignature{RegTraits<REG_TYPE>::kSignature}; }

  static ASMJIT_INLINE_NODEBUG OperandSignature signatureOfVecByType(TypeId typeId) noexcept {
    return OperandSignature{typeId <= TypeId::_kVec128End ? uint32_t(RegTraits<RegType::kX86_Xmm>::kSignature) :
                            typeId <= TypeId::_kVec256End ? uint32_t(RegTraits<RegType::kX86_Ymm>::kSignature) :
                                                            uint32_t(RegTraits<RegType::kX86_Zmm>::kSignature)};
  }

  static ASMJIT_INLINE_NODEBUG OperandSignature signatureOfVecBySize(uint32_t size) noexcept {
    return OperandSignature{size <= 16 ? uint32_t(RegTraits<RegType::kX86_Xmm>::kSignature) :
                            size <= 32 ? uint32_t(RegTraits<RegType::kX86_Ymm>::kSignature) :
                                         uint32_t(RegTraits<RegType::kX86_Zmm>::kSignature)};
  }

  //! Tests whether the `op` operand is either a low or high 8-bit GPB register.
  static ASMJIT_INLINE_NODEBUG bool isGpb(const Operand_& op) noexcept {
    // Check operand type, register group, and size. Not interested in register type.
    return op.signature().subset(Signature::kOpTypeMask | Signature::kRegGroupMask | Signature::kSizeMask) ==
           (Signature::fromOpType(OperandType::kReg) | Signature::fromRegGroup(RegGroup::kGp) | Signature::fromSize(1));
  }

  static ASMJIT_INLINE_NODEBUG bool isGpbLo(const Operand_& op) noexcept { return op.as<Reg>().isGpbLo(); }
  static ASMJIT_INLINE_NODEBUG bool isGpbHi(const Operand_& op) noexcept { return op.as<Reg>().isGpbHi(); }
  static ASMJIT_INLINE_NODEBUG bool isGpw(const Operand_& op) noexcept { return op.as<Reg>().isGpw(); }
  static ASMJIT_INLINE_NODEBUG bool isGpd(const Operand_& op) noexcept { return op.as<Reg>().isGpd(); }
  static ASMJIT_INLINE_NODEBUG bool isGpq(const Operand_& op) noexcept { return op.as<Reg>().isGpq(); }
  static ASMJIT_INLINE_NODEBUG bool isXmm(const Operand_& op) noexcept { return op.as<Reg>().isXmm(); }
  static ASMJIT_INLINE_NODEBUG bool isYmm(const Operand_& op) noexcept { return op.as<Reg>().isYmm(); }
  static ASMJIT_INLINE_NODEBUG bool isZmm(const Operand_& op) noexcept { return op.as<Reg>().isZmm(); }
  static ASMJIT_INLINE_NODEBUG bool isMm(const Operand_& op) noexcept { return op.as<Reg>().isMm(); }
  static ASMJIT_INLINE_NODEBUG bool isKReg(const Operand_& op) noexcept { return op.as<Reg>().isKReg(); }
  static ASMJIT_INLINE_NODEBUG bool isSReg(const Operand_& op) noexcept { return op.as<Reg>().isSReg(); }
  static ASMJIT_INLINE_NODEBUG bool isCReg(const Operand_& op) noexcept { return op.as<Reg>().isCReg(); }
  static ASMJIT_INLINE_NODEBUG bool isDReg(const Operand_& op) noexcept { return op.as<Reg>().isDReg(); }
  static ASMJIT_INLINE_NODEBUG bool isSt(const Operand_& op) noexcept { return op.as<Reg>().isSt(); }
  static ASMJIT_INLINE_NODEBUG bool isBnd(const Operand_& op) noexcept { return op.as<Reg>().isBnd(); }
  static ASMJIT_INLINE_NODEBUG bool isTmm(const Operand_& op) noexcept { return op.as<Reg>().isTmm(); }
  static ASMJIT_INLINE_NODEBUG bool isRip(const Operand_& op) noexcept { return op.as<Reg>().isRip(); }

  static ASMJIT_INLINE_NODEBUG bool isGpb(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isGpb(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isGpbLo(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isGpbLo(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isGpbHi(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isGpbHi(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isGpw(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isGpw(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isGpd(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isGpd(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isGpq(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isGpq(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isXmm(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isXmm(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isYmm(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isYmm(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isZmm(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isZmm(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isMm(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isMm(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isKReg(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isKReg(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isSReg(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isSReg(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isCReg(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isCReg(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isDReg(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isDReg(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isSt(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isSt(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isBnd(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isBnd(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isTmm(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isTmm(op)) & unsigned(op.id() == rId)); }
  static ASMJIT_INLINE_NODEBUG bool isRip(const Operand_& op, uint32_t rId) noexcept { return bool(unsigned(isRip(op)) & unsigned(op.id() == rId)); }
};

//! General purpose register (X86).
class Gp : public Reg {
public:
  ASMJIT_DEFINE_ABSTRACT_REG(Gp, Reg)

  //! Physical id (X86).
  //!
  //! \note Register indexes have been reduced to only support general purpose registers. There is no need to
  //! have enumerations with number suffix that expands to the exactly same value as the suffix value itself.
  enum Id : uint32_t {
    kIdAx  = 0,  //!< Physical id of AL|AH|AX|EAX|RAX registers.
    kIdCx  = 1,  //!< Physical id of CL|CH|CX|ECX|RCX registers.
    kIdDx  = 2,  //!< Physical id of DL|DH|DX|EDX|RDX registers.
    kIdBx  = 3,  //!< Physical id of BL|BH|BX|EBX|RBX registers.
    kIdSp  = 4,  //!< Physical id of SPL|SP|ESP|RSP registers.
    kIdBp  = 5,  //!< Physical id of BPL|BP|EBP|RBP registers.
    kIdSi  = 6,  //!< Physical id of SIL|SI|ESI|RSI registers.
    kIdDi  = 7,  //!< Physical id of DIL|DI|EDI|RDI registers.
    kIdR8  = 8,  //!< Physical id of R8B|R8W|R8D|R8 registers (64-bit only).
    kIdR9  = 9,  //!< Physical id of R9B|R9W|R9D|R9 registers (64-bit only).
    kIdR10 = 10, //!< Physical id of R10B|R10W|R10D|R10 registers (64-bit only).
    kIdR11 = 11, //!< Physical id of R11B|R11W|R11D|R11 registers (64-bit only).
    kIdR12 = 12, //!< Physical id of R12B|R12W|R12D|R12 registers (64-bit only).
    kIdR13 = 13, //!< Physical id of R13B|R13W|R13D|R13 registers (64-bit only).
    kIdR14 = 14, //!< Physical id of R14B|R14W|R14D|R14 registers (64-bit only).
    kIdR15 = 15  //!< Physical id of R15B|R15W|R15D|R15 registers (64-bit only).
  };

  //! Casts this register to 8-bit (LO) part.
  ASMJIT_INLINE_NODEBUG GpbLo r8() const noexcept;
  //! Casts this register to 8-bit (LO) part.
  ASMJIT_INLINE_NODEBUG GpbLo r8Lo() const noexcept;
  //! Casts this register to 8-bit (HI) part.
  ASMJIT_INLINE_NODEBUG GpbHi r8Hi() const noexcept;
  //! Casts this register to 16-bit.
  ASMJIT_INLINE_NODEBUG Gpw r16() const noexcept;
  //! Casts this register to 32-bit.
  ASMJIT_INLINE_NODEBUG Gpd r32() const noexcept;
  //! Casts this register to 64-bit.
  ASMJIT_INLINE_NODEBUG Gpq r64() const noexcept;
};

//! Vector register (XMM|YMM|ZMM) (X86).
class Vec : public Reg {
  ASMJIT_DEFINE_ABSTRACT_REG(Vec, Reg)

  //! Casts this register to XMM (clone).
  ASMJIT_INLINE_NODEBUG Xmm xmm() const noexcept;
  //! Casts this register to YMM (clone).
  ASMJIT_INLINE_NODEBUG Ymm ymm() const noexcept;
  //! Casts this register to ZMM (clone).
  ASMJIT_INLINE_NODEBUG Zmm zmm() const noexcept;

  //! Casts this register to XMM (clone).
  ASMJIT_INLINE_NODEBUG Vec v128() const noexcept;
  //! Casts this register to YMM (clone).
  ASMJIT_INLINE_NODEBUG Vec v256() const noexcept;
  //! Casts this register to ZMM (clone).
  ASMJIT_INLINE_NODEBUG Vec v512() const noexcept;

  //! Casts this register to a register that has half the size (or XMM if it's already XMM).
  ASMJIT_INLINE_NODEBUG Vec half() const noexcept {
    return Vec(type() == RegType::kX86_Zmm ? signatureOfT<RegType::kX86_Ymm>() : signatureOfT<RegType::kX86_Xmm>(), id());
  }
};

//! Segment register (X86).
class SReg : public Reg {
  ASMJIT_DEFINE_FINAL_REG(SReg, Reg, RegTraits<RegType::kX86_SReg>)

  //! X86 segment id.
  enum Id : uint32_t {
    //! No segment (default).
    kIdNone = 0,
    //! ES segment.
    kIdEs = 1,
    //! CS segment.
    kIdCs = 2,
    //! SS segment.
    kIdSs = 3,
    //! DS segment.
    kIdDs = 4,
    //! FS segment.
    kIdFs = 5,
    //! GS segment.
    kIdGs = 6,

    //! Count of X86 segment registers supported by AsmJit.
    //!
    //! \note X86 architecture has 6 segment registers - ES, CS, SS, DS, FS, GS. X64 architecture lowers them down to
    //! just FS and GS. AsmJit supports 7 segment registers - all addressable in both X86 and X64 modes and one extra
    //! called `SReg::kIdNone`, which is AsmJit specific and means that there is no segment register specified.
    kIdCount = 7
  };
};

//! GPB low or high register (X86).
class Gpb : public Gp { ASMJIT_DEFINE_ABSTRACT_REG(Gpb, Gp) };
//! GPB low register (X86).
class GpbLo : public Gpb { ASMJIT_DEFINE_FINAL_REG(GpbLo, Gpb, RegTraits<RegType::kX86_GpbLo>) };
//! GPB high register (X86).
class GpbHi : public Gpb { ASMJIT_DEFINE_FINAL_REG(GpbHi, Gpb, RegTraits<RegType::kX86_GpbHi>) };
//! GPW register (X86).
class Gpw : public Gp { ASMJIT_DEFINE_FINAL_REG(Gpw, Gp, RegTraits<RegType::kX86_Gpw>) };
//! GPD register (X86).
class Gpd : public Gp { ASMJIT_DEFINE_FINAL_REG(Gpd, Gp, RegTraits<RegType::kX86_Gpd>) };
//! GPQ register (X86_64).
class Gpq : public Gp { ASMJIT_DEFINE_FINAL_REG(Gpq, Gp, RegTraits<RegType::kX86_Gpq>) };

//! 128-bit XMM register (SSE+).
class Xmm : public Vec {
  ASMJIT_DEFINE_FINAL_REG(Xmm, Vec, RegTraits<RegType::kX86_Xmm>)
  //! Casts this register to a register that has half the size (XMM).
  ASMJIT_INLINE_NODEBUG Xmm half() const noexcept { return Xmm(id()); }
};

//! 256-bit YMM register (AVX+).
class Ymm : public Vec {
  ASMJIT_DEFINE_FINAL_REG(Ymm, Vec, RegTraits<RegType::kX86_Ymm>)
  //! Casts this register to a register that has half the size (XMM).
  ASMJIT_INLINE_NODEBUG Xmm half() const noexcept { return Xmm(id()); }
};

//! 512-bit ZMM register (AVX512+).
class Zmm : public Vec {
  ASMJIT_DEFINE_FINAL_REG(Zmm, Vec, RegTraits<RegType::kX86_Zmm>)
  //! Casts this register to a register that has half the size (YMM).
  ASMJIT_INLINE_NODEBUG Ymm half() const noexcept { return Ymm(id()); }
};

//! 64-bit MMX register (MMX+).
class Mm : public Reg { ASMJIT_DEFINE_FINAL_REG(Mm, Reg, RegTraits<RegType::kX86_Mm>) };
//! 64-bit K register (AVX512+).
class KReg : public Reg { ASMJIT_DEFINE_FINAL_REG(KReg, Reg, RegTraits<RegType::kX86_KReg>) };
//! 32-bit or 64-bit control register (X86).
class CReg : public Reg { ASMJIT_DEFINE_FINAL_REG(CReg, Reg, RegTraits<RegType::kX86_CReg>) };
//! 32-bit or 64-bit debug register (X86).
class DReg : public Reg { ASMJIT_DEFINE_FINAL_REG(DReg, Reg, RegTraits<RegType::kX86_DReg>) };
//! 80-bit FPU register (X86).
class St : public Reg { ASMJIT_DEFINE_FINAL_REG(St, Reg, RegTraits<RegType::kX86_St>) };
//! 128-bit BND register (BND+).
class Bnd : public Reg { ASMJIT_DEFINE_FINAL_REG(Bnd, Reg, RegTraits<RegType::kX86_Bnd>) };
//! 8192-bit TMM register (AMX).
class Tmm : public Reg { ASMJIT_DEFINE_FINAL_REG(Tmm, Reg, RegTraits<RegType::kX86_Tmm>) };
//! RIP register (X86).
class Rip : public Reg { ASMJIT_DEFINE_FINAL_REG(Rip, Reg, RegTraits<RegType::kX86_Rip>) };

//! \cond
ASMJIT_INLINE_NODEBUG GpbLo Gp::r8() const noexcept { return GpbLo(id()); }
ASMJIT_INLINE_NODEBUG GpbLo Gp::r8Lo() const noexcept { return GpbLo(id()); }
ASMJIT_INLINE_NODEBUG GpbHi Gp::r8Hi() const noexcept { return GpbHi(id()); }
ASMJIT_INLINE_NODEBUG Gpw Gp::r16() const noexcept { return Gpw(id()); }
ASMJIT_INLINE_NODEBUG Gpd Gp::r32() const noexcept { return Gpd(id()); }
ASMJIT_INLINE_NODEBUG Gpq Gp::r64() const noexcept { return Gpq(id()); }
ASMJIT_INLINE_NODEBUG Xmm Vec::xmm() const noexcept { return Xmm(id()); }
ASMJIT_INLINE_NODEBUG Ymm Vec::ymm() const noexcept { return Ymm(id()); }
ASMJIT_INLINE_NODEBUG Zmm Vec::zmm() const noexcept { return Zmm(id()); }
ASMJIT_INLINE_NODEBUG Vec Vec::v128() const noexcept { return Xmm(id()); }
ASMJIT_INLINE_NODEBUG Vec Vec::v256() const noexcept { return Ymm(id()); }
ASMJIT_INLINE_NODEBUG Vec Vec::v512() const noexcept { return Zmm(id()); }
//! \endcond

//! \namespace asmjit::x86::regs
//!
//! Registers provided by X86 and X64 ISAs are in both `asmjit::x86` and `asmjit::x86::regs` namespaces so they can
//! be included with using directive. For example `using namespace asmjit::x86::regs` would include all registers,
//! but not other X86-specific API, whereas `using namespace asmjit::x86` would include everything X86-specific.
#ifndef _DOXYGEN
namespace regs {
#endif

//! Creates an 8-bit low GPB register operand.
static ASMJIT_INLINE_NODEBUG constexpr GpbLo gpb(uint32_t rId) noexcept { return GpbLo(rId); }
//! Creates an 8-bit low GPB register operand.
static ASMJIT_INLINE_NODEBUG constexpr GpbLo gpb_lo(uint32_t rId) noexcept { return GpbLo(rId); }
//! Creates an 8-bit high GPB register operand.
static ASMJIT_INLINE_NODEBUG constexpr GpbHi gpb_hi(uint32_t rId) noexcept { return GpbHi(rId); }
//! Creates a 16-bit GPW register operand.
static ASMJIT_INLINE_NODEBUG constexpr Gpw gpw(uint32_t rId) noexcept { return Gpw(rId); }
//! Creates a 32-bit GPD register operand.
static ASMJIT_INLINE_NODEBUG constexpr Gpd gpd(uint32_t rId) noexcept { return Gpd(rId); }
//! Creates a 64-bit GPQ register operand (64-bit).
static ASMJIT_INLINE_NODEBUG constexpr Gpq gpq(uint32_t rId) noexcept { return Gpq(rId); }
//! Creates a 128-bit XMM register operand.
static ASMJIT_INLINE_NODEBUG constexpr Xmm xmm(uint32_t rId) noexcept { return Xmm(rId); }
//! Creates a 256-bit YMM register operand.
static ASMJIT_INLINE_NODEBUG constexpr Ymm ymm(uint32_t rId) noexcept { return Ymm(rId); }
//! Creates a 512-bit ZMM register operand.
static ASMJIT_INLINE_NODEBUG constexpr Zmm zmm(uint32_t rId) noexcept { return Zmm(rId); }
//! Creates a 64-bit Mm register operand.
static ASMJIT_INLINE_NODEBUG constexpr Mm mm(uint32_t rId) noexcept { return Mm(rId); }
//! Creates a 64-bit K register operand.
static ASMJIT_INLINE_NODEBUG constexpr KReg k(uint32_t rId) noexcept { return KReg(rId); }
//! Creates a 32-bit or 64-bit control register operand.
static ASMJIT_INLINE_NODEBUG constexpr CReg cr(uint32_t rId) noexcept { return CReg(rId); }
//! Creates a 32-bit or 64-bit debug register operand.
static ASMJIT_INLINE_NODEBUG constexpr DReg dr(uint32_t rId) noexcept { return DReg(rId); }
//! Creates an 80-bit st register operand.
static ASMJIT_INLINE_NODEBUG constexpr St st(uint32_t rId) noexcept { return St(rId); }
//! Creates a 128-bit bound register operand.
static ASMJIT_INLINE_NODEBUG constexpr Bnd bnd(uint32_t rId) noexcept { return Bnd(rId); }
//! Creates a TMM register operand.
static ASMJIT_INLINE_NODEBUG constexpr Tmm tmm(uint32_t rId) noexcept { return Tmm(rId); }

static constexpr GpbLo al = GpbLo(Gp::kIdAx);
static constexpr GpbLo bl = GpbLo(Gp::kIdBx);
static constexpr GpbLo cl = GpbLo(Gp::kIdCx);
static constexpr GpbLo dl = GpbLo(Gp::kIdDx);
static constexpr GpbLo spl = GpbLo(Gp::kIdSp);
static constexpr GpbLo bpl = GpbLo(Gp::kIdBp);
static constexpr GpbLo sil = GpbLo(Gp::kIdSi);
static constexpr GpbLo dil = GpbLo(Gp::kIdDi);
static constexpr GpbLo r8b = GpbLo(Gp::kIdR8);
static constexpr GpbLo r9b = GpbLo(Gp::kIdR9);
static constexpr GpbLo r10b = GpbLo(Gp::kIdR10);
static constexpr GpbLo r11b = GpbLo(Gp::kIdR11);
static constexpr GpbLo r12b = GpbLo(Gp::kIdR12);
static constexpr GpbLo r13b = GpbLo(Gp::kIdR13);
static constexpr GpbLo r14b = GpbLo(Gp::kIdR14);
static constexpr GpbLo r15b = GpbLo(Gp::kIdR15);

static constexpr GpbHi ah = GpbHi(Gp::kIdAx);
static constexpr GpbHi bh = GpbHi(Gp::kIdBx);
static constexpr GpbHi ch = GpbHi(Gp::kIdCx);
static constexpr GpbHi dh = GpbHi(Gp::kIdDx);

static constexpr Gpw ax = Gpw(Gp::kIdAx);
static constexpr Gpw bx = Gpw(Gp::kIdBx);
static constexpr Gpw cx = Gpw(Gp::kIdCx);
static constexpr Gpw dx = Gpw(Gp::kIdDx);
static constexpr Gpw sp = Gpw(Gp::kIdSp);
static constexpr Gpw bp = Gpw(Gp::kIdBp);
static constexpr Gpw si = Gpw(Gp::kIdSi);
static constexpr Gpw di = Gpw(Gp::kIdDi);
static constexpr Gpw r8w = Gpw(Gp::kIdR8);
static constexpr Gpw r9w = Gpw(Gp::kIdR9);
static constexpr Gpw r10w = Gpw(Gp::kIdR10);
static constexpr Gpw r11w = Gpw(Gp::kIdR11);
static constexpr Gpw r12w = Gpw(Gp::kIdR12);
static constexpr Gpw r13w = Gpw(Gp::kIdR13);
static constexpr Gpw r14w = Gpw(Gp::kIdR14);
static constexpr Gpw r15w = Gpw(Gp::kIdR15);

static constexpr Gpd eax = Gpd(Gp::kIdAx);
static constexpr Gpd ebx = Gpd(Gp::kIdBx);
static constexpr Gpd ecx = Gpd(Gp::kIdCx);
static constexpr Gpd edx = Gpd(Gp::kIdDx);
static constexpr Gpd esp = Gpd(Gp::kIdSp);
static constexpr Gpd ebp = Gpd(Gp::kIdBp);
static constexpr Gpd esi = Gpd(Gp::kIdSi);
static constexpr Gpd edi = Gpd(Gp::kIdDi);
static constexpr Gpd r8d = Gpd(Gp::kIdR8);
static constexpr Gpd r9d = Gpd(Gp::kIdR9);
static constexpr Gpd r10d = Gpd(Gp::kIdR10);
static constexpr Gpd r11d = Gpd(Gp::kIdR11);
static constexpr Gpd r12d = Gpd(Gp::kIdR12);
static constexpr Gpd r13d = Gpd(Gp::kIdR13);
static constexpr Gpd r14d = Gpd(Gp::kIdR14);
static constexpr Gpd r15d = Gpd(Gp::kIdR15);

static constexpr Gpq rax = Gpq(Gp::kIdAx);
static constexpr Gpq rbx = Gpq(Gp::kIdBx);
static constexpr Gpq rcx = Gpq(Gp::kIdCx);
static constexpr Gpq rdx = Gpq(Gp::kIdDx);
static constexpr Gpq rsp = Gpq(Gp::kIdSp);
static constexpr Gpq rbp = Gpq(Gp::kIdBp);
static constexpr Gpq rsi = Gpq(Gp::kIdSi);
static constexpr Gpq rdi = Gpq(Gp::kIdDi);
static constexpr Gpq r8 = Gpq(Gp::kIdR8);
static constexpr Gpq r9 = Gpq(Gp::kIdR9);
static constexpr Gpq r10 = Gpq(Gp::kIdR10);
static constexpr Gpq r11 = Gpq(Gp::kIdR11);
static constexpr Gpq r12 = Gpq(Gp::kIdR12);
static constexpr Gpq r13 = Gpq(Gp::kIdR13);
static constexpr Gpq r14 = Gpq(Gp::kIdR14);
static constexpr Gpq r15 = Gpq(Gp::kIdR15);

static constexpr Xmm xmm0 = Xmm(0);
static constexpr Xmm xmm1 = Xmm(1);
static constexpr Xmm xmm2 = Xmm(2);
static constexpr Xmm xmm3 = Xmm(3);
static constexpr Xmm xmm4 = Xmm(4);
static constexpr Xmm xmm5 = Xmm(5);
static constexpr Xmm xmm6 = Xmm(6);
static constexpr Xmm xmm7 = Xmm(7);
static constexpr Xmm xmm8 = Xmm(8);
static constexpr Xmm xmm9 = Xmm(9);
static constexpr Xmm xmm10 = Xmm(10);
static constexpr Xmm xmm11 = Xmm(11);
static constexpr Xmm xmm12 = Xmm(12);
static constexpr Xmm xmm13 = Xmm(13);
static constexpr Xmm xmm14 = Xmm(14);
static constexpr Xmm xmm15 = Xmm(15);
static constexpr Xmm xmm16 = Xmm(16);
static constexpr Xmm xmm17 = Xmm(17);
static constexpr Xmm xmm18 = Xmm(18);
static constexpr Xmm xmm19 = Xmm(19);
static constexpr Xmm xmm20 = Xmm(20);
static constexpr Xmm xmm21 = Xmm(21);
static constexpr Xmm xmm22 = Xmm(22);
static constexpr Xmm xmm23 = Xmm(23);
static constexpr Xmm xmm24 = Xmm(24);
static constexpr Xmm xmm25 = Xmm(25);
static constexpr Xmm xmm26 = Xmm(26);
static constexpr Xmm xmm27 = Xmm(27);
static constexpr Xmm xmm28 = Xmm(28);
static constexpr Xmm xmm29 = Xmm(29);
static constexpr Xmm xmm30 = Xmm(30);
static constexpr Xmm xmm31 = Xmm(31);

static constexpr Ymm ymm0 = Ymm(0);
static constexpr Ymm ymm1 = Ymm(1);
static constexpr Ymm ymm2 = Ymm(2);
static constexpr Ymm ymm3 = Ymm(3);
static constexpr Ymm ymm4 = Ymm(4);
static constexpr Ymm ymm5 = Ymm(5);
static constexpr Ymm ymm6 = Ymm(6);
static constexpr Ymm ymm7 = Ymm(7);
static constexpr Ymm ymm8 = Ymm(8);
static constexpr Ymm ymm9 = Ymm(9);
static constexpr Ymm ymm10 = Ymm(10);
static constexpr Ymm ymm11 = Ymm(11);
static constexpr Ymm ymm12 = Ymm(12);
static constexpr Ymm ymm13 = Ymm(13);
static constexpr Ymm ymm14 = Ymm(14);
static constexpr Ymm ymm15 = Ymm(15);
static constexpr Ymm ymm16 = Ymm(16);
static constexpr Ymm ymm17 = Ymm(17);
static constexpr Ymm ymm18 = Ymm(18);
static constexpr Ymm ymm19 = Ymm(19);
static constexpr Ymm ymm20 = Ymm(20);
static constexpr Ymm ymm21 = Ymm(21);
static constexpr Ymm ymm22 = Ymm(22);
static constexpr Ymm ymm23 = Ymm(23);
static constexpr Ymm ymm24 = Ymm(24);
static constexpr Ymm ymm25 = Ymm(25);
static constexpr Ymm ymm26 = Ymm(26);
static constexpr Ymm ymm27 = Ymm(27);
static constexpr Ymm ymm28 = Ymm(28);
static constexpr Ymm ymm29 = Ymm(29);
static constexpr Ymm ymm30 = Ymm(30);
static constexpr Ymm ymm31 = Ymm(31);

static constexpr Zmm zmm0 = Zmm(0);
static constexpr Zmm zmm1 = Zmm(1);
static constexpr Zmm zmm2 = Zmm(2);
static constexpr Zmm zmm3 = Zmm(3);
static constexpr Zmm zmm4 = Zmm(4);
static constexpr Zmm zmm5 = Zmm(5);
static constexpr Zmm zmm6 = Zmm(6);
static constexpr Zmm zmm7 = Zmm(7);
static constexpr Zmm zmm8 = Zmm(8);
static constexpr Zmm zmm9 = Zmm(9);
static constexpr Zmm zmm10 = Zmm(10);
static constexpr Zmm zmm11 = Zmm(11);
static constexpr Zmm zmm12 = Zmm(12);
static constexpr Zmm zmm13 = Zmm(13);
static constexpr Zmm zmm14 = Zmm(14);
static constexpr Zmm zmm15 = Zmm(15);
static constexpr Zmm zmm16 = Zmm(16);
static constexpr Zmm zmm17 = Zmm(17);
static constexpr Zmm zmm18 = Zmm(18);
static constexpr Zmm zmm19 = Zmm(19);
static constexpr Zmm zmm20 = Zmm(20);
static constexpr Zmm zmm21 = Zmm(21);
static constexpr Zmm zmm22 = Zmm(22);
static constexpr Zmm zmm23 = Zmm(23);
static constexpr Zmm zmm24 = Zmm(24);
static constexpr Zmm zmm25 = Zmm(25);
static constexpr Zmm zmm26 = Zmm(26);
static constexpr Zmm zmm27 = Zmm(27);
static constexpr Zmm zmm28 = Zmm(28);
static constexpr Zmm zmm29 = Zmm(29);
static constexpr Zmm zmm30 = Zmm(30);
static constexpr Zmm zmm31 = Zmm(31);

static constexpr Mm mm0 = Mm(0);
static constexpr Mm mm1 = Mm(1);
static constexpr Mm mm2 = Mm(2);
static constexpr Mm mm3 = Mm(3);
static constexpr Mm mm4 = Mm(4);
static constexpr Mm mm5 = Mm(5);
static constexpr Mm mm6 = Mm(6);
static constexpr Mm mm7 = Mm(7);

static constexpr KReg k0 = KReg(0);
static constexpr KReg k1 = KReg(1);
static constexpr KReg k2 = KReg(2);
static constexpr KReg k3 = KReg(3);
static constexpr KReg k4 = KReg(4);
static constexpr KReg k5 = KReg(5);
static constexpr KReg k6 = KReg(6);
static constexpr KReg k7 = KReg(7);

static constexpr SReg no_seg = SReg(SReg::kIdNone);
static constexpr SReg es = SReg(SReg::kIdEs);
static constexpr SReg cs = SReg(SReg::kIdCs);
static constexpr SReg ss = SReg(SReg::kIdSs);
static constexpr SReg ds = SReg(SReg::kIdDs);
static constexpr SReg fs = SReg(SReg::kIdFs);
static constexpr SReg gs = SReg(SReg::kIdGs);

static constexpr CReg cr0 = CReg(0);
static constexpr CReg cr1 = CReg(1);
static constexpr CReg cr2 = CReg(2);
static constexpr CReg cr3 = CReg(3);
static constexpr CReg cr4 = CReg(4);
static constexpr CReg cr5 = CReg(5);
static constexpr CReg cr6 = CReg(6);
static constexpr CReg cr7 = CReg(7);
static constexpr CReg cr8 = CReg(8);
static constexpr CReg cr9 = CReg(9);
static constexpr CReg cr10 = CReg(10);
static constexpr CReg cr11 = CReg(11);
static constexpr CReg cr12 = CReg(12);
static constexpr CReg cr13 = CReg(13);
static constexpr CReg cr14 = CReg(14);
static constexpr CReg cr15 = CReg(15);

static constexpr DReg dr0 = DReg(0);
static constexpr DReg dr1 = DReg(1);
static constexpr DReg dr2 = DReg(2);
static constexpr DReg dr3 = DReg(3);
static constexpr DReg dr4 = DReg(4);
static constexpr DReg dr5 = DReg(5);
static constexpr DReg dr6 = DReg(6);
static constexpr DReg dr7 = DReg(7);
static constexpr DReg dr8 = DReg(8);
static constexpr DReg dr9 = DReg(9);
static constexpr DReg dr10 = DReg(10);
static constexpr DReg dr11 = DReg(11);
static constexpr DReg dr12 = DReg(12);
static constexpr DReg dr13 = DReg(13);
static constexpr DReg dr14 = DReg(14);
static constexpr DReg dr15 = DReg(15);

static constexpr St st0 = St(0);
static constexpr St st1 = St(1);
static constexpr St st2 = St(2);
static constexpr St st3 = St(3);
static constexpr St st4 = St(4);
static constexpr St st5 = St(5);
static constexpr St st6 = St(6);
static constexpr St st7 = St(7);

static constexpr Bnd bnd0 = Bnd(0);
static constexpr Bnd bnd1 = Bnd(1);
static constexpr Bnd bnd2 = Bnd(2);
static constexpr Bnd bnd3 = Bnd(3);

static constexpr Tmm tmm0 = Tmm(0);
static constexpr Tmm tmm1 = Tmm(1);
static constexpr Tmm tmm2 = Tmm(2);
static constexpr Tmm tmm3 = Tmm(3);
static constexpr Tmm tmm4 = Tmm(4);
static constexpr Tmm tmm5 = Tmm(5);
static constexpr Tmm tmm6 = Tmm(6);
static constexpr Tmm tmm7 = Tmm(7);

static constexpr Rip rip = Rip(0);

#ifndef _DOXYGEN
} // {regs}

// Make `x86::regs` accessible through `x86` namespace as well.
using namespace regs;
#endif

//! Memory operand specific to X86 and X86_64 architecture.
class Mem : public BaseMem {
public:
  //! \name Constants
  //! \{

  //! Additional bits of operand's signature used by `x86::Mem`.
  enum AdditionalBits : uint32_t {
    // Memory address type (2 bits).
    // |........|........|XX......|........|
    kSignatureMemAddrTypeShift = 14,
    kSignatureMemAddrTypeMask = 0x03u << kSignatureMemAddrTypeShift,

    // Memory shift amount (2 bits).
    // |........|......XX|........|........|
    kSignatureMemShiftValueShift = 16,
    kSignatureMemShiftValueMask = 0x03u << kSignatureMemShiftValueShift,

    // Memory segment reg (3 bits).
    // |........|...XXX..|........|........|
    kSignatureMemSegmentShift = 18,
    kSignatureMemSegmentMask = 0x07u << kSignatureMemSegmentShift,

    // Memory broadcast type (3 bits).
    // |........|XXX.....|........|........|
    kSignatureMemBroadcastShift = 21,
    kSignatureMemBroadcastMask = 0x7u << kSignatureMemBroadcastShift
  };

  //! Address type.
  enum class AddrType : uint32_t {
    //! Default address type, Assembler will select the best type when necessary.
    kDefault = 0,
    //! Absolute address type.
    kAbs = 1,
    //! Relative address type.
    kRel = 2,

    //! Maximum value of `AddrType`.
    kMaxValue = kRel
  };

  //! Memory broadcast type.
  enum class Broadcast : uint32_t {
    //! No broadcast (regular memory operand).
    kNone = 0,
    //! Broadcast {1to2}.
    k1To2 = 1,
    //! Broadcast {1to4}.
    k1To4 = 2,
    //! Broadcast {1to8}.
    k1To8 = 3,
    //! Broadcast {1to16}.
    k1To16 = 4,
    //! Broadcast {1to32}.
    k1To32 = 5,
    //! Broadcast {1to64}.
    k1To64 = 6,

    //! Maximum value of `Broadcast`.
    kMaxValue = k1To64
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a default `Mem` operand that points to [0].
  ASMJIT_INLINE_NODEBUG constexpr Mem() noexcept
    : BaseMem() {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(const Mem& other) noexcept
    : BaseMem(other) {}

  ASMJIT_INLINE_NODEBUG explicit Mem(Globals::NoInit_) noexcept
    : BaseMem(Globals::NoInit) {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(const Signature& signature, uint32_t baseId, uint32_t indexId, int32_t offset) noexcept
    : BaseMem(signature, baseId, indexId, offset) {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(const Label& base, int32_t off, uint32_t size = 0, Signature signature = OperandSignature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(RegType::kLabelTag) |
              Signature::fromSize(size) |
              signature, base.id(), 0, off) {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(const Label& base, const BaseReg& index, uint32_t shift, int32_t off, uint32_t size = 0, Signature signature = OperandSignature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(RegType::kLabelTag) |
              Signature::fromMemIndexType(index.type()) |
              Signature::fromValue<kSignatureMemShiftValueMask>(shift) |
              Signature::fromSize(size) |
              signature, base.id(), index.id(), off) {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(const BaseReg& base, int32_t off, uint32_t size = 0, Signature signature = OperandSignature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(base.type()) |
              Signature::fromSize(size) |
              signature, base.id(), 0, off) {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(const BaseReg& base, const BaseReg& index, uint32_t shift, int32_t off, uint32_t size = 0, Signature signature = OperandSignature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemBaseType(base.type()) |
              Signature::fromMemIndexType(index.type()) |
              Signature::fromValue<kSignatureMemShiftValueMask>(shift) |
              Signature::fromSize(size) |
              signature, base.id(), index.id(), off) {}

  ASMJIT_INLINE_NODEBUG constexpr explicit Mem(uint64_t base, uint32_t size = 0, Signature signature = OperandSignature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromSize(size) |
              signature, uint32_t(base >> 32), 0, int32_t(uint32_t(base & 0xFFFFFFFFu))) {}

  ASMJIT_INLINE_NODEBUG constexpr Mem(uint64_t base, const BaseReg& index, uint32_t shift = 0, uint32_t size = 0, Signature signature = OperandSignature{0}) noexcept
    : BaseMem(Signature::fromOpType(OperandType::kMem) |
              Signature::fromMemIndexType(index.type()) |
              Signature::fromValue<kSignatureMemShiftValueMask>(shift) |
              Signature::fromSize(size) |
              signature, uint32_t(base >> 32), index.id(), int32_t(uint32_t(base & 0xFFFFFFFFu))) {}

  //! \}

  //! \name Overloaded Operators
  //! \{

  ASMJIT_INLINE_NODEBUG Mem& operator=(const Mem& other) noexcept = default;

  //! \}

  //! \name Clone
  //! \{

  //! Clones the memory operand.
  ASMJIT_INLINE_NODEBUG constexpr Mem clone() const noexcept { return Mem(*this); }

  //! Creates a copy of this memory operand adjusted by `off`.
  inline Mem cloneAdjusted(int64_t off) const noexcept {
    Mem result(*this);
    result.addOffset(off);
    return result;
  }

  //! Creates a copy of this memory operand resized to `size`.
  inline Mem cloneResized(uint32_t size) const noexcept {
    Mem result(*this);
    result.setSize(size);
    return result;
  }

  //! Creates a copy of this memory operand with a broadcast `bcst`.
  ASMJIT_INLINE_NODEBUG constexpr Mem cloneBroadcasted(Broadcast bcst) const noexcept {
    return Mem((_signature & ~Signature{kSignatureMemBroadcastMask}) | Signature::fromValue<kSignatureMemBroadcastMask>(bcst), _baseId, _data[0], int32_t(_data[1]));
  }

  //! \}

  //! \name Base & Index
  //! \{

  //! Converts memory `baseType` and `baseId` to `x86::Reg` instance.
  //!
  //! The memory must have a valid base register otherwise the result will be wrong.
  ASMJIT_INLINE_NODEBUG Reg baseReg() const noexcept { return Reg::fromTypeAndId(baseType(), baseId()); }

  //! Converts memory `indexType` and `indexId` to `x86::Reg` instance.
  //!
  //! The memory must have a valid index register otherwise the result will be wrong.
  ASMJIT_INLINE_NODEBUG Reg indexReg() const noexcept { return Reg::fromTypeAndId(indexType(), indexId()); }

  using BaseMem::setIndex;

  ASMJIT_INLINE_NODEBUG void setIndex(const BaseReg& index, uint32_t shift) noexcept {
    setIndex(index);
    setShift(shift);
  }

  //! \}

  //! \name Memory Size
  //! \{

  //! Tests whether the memory operand specifies a size (i.e. the size is not zero).
  ASMJIT_INLINE_NODEBUG constexpr bool hasSize() const noexcept { return _signature.hasField<Signature::kSizeMask>(); }
  //! Tests whether the memory operand size matches size `s`.
  ASMJIT_INLINE_NODEBUG constexpr bool hasSize(uint32_t s) const noexcept { return size() == s; }

  //! Returns the size of the memory operand in bytes.
  //!
  //! \note Most instructions would deduce the size of the memory operand, so in most cases it's expected that the
  //! returned value would be zero. However, some instruction require the size to select between multiple variations,
  //! so in some cases size is required would be non-zero (for example `inc [mem], immediate` requires size to
  //! distinguish between 8-bit, 16-bit, 32-bit, and 64-bit increments.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t size() const noexcept { return _signature.getField<Signature::kSizeMask>(); }

  //! Sets the memory operand size (in bytes).
  ASMJIT_INLINE_NODEBUG void setSize(uint32_t size) noexcept { _signature.setField<Signature::kSizeMask>(size); }

  //! \}

  //! \name Address Type
  //! \{

  //! Returns the address type of the memory operand.
  //!
  //! By default, address type of newly created memory operands is always \ref AddrType::kDefault.
  ASMJIT_INLINE_NODEBUG constexpr AddrType addrType() const noexcept { return (AddrType)_signature.getField<kSignatureMemAddrTypeMask>(); }
  //! Sets the address type to `addrType`.
  ASMJIT_INLINE_NODEBUG void setAddrType(AddrType addrType) noexcept { _signature.setField<kSignatureMemAddrTypeMask>(uint32_t(addrType)); }
  //! Resets the address type to \ref AddrType::kDefault.
  ASMJIT_INLINE_NODEBUG void resetAddrType() noexcept { _signature.setField<kSignatureMemAddrTypeMask>(uint32_t(AddrType::kDefault)); }

  //! Tests whether the address type is \ref AddrType::kAbs.
  ASMJIT_INLINE_NODEBUG constexpr bool isAbs() const noexcept { return addrType() == AddrType::kAbs; }
  //! Sets the address type to \ref AddrType::kAbs.
  ASMJIT_INLINE_NODEBUG void setAbs() noexcept { setAddrType(AddrType::kAbs); }

  //! Tests whether the address type is \ref AddrType::kRel.
  ASMJIT_INLINE_NODEBUG constexpr bool isRel() const noexcept { return addrType() == AddrType::kRel; }
  //! Sets the address type to \ref AddrType::kRel.
  ASMJIT_INLINE_NODEBUG void setRel() noexcept { setAddrType(AddrType::kRel); }

  //! \}

  //! \name Segment
  //! \{

  //! Tests whether the memory operand has a segment override.
  ASMJIT_INLINE_NODEBUG constexpr bool hasSegment() const noexcept { return _signature.hasField<kSignatureMemSegmentMask>(); }
  //! Returns the associated segment override as `SReg` operand.
  ASMJIT_INLINE_NODEBUG constexpr SReg segment() const noexcept { return SReg(segmentId()); }
  //! Returns segment override register id, see `SReg::Id`.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t segmentId() const noexcept { return _signature.getField<kSignatureMemSegmentMask>(); }

  //! Sets the segment override to `seg`.
  ASMJIT_INLINE_NODEBUG void setSegment(const SReg& seg) noexcept { setSegment(seg.id()); }
  //! Sets the segment override to `id`.
  ASMJIT_INLINE_NODEBUG void setSegment(uint32_t rId) noexcept { _signature.setField<kSignatureMemSegmentMask>(rId); }
  //! Resets the segment override.
  ASMJIT_INLINE_NODEBUG void resetSegment() noexcept { _signature.setField<kSignatureMemSegmentMask>(0); }

  //! \}

  //! \name Shift
  //! \{

  //! Tests whether the memory operand has shift (aka scale) value.
  ASMJIT_INLINE_NODEBUG constexpr bool hasShift() const noexcept { return _signature.hasField<kSignatureMemShiftValueMask>(); }
  //! Returns the memory operand's shift (aka scale) value.
  ASMJIT_INLINE_NODEBUG constexpr uint32_t shift() const noexcept { return _signature.getField<kSignatureMemShiftValueMask>(); }
  //! Sets the memory operand's shift (aka scale) value.
  ASMJIT_INLINE_NODEBUG void setShift(uint32_t shift) noexcept { _signature.setField<kSignatureMemShiftValueMask>(shift); }
  //! Resets the memory operand's shift (aka scale) value to zero.
  ASMJIT_INLINE_NODEBUG void resetShift() noexcept { _signature.setField<kSignatureMemShiftValueMask>(0); }

  //! \}

  //! \name Broadcast
  //! \{

  //! Tests whether the memory operand has broadcast {1tox}.
  ASMJIT_INLINE_NODEBUG constexpr bool hasBroadcast() const noexcept { return _signature.hasField<kSignatureMemBroadcastMask>(); }
  //! Returns the memory operand's broadcast.
  ASMJIT_INLINE_NODEBUG constexpr Broadcast getBroadcast() const noexcept { return (Broadcast)_signature.getField<kSignatureMemBroadcastMask>(); }
  //! Sets the memory operand's broadcast.
  ASMJIT_INLINE_NODEBUG void setBroadcast(Broadcast b) noexcept { _signature.setField<kSignatureMemBroadcastMask>(uint32_t(b)); }
  //! Resets the memory operand's broadcast to none.
  ASMJIT_INLINE_NODEBUG void resetBroadcast() noexcept { _signature.setField<kSignatureMemBroadcastMask>(0); }

  //! Returns a new `Mem` without a broadcast (the possible broadcast is cleared).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to1() const noexcept { return cloneBroadcasted(Broadcast::kNone); }
  //! Returns a new `Mem` with {1to2} broadcast (AVX-512).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to2() const noexcept { return cloneBroadcasted(Broadcast::k1To2); }
  //! Returns a new `Mem` with {1to4} broadcast (AVX-512).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to4() const noexcept { return cloneBroadcasted(Broadcast::k1To4); }
  //! Returns a new `Mem` with {1to8} broadcast (AVX-512).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to8() const noexcept { return cloneBroadcasted(Broadcast::k1To8); }
  //! Returns a new `Mem` with {1to16} broadcast (AVX-512).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to16() const noexcept { return cloneBroadcasted(Broadcast::k1To16); }
  //! Returns a new `Mem` with {1to32} broadcast (AVX-512).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to32() const noexcept { return cloneBroadcasted(Broadcast::k1To32); }
  //! Returns a new `Mem` with {1to64} broadcast (AVX-512).
  ASMJIT_INLINE_NODEBUG constexpr Mem _1to64() const noexcept { return cloneBroadcasted(Broadcast::k1To64); }

  //! \}
};

//! Creates `[base.reg + offset]` memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Gp& base, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(base, offset, size);
}
//! Creates `[base.reg + (index << shift) + offset]` memory operand (scalar index).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Gp& base, const Gp& index, uint32_t shift = 0, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, offset, size);
}
//! Creates `[base.reg + (index << shift) + offset]` memory operand (vector index).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Gp& base, const Vec& index, uint32_t shift = 0, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, offset, size);
}

//! Creates `[base + offset]` memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Label& base, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(base, offset, size);
}
//! Creates `[base + (index << shift) + offset]` memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Label& base, const Gp& index, uint32_t shift = 0, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, offset, size);
}
//! Creates `[base + (index << shift) + offset]` memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Label& base, const Vec& index, uint32_t shift = 0, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, offset, size);
}

//! Creates `[rip + offset]` memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(const Rip& rip_, int32_t offset = 0, uint32_t size = 0) noexcept {
  return Mem(rip_, offset, size);
}

//! Creates `[base]` absolute memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(uint64_t base, uint32_t size = 0) noexcept {
  return Mem(base, size);
}
//! Creates `[base + (index.reg << shift)]` absolute memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(uint64_t base, const Reg& index, uint32_t shift = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, size);
}
//! Creates `[base + (index.reg << shift)]` absolute memory operand.
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr(uint64_t base, const Vec& index, uint32_t shift = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, size);
}

//! Creates `[base]` absolute memory operand (absolute).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_abs(uint64_t base, uint32_t size = 0) noexcept {
  return Mem(base, size, OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kAbs));
}
//! Creates `[base + (index.reg << shift)]` absolute memory operand (absolute).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_abs(uint64_t base, const Reg& index, uint32_t shift = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, size, OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kAbs));
}
//! Creates `[base + (index.reg << shift)]` absolute memory operand (absolute).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_abs(uint64_t base, const Vec& index, uint32_t shift = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, size, OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kAbs));
}

//! Creates `[base]` relative memory operand (relative).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_rel(uint64_t base, uint32_t size = 0) noexcept {
  return Mem(base, size, OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kRel));
}
//! Creates `[base + (index.reg << shift)]` relative memory operand (relative).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_rel(uint64_t base, const Reg& index, uint32_t shift = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, size, OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kRel));
}
//! Creates `[base + (index.reg << shift)]` relative memory operand (relative).
static ASMJIT_INLINE_NODEBUG constexpr Mem ptr_rel(uint64_t base, const Vec& index, uint32_t shift = 0, uint32_t size = 0) noexcept {
  return Mem(base, index, shift, size, OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kRel));
}

#define ASMJIT_MEM_PTR(FUNC, SIZE)                                                             \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    const Gp& base, int32_t offset = 0) noexcept                                               \
      { return Mem(base, offset, SIZE); }                                                      \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    const Gp& base, const Gp& index, uint32_t shift = 0, int32_t offset = 0) noexcept          \
      { return Mem(base, index, shift, offset, SIZE); }                                        \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    const Gp& base, const Vec& index, uint32_t shift = 0, int32_t offset = 0) noexcept         \
      { return Mem(base, index, shift, offset, SIZE); }                                        \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    const Label& base, int32_t offset = 0) noexcept                                            \
      { return Mem(base, offset, SIZE); }                                                      \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    const Label& base, const Gp& index, uint32_t shift = 0, int32_t offset = 0) noexcept       \
      { return Mem(base, index, shift, offset, SIZE); }                                        \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    const Rip& rip_, int32_t offset = 0) noexcept                                              \
      { return Mem(rip_, offset, SIZE); }                                                      \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    uint64_t base) noexcept                                                                    \
      { return Mem(base, SIZE); }                                                              \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    uint64_t base, const Gp& index, uint32_t shift = 0) noexcept                               \
      { return Mem(base, index, shift, SIZE); }                                                \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC(                                             \
    uint64_t base, const Vec& index, uint32_t shift = 0) noexcept                              \
      { return Mem(base, index, shift, SIZE); }                                                \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC##_abs(                                       \
    uint64_t base) noexcept                                                                    \
      { return Mem(base, SIZE,                                                                 \
          OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kAbs)); } \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC##_abs(                                       \
    uint64_t base, const Gp& index, uint32_t shift = 0) noexcept                               \
      { return Mem(base, index, shift, SIZE,                                                   \
          OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kAbs)); } \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC##_abs(                                       \
    uint64_t base, const Vec& index, uint32_t shift = 0) noexcept                              \
      { return Mem(base, index, shift, SIZE,                                                   \
          OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kAbs)); } \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC##_rel(                                       \
    uint64_t base) noexcept                                                                    \
      { return Mem(base, SIZE,                                                                 \
          OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kRel)); } \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC##_rel(                                       \
    uint64_t base, const Gp& index, uint32_t shift = 0) noexcept                               \
      { return Mem(base, index, shift, SIZE,                                                   \
          OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kRel)); } \
                                                                                               \
  static ASMJIT_INLINE_NODEBUG constexpr Mem FUNC##_rel(                                       \
    uint64_t base, const Vec& index, uint32_t shift = 0) noexcept                              \
      { return Mem(base, index, shift, SIZE,                                                   \
          OperandSignature::fromValue<Mem::kSignatureMemAddrTypeMask>(Mem::AddrType::kRel)); }

// Definition of memory operand constructors that use platform independent naming.
ASMJIT_MEM_PTR(ptr_8, 1)
ASMJIT_MEM_PTR(ptr_16, 2)
ASMJIT_MEM_PTR(ptr_32, 4)
ASMJIT_MEM_PTR(ptr_48, 6)
ASMJIT_MEM_PTR(ptr_64, 8)
ASMJIT_MEM_PTR(ptr_80, 10)
ASMJIT_MEM_PTR(ptr_128, 16)
ASMJIT_MEM_PTR(ptr_256, 32)
ASMJIT_MEM_PTR(ptr_512, 64)

// Definition of memory operand constructors that use X86-specific convention.
ASMJIT_MEM_PTR(byte_ptr, 1)
ASMJIT_MEM_PTR(word_ptr, 2)
ASMJIT_MEM_PTR(dword_ptr, 4)
ASMJIT_MEM_PTR(fword_ptr, 6)
ASMJIT_MEM_PTR(qword_ptr, 8)
ASMJIT_MEM_PTR(tbyte_ptr, 10)
ASMJIT_MEM_PTR(tword_ptr, 10)
ASMJIT_MEM_PTR(oword_ptr, 16)
ASMJIT_MEM_PTR(dqword_ptr, 16)
ASMJIT_MEM_PTR(qqword_ptr, 32)
ASMJIT_MEM_PTR(xmmword_ptr, 16)
ASMJIT_MEM_PTR(ymmword_ptr, 32)
ASMJIT_MEM_PTR(zmmword_ptr, 64)

#undef ASMJIT_MEM_PTR

//! \}

ASMJIT_END_SUB_NAMESPACE

//! \cond INTERNAL
ASMJIT_BEGIN_NAMESPACE
ASMJIT_DEFINE_TYPE_ID(x86::Gpb, TypeId::kInt8);
ASMJIT_DEFINE_TYPE_ID(x86::Gpw, TypeId::kInt16);
ASMJIT_DEFINE_TYPE_ID(x86::Gpd, TypeId::kInt32);
ASMJIT_DEFINE_TYPE_ID(x86::Gpq, TypeId::kInt64);
ASMJIT_DEFINE_TYPE_ID(x86::Mm , TypeId::kMmx64);
ASMJIT_DEFINE_TYPE_ID(x86::Xmm, TypeId::kInt32x4);
ASMJIT_DEFINE_TYPE_ID(x86::Ymm, TypeId::kInt32x8);
ASMJIT_DEFINE_TYPE_ID(x86::Zmm, TypeId::kInt32x16);
ASMJIT_END_NAMESPACE
//! \endcond

#endif // ASMJIT_X86_X86OPERAND_H_INCLUDED
