// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_ARM_A64INSTDB_H_P_INCLUDED
#define ASMJIT_ARM_A64INSTDB_H_P_INCLUDED

#include "../core/codeholder.h"
#include "../arm/a64instdb.h"
#include "../arm/a64operand.h"

ASMJIT_BEGIN_SUB_NAMESPACE(a64)

//! \cond INTERNAL
//! \addtogroup asmjit_a64
//! \{

namespace InstDB {

// a64::InstDB - Constants Used by Instructions
// ============================================

// GP register types supported by base instructions.
static constexpr uint32_t kW = 0x1;
static constexpr uint32_t kX = 0x2;
static constexpr uint32_t kWX = 0x3;

// GP high register IDs supported by the instruction.
static constexpr uint32_t kZR = Gp::kIdZr;
static constexpr uint32_t kSP = Gp::kIdSp;

// a64::InstDB - RWInfo
// ====================

enum RWInfoType : uint32_t {
  kRWI_R,
  kRWI_RW,
  kRWI_RX,
  kRWI_RRW,
  kRWI_RWX,
  kRWI_W,
  kRWI_WRW,
  kRWI_WRX,
  kRWI_WRRW,
  kRWI_WRRX,
  kRWI_WW,
  kRWI_X,
  kRWI_XRX,
  kRWI_XXRRX,

  kRWI_LDn,
  kRWI_STn,

  kRWI_SpecialStart = kRWI_LDn
};

// a64::InstDB - ElementType
// =========================

enum ElementType : uint8_t {
  kET_None = Vec::kElementTypeNone,
  kET_B    = Vec::kElementTypeB,
  kET_H    = Vec::kElementTypeH,
  kET_S    = Vec::kElementTypeS,
  kET_D    = Vec::kElementTypeD,
  kET_2H   = Vec::kElementTypeH2,
  kET_4B   = Vec::kElementTypeB4
};

// a64::InstDB - GpType
// ====================

enum GpType : uint8_t {
  kGp_W,
  kGp_X,
  kGp_X_SP
};

// a64::InstDB - OPSig
// ===================

enum kOpSignature : uint32_t {
  kOp_GpW = GpW::kSignature,
  kOp_GpX = GpX::kSignature,

  kOp_B = VecB::kSignature,
  kOp_H = VecH::kSignature,
  kOp_S = VecS::kSignature,
  kOp_D = VecD::kSignature,
  kOp_Q = VecV::kSignature,

  kOp_V8B = VecD::kSignature | Vec::kSignatureElementB,
  kOp_V4H = VecD::kSignature | Vec::kSignatureElementH,
  kOp_V2S = VecD::kSignature | Vec::kSignatureElementS,

  kOp_V16B = VecV::kSignature | Vec::kSignatureElementB,
  kOp_V8H = VecV::kSignature | Vec::kSignatureElementH,
  kOp_V4S = VecV::kSignature | Vec::kSignatureElementS,
  kOp_V2D = VecV::kSignature | Vec::kSignatureElementD
};

// a64::InstDB - HFConv
// ====================

enum kHFConv : uint32_t {
  //! FP16 version of the instruction is not available.
  kHF_N,

  //! Doesn't do any change to the opcode.
  kHF_0,

  kHF_A,
  kHF_B,
  kHF_C,
  kHF_D,

  kHF_Count
};

// a64::InstDB - VOType
// ====================

//! Vector operand type combinations used by FP&SIMD instructions.
enum VOType : uint32_t {
  kVO_V_B,
  kVO_V_BH,
  kVO_V_BH_4S,
  kVO_V_BHS,
  kVO_V_BHS_D2,
  kVO_V_HS,
  kVO_V_S,

  kVO_V_B8H4,
  kVO_V_B8H4S2,
  kVO_V_B8D1,
  kVO_V_H4S2,

  kVO_V_B16,
  kVO_V_B16H8,
  kVO_V_B16H8S4,
  kVO_V_B16D2,
  kVO_V_H8S4,
  kVO_V_S4,
  kVO_V_D2,

  kVO_SV_BHS,
  kVO_SV_B8H4S2,
  kVO_SV_HS,
  kVO_V_Any,
  kVO_SV_Any,

  kVO_Count
};

// a64::InstDB - EncodingId
// ========================

// ${EncodingId:Begin}
// ------------------- Automatically generated, do not edit -------------------
enum EncodingId : uint32_t {
  kEncodingNone = 0,
  kEncodingBaseAddSub,
  kEncodingBaseAdr,
  kEncodingBaseAtDcIcTlbi,
  kEncodingBaseAtomicCasp,
  kEncodingBaseAtomicOp,
  kEncodingBaseAtomicSt,
  kEncodingBaseBfc,
  kEncodingBaseBfi,
  kEncodingBaseBfm,
  kEncodingBaseBfx,
  kEncodingBaseBranchCmp,
  kEncodingBaseBranchReg,
  kEncodingBaseBranchRel,
  kEncodingBaseBranchTst,
  kEncodingBaseCCmp,
  kEncodingBaseCInc,
  kEncodingBaseCSel,
  kEncodingBaseCSet,
  kEncodingBaseCmpCmn,
  kEncodingBaseExtend,
  kEncodingBaseExtract,
  kEncodingBaseLdSt,
  kEncodingBaseLdpStp,
  kEncodingBaseLdxp,
  kEncodingBaseLogical,
  kEncodingBaseMov,
  kEncodingBaseMovKNZ,
  kEncodingBaseMrs,
  kEncodingBaseMsr,
  kEncodingBaseMvnNeg,
  kEncodingBaseOp,
  kEncodingBaseOpImm,
  kEncodingBaseR,
  kEncodingBaseRM_NoImm,
  kEncodingBaseRM_SImm10,
  kEncodingBaseRM_SImm9,
  kEncodingBaseRR,
  kEncodingBaseRRII,
  kEncodingBaseRRR,
  kEncodingBaseRRRR,
  kEncodingBaseRev,
  kEncodingBaseShift,
  kEncodingBaseStx,
  kEncodingBaseStxp,
  kEncodingBaseSys,
  kEncodingBaseTst,
  kEncodingFSimdPair,
  kEncodingFSimdSV,
  kEncodingFSimdVV,
  kEncodingFSimdVVV,
  kEncodingFSimdVVVV,
  kEncodingFSimdVVVe,
  kEncodingISimdPair,
  kEncodingISimdSV,
  kEncodingISimdVV,
  kEncodingISimdVVV,
  kEncodingISimdVVVI,
  kEncodingISimdVVVV,
  kEncodingISimdVVVVx,
  kEncodingISimdVVVe,
  kEncodingISimdVVVx,
  kEncodingISimdVVx,
  kEncodingISimdWWV,
  kEncodingSimdBicOrr,
  kEncodingSimdCmp,
  kEncodingSimdDot,
  kEncodingSimdDup,
  kEncodingSimdFcadd,
  kEncodingSimdFccmpFccmpe,
  kEncodingSimdFcm,
  kEncodingSimdFcmla,
  kEncodingSimdFcmpFcmpe,
  kEncodingSimdFcsel,
  kEncodingSimdFcvt,
  kEncodingSimdFcvtLN,
  kEncodingSimdFcvtSV,
  kEncodingSimdFmlal,
  kEncodingSimdFmov,
  kEncodingSimdIns,
  kEncodingSimdLdNStN,
  kEncodingSimdLdSt,
  kEncodingSimdLdpStp,
  kEncodingSimdLdurStur,
  kEncodingSimdMov,
  kEncodingSimdMoviMvni,
  kEncodingSimdShift,
  kEncodingSimdShiftES,
  kEncodingSimdSm3tt,
  kEncodingSimdSmovUmov,
  kEncodingSimdSxtlUxtl,
  kEncodingSimdTblTbx
};
// ----------------------------------------------------------------------------
// ${EncodingId:End}

// a64::InstDB::EncodingData
// =========================

namespace EncodingData {

#define M_OPCODE(field, bits) \
  uint32_t _##field : bits; \
  inline constexpr uint32_t field() const noexcept { return uint32_t(_##field) << (32 - bits); }

struct BaseOp {
  uint32_t opcode;
};

struct BaseOpImm {
  uint32_t opcode;
  uint16_t immBits;
  uint16_t immOffset;
};

struct BaseR {
  uint32_t opcode;
  uint32_t rType : 8;
  uint32_t rHiId : 8;
  uint32_t rShift : 8;
};

struct BaseRR {
  uint32_t opcode;
  uint32_t aType : 2;
  uint32_t aHiId : 6;
  uint32_t aShift : 5;
  uint32_t bType : 2;
  uint32_t bHiId : 6;
  uint32_t bShift : 5;
  uint32_t uniform : 1;
};

struct BaseRRR {
  M_OPCODE(opcode, 22)
  uint32_t aType : 2;
  uint32_t aHiId : 6;
  uint32_t bType : 2;
  uint32_t bHiId : 6;
  uint32_t cType : 2;
  uint32_t cHiId : 6;
  uint32_t uniform : 1;
};

struct BaseRRRR {
  M_OPCODE(opcode, 22)
  uint32_t aType : 2;
  uint32_t aHiId : 6;
  uint32_t bType : 2;
  uint32_t bHiId : 6;
  uint32_t cType : 2;
  uint32_t cHiId : 6;
  uint32_t dType : 2;
  uint32_t dHiId : 6;
  uint32_t uniform : 1;
};

struct BaseRRII {
  M_OPCODE(opcode, 22)
  uint32_t aType : 2;
  uint32_t aHiId : 6;
  uint32_t bType : 2;
  uint32_t bHiId : 6;
  uint32_t aImmSize : 6;
  uint32_t aImmDiscardLsb : 5;
  uint32_t aImmOffset : 5;
  uint32_t bImmSize : 6;
  uint32_t bImmDiscardLsb : 5;
  uint32_t bImmOffset : 5;
};

struct BaseAtDcIcTlbi {
  uint32_t immVerifyMask : 14;
  uint32_t immVerifyData : 14;
  uint32_t mandatoryReg : 1;
};

struct BaseAdcSbc {
  uint32_t opcode;
};

struct BaseAddSub {
  uint32_t shiftedOp  : 10; // sf|.......|Sh|.|Rm|  Imm:6 |Rn|Rd|
  uint32_t extendedOp : 10; // sf|.......|..|.|Rm|Opt|Imm3|Rn|Rd|
  uint32_t immediateOp: 10; // sf|.......|Sh|    Imm:12   |Rn|Rd|
};

struct BaseAdr {
  M_OPCODE(opcode, 22)
  OffsetType offsetType : 8;
};

struct BaseBfm {
  uint32_t opcode;         // sf|........|N|ImmR:6|ImmS:6|Rn|Rd|
};

struct BaseCmpCmn {
  uint32_t shiftedOp  : 10; // sf|.......|Sh|.|Rm|  Imm:6 |Rn|11111|
  uint32_t extendedOp : 10; // sf|.......|..|.|Rm|Opt|Imm3|Rn|11111|
  uint32_t immediateOp: 10; // sf|.......|Sh|    Imm:12   |Rn|11111|
};

struct BaseExtend {
  M_OPCODE(opcode, 22)      // sf|........|N|......|......|Rn|Rd|
  uint32_t rType : 2;
  uint32_t u : 1;
};

struct BaseLogical {
  uint32_t shiftedOp  : 10; // sf|.......|Sh|.|Rm|  Imm:6 |Rn|Rd|
  uint32_t immediateOp: 10; // sf|........|N|ImmR:6|ImmS:6|Rn|Rd|
  uint32_t negateImm  : 1 ; // True if this is an operation that must negate IMM.
};

struct BaseMvnNeg {
  uint32_t opcode;
};

struct BaseShift {
  M_OPCODE(registerOp, 22)
  M_OPCODE(immediateOp, 22)
  uint32_t ror : 2;
};

struct BaseTst {
  uint32_t shiftedOp  : 10; // sf|.......|Sh|.|Rm|  Imm:6 |Rn|11111|
  uint32_t immediateOp: 10; // sf|........|N|ImmR:6|ImmS:6|Rn|11111|
};

struct BaseRM_NoImm {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t rHiId : 6;
  uint32_t xOffset : 5;
};

struct BaseRM_SImm9 {
  M_OPCODE(offsetOp, 22)
  M_OPCODE(prePostOp, 22)
  uint32_t rType : 2;
  uint32_t rHiId : 6;
  uint32_t xOffset : 5;
  uint32_t immShift : 4;
};

struct BaseRM_SImm10 {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t rHiId : 6;
  uint32_t xOffset : 5;
  uint32_t immShift : 4;
};

struct BaseLdSt {
  uint32_t uOffsetOp  : 10;
  uint32_t prePostOp  : 11;
  uint32_t registerOp : 11;
  uint32_t literalOp  : 8;
  uint32_t rType      : 2;
  uint32_t xOffset    : 5;
  uint32_t uOffsetShift : 3;
  uint32_t uAltInstId : 14;
};

struct BaseLdpStp {
  uint32_t offsetOp : 10;
  uint32_t prePostOp : 10;
  uint32_t rType : 2;
  uint32_t xOffset : 5;
  uint32_t offsetShift : 3;
};

struct BaseStx {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t xOffset : 5;
};

struct BaseLdxp {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t xOffset : 5;
};

struct BaseStxp {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t xOffset : 5;
};

struct BaseAtomicOp {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t xOffset : 5;
  uint32_t zr : 1;
};

struct BaseAtomicSt {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t xOffset : 5;
};

struct BaseAtomicCasp {
  M_OPCODE(opcode, 22)
  uint32_t rType : 2;
  uint32_t xOffset : 5;
};

typedef BaseOp BaseBranchReg;
typedef BaseOp BaseBranchRel;
typedef BaseOp BaseBranchCmp;
typedef BaseOp BaseBranchTst;
typedef BaseOp BaseExtract;
typedef BaseOp BaseBfc;
typedef BaseOp BaseBfi;
typedef BaseOp BaseBfx;
typedef BaseOp BaseCCmp;
typedef BaseOp BaseCInc;
typedef BaseOp BaseCSet;
typedef BaseOp BaseCSel;
typedef BaseOp BaseMovKNZ;
typedef BaseOp BaseMull;

struct FSimdGeneric {
  uint32_t _scalarOp : 28;
  uint32_t _scalarHf : 4;
  uint32_t _vectorOp : 28;
  uint32_t _vectorHf : 4;

  constexpr uint32_t scalarOp() const noexcept { return uint32_t(_scalarOp) << 10; }
  constexpr uint32_t vectorOp() const noexcept { return uint32_t(_vectorOp) << 10; }
  constexpr uint32_t scalarHf() const noexcept { return uint32_t(_scalarHf); }
  constexpr uint32_t vectorHf() const noexcept { return uint32_t(_vectorHf); }
};

typedef FSimdGeneric FSimdVV;
typedef FSimdGeneric FSimdVVV;
typedef FSimdGeneric FSimdVVVV;

struct FSimdSV {
  uint32_t opcode;
};

struct FSimdVVVe {
  uint32_t _scalarOp : 28;
  uint32_t _scalarHf : 4;
  uint32_t _vectorOp;
  uint32_t _elementOp;

  constexpr uint32_t scalarOp() const noexcept { return uint32_t(_scalarOp) << 10; }
  constexpr uint32_t scalarHf() const noexcept { return uint32_t(_scalarHf); };
  constexpr uint32_t vectorOp() const noexcept { return uint32_t(_vectorOp) << 10; }
  constexpr uint32_t vectorHf() const noexcept { return kHF_C; }
  constexpr uint32_t elementScalarOp() const noexcept { return (uint32_t(_elementOp) << 10) | (0x5u << 28); }
  constexpr uint32_t elementVectorOp() const noexcept { return (uint32_t(_elementOp) << 10); }
};

struct SimdFcadd {
  uint32_t _opcode;

  constexpr uint32_t opcode() const noexcept { return _opcode << 10; }
};

struct SimdFcmla {
  uint32_t _regularOp;
  uint32_t _elementOp;

  constexpr uint32_t regularOp() const noexcept { return uint32_t(_regularOp) << 10; }
  constexpr uint32_t elementOp() const noexcept { return (uint32_t(_elementOp) << 10); }
};

struct SimdFccmpFccmpe {
  uint32_t _opcode;
  constexpr uint32_t opcode() const noexcept { return _opcode; }
};

struct SimdFcm {
  uint32_t _registerOp : 28;
  uint32_t _registerHf : 4;

  uint32_t _zeroOp : 28;

  constexpr bool hasRegisterOp() const noexcept { return _registerOp != 0; }
  constexpr bool hasZeroOp() const noexcept { return _zeroOp != 0; }

  constexpr uint32_t registerScalarOp() const noexcept { return (uint32_t(_registerOp) << 10) | (0x5u << 28); }
  constexpr uint32_t registerVectorOp() const noexcept { return uint32_t(_registerOp) << 10; }
  constexpr uint32_t registerScalarHf() const noexcept { return uint32_t(_registerHf); }
  constexpr uint32_t registerVectorHf() const noexcept { return uint32_t(_registerHf); }

  constexpr uint32_t zeroScalarOp() const noexcept { return (uint32_t(_zeroOp) << 10) | (0x5u << 28); }
  constexpr uint32_t zeroVectorOp() const noexcept { return (uint32_t(_zeroOp) << 10); }
};

struct SimdFcmpFcmpe {
  uint32_t _opcode;
  constexpr uint32_t opcode() const noexcept { return _opcode; }
};

struct SimdFcvtLN {
  uint32_t _opcode : 22;
  uint32_t _isCvtxn : 1;
  uint32_t _hasScalar : 1;

  constexpr uint32_t scalarOp() const noexcept { return (uint32_t(_opcode) << 10) | (0x5u << 28); }
  constexpr uint32_t vectorOp() const noexcept { return (uint32_t(_opcode) << 10); }

  constexpr uint32_t isCvtxn() const noexcept { return _isCvtxn; }
  constexpr uint32_t hasScalar() const noexcept { return _hasScalar; }
};

struct SimdFcvtSV {
  uint32_t _vectorIntOp;
  uint32_t _vectorFpOp;
  uint32_t _generalOp : 31;
  uint32_t _isFloatToInt : 1;

  constexpr uint32_t scalarIntOp() const noexcept { return (uint32_t(_vectorIntOp) << 10) | (0x5u << 28); }
  constexpr uint32_t vectorIntOp() const noexcept { return uint32_t(_vectorIntOp) << 10; }
  constexpr uint32_t scalarFpOp() const noexcept { return (uint32_t(_vectorFpOp) << 10) | (0x5u << 28); }
  constexpr uint32_t vectorFpOp() const noexcept { return uint32_t(_vectorFpOp) << 10; }
  constexpr uint32_t generalOp() const noexcept { return (uint32_t(_generalOp) << 10); }

  constexpr uint32_t isFloatToInt() const noexcept { return _isFloatToInt; }
  constexpr uint32_t isFixedPoint() const noexcept { return _vectorFpOp != 0; }
};

struct SimdFmlal {
  uint32_t _vectorOp;
  uint32_t _elementOp;
  uint8_t _optionalQ;
  uint8_t tA;
  uint8_t tB;
  uint8_t tElement;

  constexpr uint32_t vectorOp() const noexcept { return uint32_t(_vectorOp) << 10; }
  constexpr uint32_t elementOp() const noexcept { return uint32_t(_elementOp) << 10; }
  constexpr uint32_t optionalQ() const noexcept { return _optionalQ; }
};

struct FSimdPair {
  uint32_t _scalarOp;
  uint32_t _vectorOp;

  constexpr uint32_t scalarOp() const noexcept { return uint32_t(_scalarOp) << 10; }
  constexpr uint32_t vectorOp() const noexcept { return uint32_t(_vectorOp) << 10; }
};

struct ISimdVV {
  M_OPCODE(opcode, 22)
  uint32_t vecOpType : 6;
};

struct ISimdVVx {
  M_OPCODE(opcode, 22)
  uint32_t op0Signature;
  uint32_t op1Signature;
};

struct ISimdSV {
  M_OPCODE(opcode, 22)
  uint32_t vecOpType : 6;
};

struct ISimdVVV {
  M_OPCODE(opcode, 22)
  uint32_t vecOpType : 6;
};

struct ISimdVVVx {
  M_OPCODE(opcode, 22)
  uint32_t op0Signature;
  uint32_t op1Signature;
  uint32_t op2Signature;
};

struct ISimdWWV {
  M_OPCODE(opcode, 22)
  uint32_t vecOpType : 6;
};

struct ISimdVVVe {
  uint32_t regularOp : 26; // 22 bits used.
  uint32_t regularVecType : 6;
  uint32_t elementOp : 26; // 22 bits used.
  uint32_t elementVecType : 6;
};

struct ISimdVVVI {
  M_OPCODE(opcode, 22)
  uint32_t vecOpType : 6;
  uint32_t immSize : 4;
  uint32_t immShift : 4;
  uint32_t imm64HasOneBitLess : 1;
};

struct ISimdVVVV {
  uint32_t opcode : 22;
  uint32_t vecOpType : 6;
};

struct ISimdVVVVx {
  uint32_t opcode;
  uint32_t op0Signature;
  uint32_t op1Signature;
  uint32_t op2Signature;
  uint32_t op3Signature;
};

struct SimdBicOrr {
  uint32_t registerOp;   // 22 bits used.
  uint32_t immediateOp;  // 22 bits used.
};

struct SimdCmp {
  uint32_t regOp;
  uint32_t zeroOp : 22;
  uint32_t vecOpType : 6;
};

struct SimdDot {
  uint32_t vectorOp;     // 22 bits used.
  uint32_t elementOp;    // 22 bits used.
  uint8_t tA;            // Element-type of the first operand.
  uint8_t tB;            // Element-type of the second and third operands.
  uint8_t tElement;      // Element-type of the element index[] operand.
};

struct SimdMoviMvni {
  uint32_t opcode : 31;
  uint32_t inverted : 1;
};

struct SimdLdSt {
  uint32_t uOffsetOp  : 10;
  uint32_t prePostOp  : 11;
  uint32_t registerOp : 11;
  uint32_t literalOp  : 8;
  uint32_t uAltInstId : 16;
};

struct SimdLdNStN {
  uint32_t singleOp;
  uint32_t multipleOp : 22;
  uint32_t n : 3;
  uint32_t replicate : 1;
};

struct SimdLdpStp {
  uint32_t offsetOp : 10;
  uint32_t prePostOp : 10;
};

struct SimdLdurStur {
  uint32_t opcode;
};

struct ISimdPair {
  uint32_t opcode2;      // 22 bits used.
  uint32_t opcode3 : 26; // 22 bits used.
  uint32_t opType3 : 6;
};

struct SimdShift {
  uint32_t registerOp;       // 22 bits used.
  uint32_t immediateOp : 22; // 22 bits used.
  uint32_t invertedImm : 1;
  uint32_t vecOpType : 6;
};

struct SimdShiftES {
  uint32_t opcode : 22;
  uint32_t vecOpType : 6;
};

struct SimdSm3tt {
  uint32_t opcode;
};

struct SimdSmovUmov {
  uint32_t opcode : 22;
  uint32_t vecOpType : 6;
  uint32_t isSigned : 1;
};

struct SimdSxtlUxtl {
  uint32_t opcode : 22;
  uint32_t vecOpType : 6;
};

struct SimdTblTbx {
  uint32_t opcode;
};

#undef M_OPCODE

// ${EncodingDataForward:Begin}
// ------------------- Automatically generated, do not edit -------------------
extern const BaseAddSub baseAddSub[4];
extern const BaseAdr baseAdr[2];
extern const BaseAtDcIcTlbi baseAtDcIcTlbi[4];
extern const BaseAtomicCasp baseAtomicCasp[4];
extern const BaseAtomicOp baseAtomicOp[123];
extern const BaseAtomicSt baseAtomicSt[48];
extern const BaseBfc baseBfc[1];
extern const BaseBfi baseBfi[3];
extern const BaseBfm baseBfm[3];
extern const BaseBfx baseBfx[3];
extern const BaseBranchCmp baseBranchCmp[2];
extern const BaseBranchReg baseBranchReg[3];
extern const BaseBranchRel baseBranchRel[2];
extern const BaseBranchTst baseBranchTst[2];
extern const BaseCCmp baseCCmp[2];
extern const BaseCInc baseCInc[3];
extern const BaseCSel baseCSel[4];
extern const BaseCSet baseCSet[2];
extern const BaseCmpCmn baseCmpCmn[2];
extern const BaseExtend baseExtend[5];
extern const BaseExtract baseExtract[1];
extern const BaseLdSt baseLdSt[9];
extern const BaseLdpStp baseLdpStp[6];
extern const BaseLdxp baseLdxp[2];
extern const BaseLogical baseLogical[8];
extern const BaseMovKNZ baseMovKNZ[3];
extern const BaseMvnNeg baseMvnNeg[3];
extern const BaseOp baseOp[23];
extern const BaseOpImm baseOpImm[14];
extern const BaseR baseR[10];
extern const BaseRM_NoImm baseRM_NoImm[21];
extern const BaseRM_SImm10 baseRM_SImm10[2];
extern const BaseRM_SImm9 baseRM_SImm9[23];
extern const BaseRR baseRR[15];
extern const BaseRRII baseRRII[2];
extern const BaseRRR baseRRR[26];
extern const BaseRRRR baseRRRR[6];
extern const BaseShift baseShift[8];
extern const BaseStx baseStx[3];
extern const BaseStxp baseStxp[2];
extern const BaseTst baseTst[1];
extern const FSimdPair fSimdPair[5];
extern const FSimdSV fSimdSV[4];
extern const FSimdVV fSimdVV[17];
extern const FSimdVVV fSimdVVV[13];
extern const FSimdVVVV fSimdVVVV[4];
extern const FSimdVVVe fSimdVVVe[4];
extern const ISimdPair iSimdPair[1];
extern const ISimdSV iSimdSV[7];
extern const ISimdVV iSimdVV[29];
extern const ISimdVVV iSimdVVV[65];
extern const ISimdVVVI iSimdVVVI[2];
extern const ISimdVVVV iSimdVVVV[2];
extern const ISimdVVVVx iSimdVVVVx[1];
extern const ISimdVVVe iSimdVVVe[25];
extern const ISimdVVVx iSimdVVVx[17];
extern const ISimdVVx iSimdVVx[13];
extern const ISimdWWV iSimdWWV[8];
extern const SimdBicOrr simdBicOrr[2];
extern const SimdCmp simdCmp[7];
extern const SimdDot simdDot[5];
extern const SimdFcadd simdFcadd[1];
extern const SimdFccmpFccmpe simdFccmpFccmpe[2];
extern const SimdFcm simdFcm[5];
extern const SimdFcmla simdFcmla[1];
extern const SimdFcmpFcmpe simdFcmpFcmpe[2];
extern const SimdFcvtLN simdFcvtLN[6];
extern const SimdFcvtSV simdFcvtSV[12];
extern const SimdFmlal simdFmlal[6];
extern const SimdLdNStN simdLdNStN[12];
extern const SimdLdSt simdLdSt[2];
extern const SimdLdpStp simdLdpStp[4];
extern const SimdLdurStur simdLdurStur[2];
extern const SimdMoviMvni simdMoviMvni[2];
extern const SimdShift simdShift[40];
extern const SimdShiftES simdShiftES[2];
extern const SimdSm3tt simdSm3tt[4];
extern const SimdSmovUmov simdSmovUmov[2];
extern const SimdSxtlUxtl simdSxtlUxtl[4];
extern const SimdTblTbx simdTblTbx[2];
// ----------------------------------------------------------------------------
// ${EncodingDataForward:End}

} // {EncodingData}

// a64::InstDB - InstNameIndex
// ===========================

// ${NameLimits:Begin}
// ------------------- Automatically generated, do not edit -------------------
enum : uint32_t { kMaxNameSize = 9 };
// ----------------------------------------------------------------------------
// ${NameLimits:End}

struct InstNameIndex {
  uint16_t start;
  uint16_t end;
};

// a64::InstDB - Tables
// ====================

#ifndef ASMJIT_NO_TEXT
extern const uint32_t _instNameIndexTable[];
extern const char _instNameStringTable[];
extern const InstNameIndex instNameIndex[26];
#endif // !ASMJIT_NO_TEXT

} // {InstDB}

//! \}
//! \endcond

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_A64_ARMINSTDB_H_P_INCLUDED

