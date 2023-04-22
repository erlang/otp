// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_X86_X86INSTDB_H_INCLUDED
#define ASMJIT_X86_X86INSTDB_H_INCLUDED

#include "../x86/x86globals.h"

ASMJIT_BEGIN_SUB_NAMESPACE(x86)

//! \addtogroup asmjit_x86
//! \{

//! Instruction database (X86).
namespace InstDB {

//! Describes which operation mode is supported by an instruction.
enum class Mode : uint8_t {
  //! Invalid mode.
  kNone = 0x00u,
  //! X86 mode supported.
  kX86 = 0x01u,
  //! X64 mode supported.
  kX64 = 0x02u,
  //! Both X86 and X64 modes supported.
  kAny = 0x03u
};
ASMJIT_DEFINE_ENUM_FLAGS(Mode)

//! Converts architecture to operation mode, see \ref Mode.
static constexpr Mode modeFromArch(Arch arch) noexcept {
  return arch == Arch::kX86 ? Mode::kX86 :
         arch == Arch::kX64 ? Mode::kX64 : Mode::kNone;
}

//! Operand signature flags used by \ref OpSignature.
enum class OpFlags : uint64_t {
  //! No operand flags.
  kNone = 0u,

  kRegGpbLo        = 0x0000000000000001u, //!< Operand can be low 8-bit GPB register.
  kRegGpbHi        = 0x0000000000000002u, //!< Operand can be high 8-bit GPB register.
  kRegGpw          = 0x0000000000000004u, //!< Operand can be 16-bit GPW register.
  kRegGpd          = 0x0000000000000008u, //!< Operand can be 32-bit GPD register.
  kRegGpq          = 0x0000000000000010u, //!< Operand can be 64-bit GPQ register.
  kRegXmm          = 0x0000000000000020u, //!< Operand can be 128-bit XMM register.
  kRegYmm          = 0x0000000000000040u, //!< Operand can be 256-bit YMM register.
  kRegZmm          = 0x0000000000000080u, //!< Operand can be 512-bit ZMM register.
  kRegMm           = 0x0000000000000100u, //!< Operand can be 64-bit MM register.
  kRegKReg         = 0x0000000000000200u, //!< Operand can be 64-bit K register.
  kRegSReg         = 0x0000000000000400u, //!< Operand can be SReg (segment register).
  kRegCReg         = 0x0000000000000800u, //!< Operand can be CReg (control register).
  kRegDReg         = 0x0000000000001000u, //!< Operand can be DReg (debug register).
  kRegSt           = 0x0000000000002000u, //!< Operand can be 80-bit ST register (X87).
  kRegBnd          = 0x0000000000004000u, //!< Operand can be 128-bit BND register.
  kRegTmm          = 0x0000000000008000u, //!< Operand can be 0..8192-bit TMM register.
  kRegMask         = 0x000000000000FFFFu, //!< Mask of all possible register types.

  kMemUnspecified  = 0x0000000000040000u, //!< Operand can be a scalar memory pointer without size.
  kMem8            = 0x0000000000080000u, //!< Operand can be an 8-bit memory pointer.
  kMem16           = 0x0000000000100000u, //!< Operand can be a 16-bit memory pointer.
  kMem32           = 0x0000000000200000u, //!< Operand can be a 32-bit memory pointer.
  kMem48           = 0x0000000000400000u, //!< Operand can be a 48-bit memory pointer (FAR pointers only).
  kMem64           = 0x0000000000800000u, //!< Operand can be a 64-bit memory pointer.
  kMem80           = 0x0000000001000000u, //!< Operand can be an 80-bit memory pointer.
  kMem128          = 0x0000000002000000u, //!< Operand can be a 128-bit memory pointer.
  kMem256          = 0x0000000004000000u, //!< Operand can be a 256-bit memory pointer.
  kMem512          = 0x0000000008000000u, //!< Operand can be a 512-bit memory pointer.
  kMem1024         = 0x0000000010000000u, //!< Operand can be a 1024-bit memory pointer.
  kMemMask         = 0x000000001FFC0000u, //!< Mask of all possible scalar memory types.

  kVm32x           = 0x0000000040000000u, //!< Operand can be a vm32x (vector) pointer.
  kVm32y           = 0x0000000080000000u, //!< Operand can be a vm32y (vector) pointer.
  kVm32z           = 0x0000000100000000u, //!< Operand can be a vm32z (vector) pointer.
  kVm64x           = 0x0000000200000000u, //!< Operand can be a vm64x (vector) pointer.
  kVm64y           = 0x0000000400000000u, //!< Operand can be a vm64y (vector) pointer.
  kVm64z           = 0x0000000800000000u, //!< Operand can be a vm64z (vector) pointer.
  kVmMask          = 0x0000000FC0000000u, //!< Mask of all possible vector memory types.

  kImmI4           = 0x0000001000000000u, //!< Operand can be signed 4-bit immediate.
  kImmU4           = 0x0000002000000000u, //!< Operand can be unsigned 4-bit immediate.
  kImmI8           = 0x0000004000000000u, //!< Operand can be signed 8-bit immediate.
  kImmU8           = 0x0000008000000000u, //!< Operand can be unsigned 8-bit immediate.
  kImmI16          = 0x0000010000000000u, //!< Operand can be signed 16-bit immediate.
  kImmU16          = 0x0000020000000000u, //!< Operand can be unsigned 16-bit immediate.
  kImmI32          = 0x0000040000000000u, //!< Operand can be signed 32-bit immediate.
  kImmU32          = 0x0000080000000000u, //!< Operand can be unsigned 32-bit immediate.
  kImmI64          = 0x0000100000000000u, //!< Operand can be signed 64-bit immediate.
  kImmU64          = 0x0000200000000000u, //!< Operand can be unsigned 64-bit immediate.
  kImmMask         = 0x00003FF000000000u, //!< Mask of all immediate types.

  kRel8            = 0x0000400000000000u, //!< Operand can be relative 8-bit  displacement.
  kRel32           = 0x0000800000000000u, //!< Operand can be relative 32-bit displacement.
  kRelMask         = 0x0000C00000000000u, //!< Mask of all relative displacement types.

  kFlagMemBase     = 0x0001000000000000u, //!< Flag: Only memory base is allowed (no index, no offset).
  kFlagMemDs       = 0x0002000000000000u, //!< Flag: Implicit memory operand's DS segment.
  kFlagMemEs       = 0x0004000000000000u, //!< Flag: Implicit memory operand's ES segment.

  kFlagMib         = 0x0008000000000000u, //!< Flag: Operand is MIB (base+index) pointer.
  kFlagTMem        = 0x0010000000000000u, //!< Flag: Operand is TMEM (sib_mem), AMX memory pointer.

  kFlagImplicit    = 0x0080000000000000u, //!< Flag: Operand is implicit.
  kFlagMask        = 0x009F000000000000u, //!< Mask of all flags.

  //! Contains mask of all registers, memory operands, immediate operands, and displacement operands.
  kOpMask          = kRegMask | kMemMask | kVmMask | kImmMask | kRelMask
};
ASMJIT_DEFINE_ENUM_FLAGS(OpFlags)

//! Operand signature.
//!
//! Contains all possible operand combinations, memory size information, and a fixed register id (or `BaseReg::kIdBad`
//! if fixed id isn't required).
struct OpSignature {
  //! \name Members
  //! \{

  uint64_t _flags : 56;
  uint64_t _regMask : 8;

  //! \}

  //! \name Accessors
  //! \{

  //! Returns operand signature flags.
  inline OpFlags flags() const noexcept { return (OpFlags)_flags; }

  //! Tests whether the given `flag` is set.
  inline bool hasFlag(OpFlags flag) const noexcept { return (_flags & uint64_t(flag)) != 0; }

  //! Tests whether this signature contains at least one register operand of any type.
  inline bool hasReg() const noexcept { return hasFlag(OpFlags::kRegMask); }
  //! Tests whether this signature contains at least one scalar memory operand of any type.
  inline bool hasMem() const noexcept { return hasFlag(OpFlags::kMemMask); }
  //! Tests whether this signature contains at least one vector memory operand of any type.
  inline bool hasVm() const noexcept { return hasFlag(OpFlags::kVmMask); }
  //! Tests whether this signature contains at least one immediate operand of any type.
  inline bool hasImm() const noexcept { return hasFlag(OpFlags::kImmMask); }
  //! Tests whether this signature contains at least one relative displacement operand of any type.
  inline bool hasRel() const noexcept { return hasFlag(OpFlags::kRelMask); }

  //! Tests whether the operand is implicit.
  inline bool isImplicit() const noexcept { return hasFlag(OpFlags::kFlagImplicit); }

  //! Returns a physical register mask.
  inline RegMask regMask() const noexcept { return _regMask; }

  //! \}
};

ASMJIT_VARAPI const OpSignature _opSignatureTable[];

//! Instruction signature.
//!
//! Contains a sequence of operands' combinations and other metadata that defines a single instruction. This data is
//! used by instruction validator.
struct InstSignature {
  //! \name Members
  //! \{

  //! Count of operands in `opIndex` (0..6).
  uint8_t _opCount : 3;
  //! Architecture modes supported (X86 / X64).
  uint8_t _mode : 2;
  //! Number of implicit operands.
  uint8_t _implicitOpCount : 3;
  //! Reserved for future use.
  uint8_t _reserved;
  //! Indexes to `OpSignature` table.
  uint8_t _opSignatureIndexes[Globals::kMaxOpCount];

  //! \}

  //! \name Accessors
  //! \{

  //! Returns instruction operation mode.
  inline Mode mode() const noexcept { return (Mode)_mode; }
  //! Tests whether the instruction supports the given operating mode.
  inline bool supportsMode(Mode mode) const noexcept { return (uint8_t(_mode) & uint8_t(mode)) != 0; }

  //! Returns the number of operands of this signature.
  inline uint32_t opCount() const noexcept { return _opCount; }
  //! Returns the number of implicit operands this signature has.
  inline uint32_t implicitOpCount() const noexcept { return _implicitOpCount; }
  //! Tests whether this instruction signature has at least one implicit operand.
  inline bool hasImplicitOperands() const noexcept { return _implicitOpCount != 0; }

  //! Returns indexes to \ref _opSignatureTable for each operand of the instruction.
  //!
  //! \note The returned array always provides indexes for all operands (see \ref Globals::kMaxOpCount) even if the
  //! instruction provides less operands. Undefined operands have always index of zero.
  inline const uint8_t* opSignatureIndexes() const noexcept { return _opSignatureIndexes; }

  //! Returns index to \ref _opSignatureTable, corresponding to the requested operand `index` of the instruction.
  inline uint8_t opSignatureIndex(size_t index) const noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxOpCount);
    return _opSignatureIndexes[index];
  }

  //! Returns \ref OpSignature corresponding to the requested operand `index` of the instruction.
  inline const OpSignature& opSignature(size_t index) const noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxOpCount);
    return _opSignatureTable[_opSignatureIndexes[index]];
  }

  //! \}
};

ASMJIT_VARAPI const InstSignature _instSignatureTable[];

//! Instruction flags.
//!
//! Details about instruction encoding, operation, features, and some limitations.
enum class InstFlags : uint32_t {
  //! No flags.
  kNone = 0x00000000u,

  // Instruction Family
  // ------------------
  //
  // Instruction family information.

  //! Instruction that accesses FPU registers.
  kFpu = 0x00000100u,
  //! Instruction that accesses MMX registers (including 3DNOW and GEODE) and EMMS.
  kMmx = 0x00000200u,
  //! Instruction that accesses XMM registers (SSE, AVX, AVX512).
  kVec = 0x00000400u,

  // FPU Flags
  // ---------
  //
  // Used to tell the encoder which memory operand sizes are encodable.

  //! FPU instruction can address `word_ptr` (shared with M80).
  kFpuM16 = 0x00000800u,
  //! FPU instruction can address `dword_ptr`.
  kFpuM32 = 0x00001000u,
  //! FPU instruction can address `qword_ptr`.
  kFpuM64 = 0x00002000u,
  //! FPU instruction can address `tword_ptr` (shared with M16).
  kFpuM80 = 0x00000800u,

  // Prefixes and Encoding Flags
  // ---------------------------
  //
  // These describe optional X86 prefixes that can be used to change the instruction's operation.

  //! Instruction can be prefixed with using the REP(REPE) or REPNE prefix.
  kRep = 0x00004000u,
  //! Rep prefix is accepted, but it has no effect other than being emitted with the instruction (as an extra byte).
  kRepIgnored = 0x00008000u,
  //! Instruction can be prefixed with using the LOCK prefix.
  kLock = 0x00010000u,
  //! Instruction can be prefixed with using the XACQUIRE prefix.
  kXAcquire = 0x00020000u,
  //! Instruction can be prefixed with using the XRELEASE prefix.
  kXRelease = 0x00040000u,
  //! Instruction uses MIB (BNDLDX|BNDSTX) to encode two registers.
  kMib = 0x00080000u,
  //! Instruction uses VSIB instead of legacy SIB.
  kVsib = 0x00100000u,
  //! Instruction uses TSIB (or SIB_MEM) encoding (MODRM followed by SIB).
  kTsib = 0x00200000u,

  // If both `kPrefixVex` and `kPrefixEvex` flags are specified it means that the instructions can be encoded
  // by either VEX or EVEX prefix. In that case AsmJit checks global options and also instruction options to decide
  // whether to emit VEX or EVEX prefix.

  //! Instruction can be encoded by VEX|XOP (AVX|AVX2|BMI|XOP|...).
  kVex = 0x00400000u,
  //! Instruction can be encoded by EVEX (AVX512).
  kEvex = 0x00800000u,
  //! EVEX encoding is preferred over VEX encoding (AVX515_VNNI vs AVX_VNNI).
  kPreferEvex = 0x01000000u,
  //! EVEX and VEX signatures are compatible.
  kEvexCompat = 0x02000000u,
  //! EVEX instruction requires K register in the first operand (compare instructions).
  kEvexKReg = 0x04000000u,
  //! EVEX instruction requires two operands and K register as a selector (gather instructions).
  kEvexTwoOp = 0x08000000u,
  //! VEX instruction that can be transformed to a compatible EVEX instruction.
  kEvexTransformable = 0x10000000u,

  // Other Flags
  // -----------

  //! Instruction uses consecutive registers.
  //!
  //! Used by V4FMADDPS, V4FMADDSS, V4FNMADDPS, V4FNMADDSS, VP4DPWSSD, VP4DPWSSDS, VP2INTERSECTD, and VP2INTERSECTQ
  //! instructions
  kConsecutiveRegs = 0x20000000u
};
ASMJIT_DEFINE_ENUM_FLAGS(InstFlags)

//! AVX-512 flags.
enum class Avx512Flags : uint32_t {
  //! No AVX-512 flags.
  kNone = 0,

  //! Internally used in tables, has no meaning.
  k_ = 0x00000000u,
  //! Supports masking {k1..k7}.
  kK = 0x00000001u,
  //! Supports zeroing {z}, must be used together with `kAvx512k`.
  kZ = 0x00000002u,
  //! Supports 'embedded-rounding' {er} with implicit {sae},
  kER = 0x00000004u,
  //! Supports 'suppress-all-exceptions' {sae}.
  kSAE = 0x00000008u,
  //! Supports 16-bit broadcast 'b16'.
  kB16 = 0x00000010u,
  //! Supports 32-bit broadcast 'b32'.
  kB32 = 0x00000020u,
  //! Supports 64-bit broadcast 'b64'.
  kB64 = 0x00000040u,
  //! Operates on a vector of consecutive registers (AVX512_4FMAPS and AVX512_4VNNIW).
  kT4X = 0x00000080u,

  //! Implicit zeroing if {k} masking is used. Using {z} is not valid in this case as it's implicit.
  kImplicitZ = 0x00000100,
};
ASMJIT_DEFINE_ENUM_FLAGS(Avx512Flags)

//! Instruction common information.
//!
//! Aggregated information shared across one or more instruction.
struct CommonInfo {
  //! Instruction flags.
  uint32_t _flags;
  //! Reserved for future use.
  uint32_t _avx512Flags : 11;
  //! First `InstSignature` entry in the database.
  uint32_t _iSignatureIndex : 11;
  //! Number of relevant `ISignature` entries.
  uint32_t _iSignatureCount : 5;
  //! Instruction control flow category, see \ref InstControlFlow.
  uint32_t _controlFlow : 3;
  //! Specifies what happens if all source operands share the same register.
  uint32_t _sameRegHint : 2;

  //! \name Accessors
  //! \{

  //! Returns instruction flags.
  inline InstFlags flags() const noexcept { return (InstFlags)_flags; }
  //! Tests whether the instruction has a `flag`.
  inline bool hasFlag(InstFlags flag) const noexcept { return Support::test(_flags, flag); }

  //! Returns instruction AVX-512 flags.
  inline Avx512Flags avx512Flags() const noexcept { return (Avx512Flags)_avx512Flags; }
  //! Tests whether the instruction has an AVX-512 `flag`.
  inline bool hasAvx512Flag(Avx512Flags flag) const noexcept { return Support::test(_avx512Flags, flag); }

  //! Tests whether the instruction is FPU instruction.
  inline bool isFpu() const noexcept { return hasFlag(InstFlags::kFpu); }
  //! Tests whether the instruction is MMX/3DNOW instruction that accesses MMX registers (includes EMMS and FEMMS).
  inline bool isMmx() const noexcept { return hasFlag(InstFlags::kMmx); }
  //! Tests whether the instruction is SSE|AVX|AVX512 instruction that accesses XMM|YMM|ZMM registers.
  inline bool isVec() const noexcept { return hasFlag(InstFlags::kVec); }
  //! Tests whether the instruction is SSE+ (SSE4.2, AES, SHA included) instruction that accesses XMM registers.
  inline bool isSse() const noexcept { return (flags() & (InstFlags::kVec | InstFlags::kVex | InstFlags::kEvex)) == InstFlags::kVec; }
  //! Tests whether the instruction is AVX+ (FMA included) instruction that accesses XMM|YMM|ZMM registers.
  inline bool isAvx() const noexcept { return isVec() && isVexOrEvex(); }

  //! Tests whether the instruction can be prefixed with LOCK prefix.
  inline bool hasLockPrefix() const noexcept { return hasFlag(InstFlags::kLock); }
  //! Tests whether the instruction can be prefixed with REP (REPE|REPZ) prefix.
  inline bool hasRepPrefix() const noexcept { return hasFlag(InstFlags::kRep); }
  //! Tests whether the instruction can be prefixed with XACQUIRE prefix.
  inline bool hasXAcquirePrefix() const noexcept { return hasFlag(InstFlags::kXAcquire); }
  //! Tests whether the instruction can be prefixed with XRELEASE prefix.
  inline bool hasXReleasePrefix() const noexcept { return hasFlag(InstFlags::kXRelease); }

  //! Tests whether the rep prefix is supported by the instruction, but ignored (has no effect).
  inline bool isRepIgnored() const noexcept { return hasFlag(InstFlags::kRepIgnored); }
  //! Tests whether the instruction uses MIB.
  inline bool isMibOp() const noexcept { return hasFlag(InstFlags::kMib); }
  //! Tests whether the instruction uses VSIB.
  inline bool isVsibOp() const noexcept { return hasFlag(InstFlags::kVsib); }
  //! Tests whether the instruction uses TSIB (AMX, instruction requires MOD+SIB).
  inline bool isTsibOp() const noexcept { return hasFlag(InstFlags::kTsib); }
  //! Tests whether the instruction uses VEX (can be set together with EVEX if both are encodable).
  inline bool isVex() const noexcept { return hasFlag(InstFlags::kVex); }
  //! Tests whether the instruction uses EVEX (can be set together with VEX if both are encodable).
  inline bool isEvex() const noexcept { return hasFlag(InstFlags::kEvex); }
  //! Tests whether the instruction uses EVEX (can be set together with VEX if both are encodable).
  inline bool isVexOrEvex() const noexcept { return hasFlag(InstFlags::kVex | InstFlags::kEvex); }

  //! Tests whether the instruction should prefer EVEX prefix instead of VEX prefix.
  inline bool preferEvex() const noexcept { return hasFlag(InstFlags::kPreferEvex); }

  inline bool isEvexCompatible() const noexcept { return hasFlag(InstFlags::kEvexCompat); }
  inline bool isEvexKRegOnly() const noexcept { return hasFlag(InstFlags::kEvexKReg); }
  inline bool isEvexTwoOpOnly() const noexcept { return hasFlag(InstFlags::kEvexTwoOp); }
  inline bool isEvexTransformable() const noexcept { return hasFlag(InstFlags::kEvexTransformable); }

  //! Tests whether the instruction supports AVX512 masking {k}.
  inline bool hasAvx512K() const noexcept { return hasAvx512Flag(Avx512Flags::kK); }
  //! Tests whether the instruction supports AVX512 zeroing {k}{z}.
  inline bool hasAvx512Z() const noexcept { return hasAvx512Flag(Avx512Flags::kZ); }
  //! Tests whether the instruction supports AVX512 embedded-rounding {er}.
  inline bool hasAvx512ER() const noexcept { return hasAvx512Flag(Avx512Flags::kER); }
  //! Tests whether the instruction supports AVX512 suppress-all-exceptions {sae}.
  inline bool hasAvx512SAE() const noexcept { return hasAvx512Flag(Avx512Flags::kSAE); }
  //! Tests whether the instruction supports AVX512 broadcast (either 32-bit or 64-bit).
  inline bool hasAvx512B() const noexcept { return hasAvx512Flag(Avx512Flags::kB16 | Avx512Flags::kB32 | Avx512Flags::kB64); }
  //! Tests whether the instruction supports AVX512 broadcast (16-bit).
  inline bool hasAvx512B16() const noexcept { return hasAvx512Flag(Avx512Flags::kB16); }
  //! Tests whether the instruction supports AVX512 broadcast (32-bit).
  inline bool hasAvx512B32() const noexcept { return hasAvx512Flag(Avx512Flags::kB32); }
  //! Tests whether the instruction supports AVX512 broadcast (64-bit).
  inline bool hasAvx512B64() const noexcept { return hasAvx512Flag(Avx512Flags::kB64); }

  // Returns the size of the broadcast - either 2, 4, or 8, or 0 if broadcast is not supported.
  inline uint32_t broadcastSize() const noexcept {
    constexpr uint32_t kShift = Support::ConstCTZ<uint32_t(Avx512Flags::kB16)>::value;
    return (uint32_t(_avx512Flags) & uint32_t(Avx512Flags::kB16 | Avx512Flags::kB32 | Avx512Flags::kB64)) >> (kShift - 1);
  }

  inline uint32_t signatureIndex() const noexcept { return _iSignatureIndex; }
  inline uint32_t signatureCount() const noexcept { return _iSignatureCount; }

  inline const InstSignature* signatureData() const noexcept { return _instSignatureTable + _iSignatureIndex; }
  inline const InstSignature* signatureEnd() const noexcept { return _instSignatureTable + _iSignatureIndex + _iSignatureCount; }

  //! Returns a control flow category of the instruction.
  inline InstControlFlow controlFlow() const noexcept { return (InstControlFlow)_controlFlow; }

  //! Returns a hint that can be used when both inputs are the same register.
  inline InstSameRegHint sameRegHint() const noexcept { return (InstSameRegHint)_sameRegHint; }

  //! \}
};

ASMJIT_VARAPI const CommonInfo _commonInfoTable[];

//! Instruction information.
struct InstInfo {
  //! Reserved for future use.
  uint32_t _reserved : 14;
  //! Index to \ref _commonInfoTable.
  uint32_t _commonInfoIndex : 10;
  //! Index to \ref _additionalInfoTable.
  uint32_t _additionalInfoIndex : 8;

  //! Instruction encoding (internal encoding identifier used by \ref Assembler).
  uint8_t _encoding;
  //! Main opcode value (0..255).
  uint8_t _mainOpcodeValue;
  //! Index to \ref _mainOpcodeTable` that is combined with \ref _mainOpcodeValue to form the final opcode.
  uint8_t _mainOpcodeIndex;
  //! Index to \ref _altOpcodeTable that contains a full alternative opcode.
  uint8_t _altOpcodeIndex;

  //! \name Accessors
  //! \{

  //! Returns common information, see \ref CommonInfo.
  inline const CommonInfo& commonInfo() const noexcept { return _commonInfoTable[_commonInfoIndex]; }

  //! Returns instruction flags, see \ref Flags.
  inline InstFlags flags() const noexcept { return commonInfo().flags(); }
  //! Tests whether the instruction has flag `flag`, see \ref Flags.
  inline bool hasFlag(InstFlags flag) const noexcept { return commonInfo().hasFlag(flag); }

  //! Returns instruction AVX-512 flags, see \ref Avx512Flags.
  inline Avx512Flags avx512Flags() const noexcept { return commonInfo().avx512Flags(); }
  //! Tests whether the instruction has an AVX-512 `flag`, see \ref Avx512Flags.
  inline bool hasAvx512Flag(Avx512Flags flag) const noexcept { return commonInfo().hasAvx512Flag(flag); }

  //! Tests whether the instruction is FPU instruction.
  inline bool isFpu() const noexcept { return commonInfo().isFpu(); }
  //! Tests whether the instruction is MMX/3DNOW instruction that accesses MMX registers (includes EMMS and FEMMS).
  inline bool isMmx() const noexcept { return commonInfo().isMmx(); }
  //! Tests whether the instruction is SSE|AVX|AVX512 instruction that accesses XMM|YMM|ZMM registers.
  inline bool isVec() const noexcept { return commonInfo().isVec(); }
  //! Tests whether the instruction is SSE+ (SSE4.2, AES, SHA included) instruction that accesses XMM registers.
  inline bool isSse() const noexcept { return commonInfo().isSse(); }
  //! Tests whether the instruction is AVX+ (FMA included) instruction that accesses XMM|YMM|ZMM registers.
  inline bool isAvx() const noexcept { return commonInfo().isAvx(); }

  //! Tests whether the instruction can be prefixed with LOCK prefix.
  inline bool hasLockPrefix() const noexcept { return commonInfo().hasLockPrefix(); }
  //! Tests whether the instruction can be prefixed with REP (REPE|REPZ) prefix.
  inline bool hasRepPrefix() const noexcept { return commonInfo().hasRepPrefix(); }
  //! Tests whether the instruction can be prefixed with XACQUIRE prefix.
  inline bool hasXAcquirePrefix() const noexcept { return commonInfo().hasXAcquirePrefix(); }
  //! Tests whether the instruction can be prefixed with XRELEASE prefix.
  inline bool hasXReleasePrefix() const noexcept { return commonInfo().hasXReleasePrefix(); }

  //! Tests whether the rep prefix is supported by the instruction, but ignored (has no effect).
  inline bool isRepIgnored() const noexcept { return commonInfo().isRepIgnored(); }
  //! Tests whether the instruction uses MIB.
  inline bool isMibOp() const noexcept { return hasFlag(InstFlags::kMib); }
  //! Tests whether the instruction uses VSIB.
  inline bool isVsibOp() const noexcept { return hasFlag(InstFlags::kVsib); }
  //! Tests whether the instruction uses VEX (can be set together with EVEX if both are encodable).
  inline bool isVex() const noexcept { return hasFlag(InstFlags::kVex); }
  //! Tests whether the instruction uses EVEX (can be set together with VEX if both are encodable).
  inline bool isEvex() const noexcept { return hasFlag(InstFlags::kEvex); }
  //! Tests whether the instruction uses EVEX (can be set together with VEX if both are encodable).
  inline bool isVexOrEvex() const noexcept { return hasFlag(InstFlags::kVex | InstFlags::kEvex); }

  inline bool isEvexCompatible() const noexcept { return hasFlag(InstFlags::kEvexCompat); }
  inline bool isEvexKRegOnly() const noexcept { return hasFlag(InstFlags::kEvexKReg); }
  inline bool isEvexTwoOpOnly() const noexcept { return hasFlag(InstFlags::kEvexTwoOp); }
  inline bool isEvexTransformable() const noexcept { return hasFlag(InstFlags::kEvexTransformable); }

  //! Tests whether the instruction supports AVX512 masking {k}.
  inline bool hasAvx512K() const noexcept { return hasAvx512Flag(Avx512Flags::kK); }
  //! Tests whether the instruction supports AVX512 zeroing {k}{z}.
  inline bool hasAvx512Z() const noexcept { return hasAvx512Flag(Avx512Flags::kZ); }
  //! Tests whether the instruction supports AVX512 embedded-rounding {er}.
  inline bool hasAvx512ER() const noexcept { return hasAvx512Flag(Avx512Flags::kER); }
  //! Tests whether the instruction supports AVX512 suppress-all-exceptions {sae}.
  inline bool hasAvx512SAE() const noexcept { return hasAvx512Flag(Avx512Flags::kSAE); }
  //! Tests whether the instruction supports AVX512 broadcast (either 32-bit or 64-bit).
  inline bool hasAvx512B() const noexcept { return hasAvx512Flag(Avx512Flags::kB16 | Avx512Flags::kB32 | Avx512Flags::kB64); }
  //! Tests whether the instruction supports AVX512 broadcast (16-bit).
  inline bool hasAvx512B16() const noexcept { return hasAvx512Flag(Avx512Flags::kB16); }
  //! Tests whether the instruction supports AVX512 broadcast (32-bit).
  inline bool hasAvx512B32() const noexcept { return hasAvx512Flag(Avx512Flags::kB32); }
  //! Tests whether the instruction supports AVX512 broadcast (64-bit).
  inline bool hasAvx512B64() const noexcept { return hasAvx512Flag(Avx512Flags::kB64); }

  //! Returns a control flow category of the instruction.
  inline InstControlFlow controlFlow() const noexcept { return commonInfo().controlFlow(); }
  //! Returns a hint that can be used when both inputs are the same register.
  inline InstSameRegHint sameRegHint() const noexcept { return commonInfo().sameRegHint(); }

  inline uint32_t signatureIndex() const noexcept { return commonInfo().signatureIndex(); }
  inline uint32_t signatureCount() const noexcept { return commonInfo().signatureCount(); }

  inline const InstSignature* signatureData() const noexcept { return commonInfo().signatureData(); }
  inline const InstSignature* signatureEnd() const noexcept { return commonInfo().signatureEnd(); }

  //! \}
};

ASMJIT_VARAPI const InstInfo _instInfoTable[];

static inline const InstInfo& infoById(InstId instId) noexcept {
  ASMJIT_ASSERT(Inst::isDefinedId(instId));
  return _instInfoTable[instId];
}

//! \cond INTERNAL
static_assert(sizeof(OpSignature) == 8, "InstDB::OpSignature must be 8 bytes long");
//! \endcond

} // {InstDB}

//! \}

ASMJIT_END_SUB_NAMESPACE

#endif // ASMJIT_X86_X86INSTDB_H_INCLUDED
