// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_INST_H_INCLUDED
#define ASMJIT_CORE_INST_H_INCLUDED

#include "../core/cpuinfo.h"
#include "../core/operand.h"
#include "../core/string.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_instruction_db
//! \{

//! Describes an instruction id and modifiers used together with the id.
//!
//! Each architecture has a set of valid instructions indexed from 0. Instruction with 0 id is, however, a special
//! instruction that describes a "no instruction" or "invalid instruction". Different architectures can assign a.
//! different instruction to the same id, each architecture typicall has its own instructions indexed from 1.
//!
//! Instruction identifiers listed by architecture:
//!
//!   - \ref x86::Inst (X86 and X86_64)
//!   - \ref a64::Inst (AArch64)
typedef uint32_t InstId;

//! Instruction id parts.
//!
//! A mask that specifies a bit-layout of \ref InstId.
enum class InstIdParts : uint32_t {
  // Common Masks
  // ------------

  //! Real id without any modifiers (always 16 least significant bits).
  kRealId   = 0x0000FFFFu,
  //! Instruction is abstract (or virtual, IR, etc...).
  kAbstract = 0x80000000u,

  // ARM Specific
  // ------------

  //! AArch32 first data type, used by ASIMD instructions (`inst.dt.dt2`).
  kA32_DT   = 0x000F0000u,
  //! AArch32 second data type, used by ASIMD instructions (`inst.dt.dt2`).
  kA32_DT2  = 0x00F00000u,
  //! AArch32/AArch64 condition code.
  kARM_Cond = 0x78000000u
};

//! Instruction options.
//!
//! Instruction options complement instruction identifier and attributes.
enum class InstOptions : uint32_t {
  //! No options.
  kNone = 0,

  //! Used internally by emitters for handling errors and rare cases.
  kReserved = 0x00000001u,

  //! Prevents following a jump during compilation (Compiler).
  kUnfollow = 0x00000002u,

  //! Overwrite the destination operand(s) (Compiler).
  //!
  //! Hint that is important for register liveness analysis. It tells the compiler that the destination operand will
  //! be overwritten now or by adjacent instructions. Compiler knows when a register is completely overwritten by a
  //! single instruction, for example you don't have to mark "movaps" or "pxor x, x", however, if a pair of
  //! instructions is used and the first of them doesn't completely overwrite the content of the destination,
  //! Compiler fails to mark that register as dead.
  //!
  //! X86 Specific
  //! ------------
  //!
  //!   - All instructions that always overwrite at least the size of the register the virtual-register uses, for
  //!     example "mov", "movq", "movaps" don't need the overwrite option to be used - conversion, shuffle, and
  //!     other miscellaneous instructions included.
  //!
  //!   - All instructions that clear the destination register if all operands are the same, for example "xor x, x",
  //!     "pcmpeqb x x", etc...
  //!
  //!   - Consecutive instructions that partially overwrite the variable until there is no old content require
  //!     `BaseCompiler::overwrite()` to be used. Some examples (not always the best use cases thought):
  //!
  //!     - `movlps xmm0, ?` followed by `movhps xmm0, ?` and vice versa
  //!     - `movlpd xmm0, ?` followed by `movhpd xmm0, ?` and vice versa
  //!     - `mov al, ?` followed by `and ax, 0xFF`
  //!     - `mov al, ?` followed by `mov ah, al`
  //!     - `pinsrq xmm0, ?, 0` followed by `pinsrq xmm0, ?, 1`
  //!
  //!   - If the allocated virtual register is used temporarily for scalar operations. For example if you allocate a
  //!     full vector like `x86::Compiler::newXmm()` and then use that vector for scalar operations you should use
  //!     `overwrite()` directive:
  //!
  //!     - `sqrtss x, y` - only LO element of `x` is changed, if you don't
  //!       use HI elements, use `compiler.overwrite().sqrtss(x, y)`.
  kOverwrite = 0x00000004u,

  //! Emit short-form of the instruction.
  kShortForm = 0x00000010u,
  //! Emit long-form of the instruction.
  kLongForm = 0x00000020u,

  //! Conditional jump is likely to be taken.
  kTaken = 0x00000040u,
  //! Conditional jump is unlikely to be taken.
  kNotTaken = 0x00000080u,

  // X86 & X64 Options
  // -----------------

  //! Use ModMR instead of ModRM if applicable.
  kX86_ModMR = 0x00000100u,
  //! Use ModRM instead of ModMR if applicable.
  kX86_ModRM = 0x00000200u,
  //! Use 3-byte VEX prefix if possible (AVX) (must be 0x00000400).
  kX86_Vex3 = 0x00000400u,
  //! Use VEX prefix when both VEX|EVEX prefixes are available (HINT: AVX_VNNI).
  kX86_Vex = 0x00000800u,
  //! Use 4-byte EVEX prefix if possible (AVX-512) (must be 0x00001000).
  kX86_Evex = 0x00001000u,

  //! LOCK prefix (lock-enabled instructions only).
  kX86_Lock = 0x00002000u,
  //! REP prefix (string instructions only).
  kX86_Rep = 0x00004000u,
  //! REPNE prefix (string instructions only).
  kX86_Repne = 0x00008000u,

  //! XACQUIRE prefix (only allowed instructions).
  kX86_XAcquire = 0x00010000u,
  //! XRELEASE prefix (only allowed instructions).
  kX86_XRelease = 0x00020000u,

  //! AVX-512: embedded-rounding {er} and implicit {sae}.
  kX86_ER = 0x00040000u,
  //! AVX-512: suppress-all-exceptions {sae}.
  kX86_SAE = 0x00080000u,
  //! AVX-512: round-to-nearest (even) {rn-sae} (bits 00).
  kX86_RN_SAE = 0x00000000u,
  //! AVX-512: round-down (toward -inf) {rd-sae} (bits 01).
  kX86_RD_SAE = 0x00200000u,
  //! AVX-512: round-up (toward +inf) {ru-sae} (bits 10).
  kX86_RU_SAE = 0x00400000u,
  //! AVX-512: round-toward-zero (truncate) {rz-sae} (bits 11).
  kX86_RZ_SAE = 0x00600000u,
  //! AVX-512: Use zeroing {k}{z} instead of merging {k}.
  kX86_ZMask = 0x00800000u,

  //! AVX-512: Mask to get embedded rounding bits (2 bits).
  kX86_ERMask = kX86_RZ_SAE,
  //! AVX-512: Mask of all possible AVX-512 options except EVEX prefix flag.
  kX86_AVX512Mask = 0x00FC0000u,

  //! Force REX.B and/or VEX.B field (X64 only).
  kX86_OpCodeB = 0x01000000u,
  //! Force REX.X and/or VEX.X field (X64 only).
  kX86_OpCodeX = 0x02000000u,
  //! Force REX.R and/or VEX.R field (X64 only).
  kX86_OpCodeR = 0x04000000u,
  //! Force REX.W and/or VEX.W field (X64 only).
  kX86_OpCodeW = 0x08000000u,
  //! Force REX prefix (X64 only).
  kX86_Rex = 0x40000000u,
  //! Invalid REX prefix (set by X86 or when AH|BH|CH|DH regs are used on X64).
  kX86_InvalidRex = 0x80000000u
};
ASMJIT_DEFINE_ENUM_FLAGS(InstOptions)

//! Instruction control flow.
enum class InstControlFlow : uint32_t {
  //! Regular instruction.
  kRegular = 0u,
  //! Unconditional jump.
  kJump = 1u,
  //! Conditional jump (branch).
  kBranch = 2u,
  //! Function call.
  kCall = 3u,
  //! Function return.
  kReturn = 4u,

  //! Maximum value of `InstType`.
  kMaxValue = kReturn
};

//! Hint that is used when both input operands to the instruction are the same.
//!
//! Provides hints to the instrution RW query regarding special cases in which two or more operands are the same
//! registers. This is required by instructions such as XOR, AND, OR, SUB, etc... These hints will influence the
//! RW operations query.
enum class InstSameRegHint : uint8_t {
  //! No special handling.
  kNone = 0,
  //! Operands become read-only, the operation doesn't change the content - `X & X` and similar.
  kRO = 1,
  //! Operands become write-only, the content of the input(s) don't matter - `X ^ X`, `X - X`, and similar.
  kWO = 2
};

//! Instruction id, options, and extraReg in a single structure. This structure exists mainly to simplify analysis
//! and validation API that requires `BaseInst` and `Operand[]` array.
class BaseInst {
public:
  //! \name Members
  //! \{

  //! Instruction id with modifiers.
  InstId _id;
  //! Instruction options.
  InstOptions _options;
  //! Extra register used by the instruction (either REP register or AVX-512 selector).
  RegOnly _extraReg;

  enum Id : uint32_t {
    //! Invalid or uninitialized instruction id.
    kIdNone = 0x00000000u,
    //! Abstract instruction (BaseBuilder and BaseCompiler).
    kIdAbstract = 0x80000000u
  };

  //! \}

  //! \name Construction & Destruction
  //! \{

  //! Creates a new BaseInst instance with `id` and `options` set.
  //!
  //! Default values of `id` and `options` are zero, which means 'none' instruction. Such instruction is guaranteed
  //! to never exist for any architecture supported by AsmJit.
  inline explicit BaseInst(InstId instId = 0, InstOptions options = InstOptions::kNone) noexcept
    : _id(instId),
      _options(options),
      _extraReg() {}

  inline BaseInst(InstId instId, InstOptions options, const RegOnly& extraReg) noexcept
    : _id(instId),
      _options(options),
      _extraReg(extraReg) {}

  inline BaseInst(InstId instId, InstOptions options, const BaseReg& extraReg) noexcept
    : _id(instId),
      _options(options),
      _extraReg { extraReg.signature(), extraReg.id() } {}

  //! \}

  //! \name Instruction id and modifiers
  //! \{

  //! Returns the instruction id with modifiers.
  inline InstId id() const noexcept { return _id; }
  //! Sets the instruction id and modiiers from `id`.
  inline void setId(InstId id) noexcept { _id = id; }
  //! Resets the instruction id and modifiers to zero, see \ref kIdNone.
  inline void resetId() noexcept { _id = 0; }

  //! Returns a real instruction id that doesn't contain any modifiers.
  inline InstId realId() const noexcept { return _id & uint32_t(InstIdParts::kRealId); }

  template<InstIdParts kPart>
  inline uint32_t getInstIdPart() const noexcept {
    return (uint32_t(_id) & uint32_t(kPart)) >> Support::ConstCTZ<uint32_t(kPart)>::value;
  }

  template<InstIdParts kPart>
  inline void setInstIdPart(uint32_t value) noexcept {
    _id = (_id & ~uint32_t(kPart)) | (value << Support::ConstCTZ<uint32_t(kPart)>::value);
  }

  //! \}

  //! \name Instruction Options
  //! \{

  inline InstOptions options() const noexcept { return _options; }
  inline bool hasOption(InstOptions option) const noexcept { return Support::test(_options, option); }
  inline void setOptions(InstOptions options) noexcept { _options = options; }
  inline void addOptions(InstOptions options) noexcept { _options |= options; }
  inline void clearOptions(InstOptions options) noexcept { _options &= ~options; }
  inline void resetOptions() noexcept { _options = InstOptions::kNone; }

  //! \}

  //! \name Extra Register
  //! \{

  inline bool hasExtraReg() const noexcept { return _extraReg.isReg(); }
  inline RegOnly& extraReg() noexcept { return _extraReg; }
  inline const RegOnly& extraReg() const noexcept { return _extraReg; }
  inline void setExtraReg(const BaseReg& reg) noexcept { _extraReg.init(reg); }
  inline void setExtraReg(const RegOnly& reg) noexcept { _extraReg.init(reg); }
  inline void resetExtraReg() noexcept { _extraReg.reset(); }

  //! \}

  //! \name ARM Specific
  //! \{

  inline arm::CondCode armCondCode() const noexcept { return (arm::CondCode)getInstIdPart<InstIdParts::kARM_Cond>(); }
  inline void setArmCondCode(arm::CondCode cc) noexcept { setInstIdPart<InstIdParts::kARM_Cond>(uint32_t(cc)); }

  //! \}

  //! \name Statics
  //! \{

  static inline constexpr InstId composeARMInstId(uint32_t id, arm::CondCode cc) noexcept {
    return id | (uint32_t(cc) << Support::ConstCTZ<uint32_t(InstIdParts::kARM_Cond)>::value);
  }

  static inline constexpr arm::CondCode extractARMCondCode(uint32_t id) noexcept {
    return (arm::CondCode)((uint32_t(id) & uint32_t(InstIdParts::kARM_Cond)) >> Support::ConstCTZ<uint32_t(InstIdParts::kARM_Cond)>::value);
  }

  //! \}
};

//! CPU read/write flags used by \ref InstRWInfo.
//!
//! These flags can be used to get a basic overview about CPU specifics flags used by instructions.
enum class CpuRWFlags : uint32_t {
  //! No flags.
  kNone = 0x00000000u,

  // Common RW Flags (0x000000FF)
  // ----------------------------

  //! Carry flag.
  kCF = 0x00000001u,
  //! Signed overflow flag.
  kOF = 0x00000002u,
  //! Sign flag (negative/sign, if set).
  kSF = 0x00000004u,
  //! Zero and/or equality flag (1 if zero/equal).
  kZF = 0x00000008u,

  // X86 Specific RW Flags (0xFFFFFF00)
  // ----------------------------------

  //! Carry flag (X86, X86_64).
  kX86_CF = kCF,
  //! Overflow flag (X86, X86_64).
  kX86_OF = kOF,
  //! Sign flag (X86, X86_64).
  kX86_SF = kSF,
  //! Zero flag (X86, X86_64).
  kX86_ZF = kZF,

  //! Adjust flag (X86, X86_64).
  kX86_AF = 0x00000100u,
  //! Parity flag (X86, X86_64).
  kX86_PF = 0x00000200u,
  //! Direction flag (X86, X86_64).
  kX86_DF = 0x00000400u,
  //! Interrupt enable flag (X86, X86_64).
  kX86_IF = 0x00000800u,

  //! Alignment check flag (X86, X86_64).
  kX86_AC = 0x00001000u,

  //! FPU C0 status flag (X86, X86_64).
  kX86_C0 = 0x00010000u,
  //! FPU C1 status flag (X86, X86_64).
  kX86_C1 = 0x00020000u,
  //! FPU C2 status flag (X86, X86_64).
  kX86_C2 = 0x00040000u,
  //! FPU C3 status flag (X86, X86_64).
  kX86_C3 = 0x00080000u
};
ASMJIT_DEFINE_ENUM_FLAGS(CpuRWFlags)

//! Operand read/write flags describe how the operand is accessed and some additional features.
enum class OpRWFlags {
  //! No flags.
  kNone = 0,

  //! Operand is read.
  kRead = 0x00000001u,

  //! Operand is written.
  kWrite = 0x00000002u,

  //! Operand is both read and written.
  kRW = 0x00000003u,

  //! Register operand can be replaced by a memory operand.
  kRegMem = 0x00000004u,

  //! The register must be allocated to the index of the previous register + 1.
  //!
  //! This flag is used by all architectures to describe instructions that use consecutive registers, where only the
  //! first one is encoded in the instruction, and the others are just a sequence that starts with the first one. On
  //! X86/X86_64 architecture this is used by instructions such as V4FMADDPS, V4FMADDSS, V4FNMADDPS, V4FNMADDSS,
  //! VP4DPWSSD, VP4DPWSSDS, VP2INTERSECTD, and VP2INTERSECTQ. On ARM/AArch64 this is used by vector load and store
  //! instructions that can load or store multiple registers at once.
  kConsecutive = 0x00000008u,

  //! The `extendByteMask()` represents a zero extension.
  kZExt = 0x00000010u,

  //! Register operand must use \ref OpRWInfo::physId().
  kRegPhysId = 0x00000100u,
  //! Base register of a memory operand must use \ref OpRWInfo::physId().
  kMemPhysId = 0x00000200u,

  //! This memory operand is only used to encode registers and doesn't access memory.
  //!
  //! X86 Specific
  //! ------------
  //!
  //! Instructions that use such feature include BNDLDX, BNDSTX, and LEA.
  kMemFake = 0x000000400u,

  //! Base register of the memory operand will be read.
  kMemBaseRead = 0x00001000u,
  //! Base register of the memory operand will be written.
  kMemBaseWrite = 0x00002000u,
  //! Base register of the memory operand will be read & written.
  kMemBaseRW = 0x00003000u,

  //! Index register of the memory operand will be read.
  kMemIndexRead = 0x00004000u,
  //! Index register of the memory operand will be written.
  kMemIndexWrite = 0x00008000u,
  //! Index register of the memory operand will be read & written.
  kMemIndexRW = 0x0000C000u,

  //! Base register of the memory operand will be modified before the operation.
  kMemBasePreModify = 0x00010000u,
  //! Base register of the memory operand will be modified after the operation.
  kMemBasePostModify = 0x00020000u
};
ASMJIT_DEFINE_ENUM_FLAGS(OpRWFlags)

// Don't remove these asserts. Read/Write flags are used extensively
// by Compiler and they must always be compatible with constants below.
static_assert(uint32_t(OpRWFlags::kRead) == 0x1, "OpRWFlags::kRead flag must be 0x1");
static_assert(uint32_t(OpRWFlags::kWrite) == 0x2, "OpRWFlags::kWrite flag must be 0x2");
static_assert(uint32_t(OpRWFlags::kRegMem) == 0x4, "OpRWFlags::kRegMem flag must be 0x4");

//! Read/Write information related to a single operand, used by \ref InstRWInfo.
struct OpRWInfo {
  //! \name Members
  //! \{

  //! Read/Write flags.
  OpRWFlags _opFlags;
  //! Physical register index, if required.
  uint8_t _physId;
  //! Size of a possible memory operand that can replace a register operand.
  uint8_t _rmSize;
  //! If non-zero, then this is a consecutive lead register, and the value describes how many registers follow.
  uint8_t _consecutiveLeadCount;
  //! Reserved for future use.
  uint8_t _reserved[1];
  //! Read bit-mask where each bit represents one byte read from Reg/Mem.
  uint64_t _readByteMask;
  //! Write bit-mask where each bit represents one byte written to Reg/Mem.
  uint64_t _writeByteMask;
  //! Zero/Sign extend bit-mask where each bit represents one byte written to Reg/Mem.
  uint64_t _extendByteMask;

  //! \}

  //! \name Reset
  //! \{

  //! Resets this operand information to all zeros.
  inline void reset() noexcept { memset(this, 0, sizeof(*this)); }

  //! Resets this operand info (resets all members) and set common information
  //! to the given `opFlags`, `regSize`, and possibly `physId`.
  inline void reset(OpRWFlags opFlags, uint32_t regSize, uint32_t physId = BaseReg::kIdBad) noexcept {
    _opFlags = opFlags;
    _physId = uint8_t(physId);
    _rmSize = Support::test(opFlags, OpRWFlags::kRegMem) ? uint8_t(regSize) : uint8_t(0);
    _consecutiveLeadCount = 0;
    _resetReserved();

    uint64_t mask = Support::lsbMask<uint64_t>(regSize);
    _readByteMask = Support::test(opFlags, OpRWFlags::kRead) ? mask : uint64_t(0);
    _writeByteMask = Support::test(opFlags, OpRWFlags::kWrite) ? mask : uint64_t(0);
    _extendByteMask = 0;
  }

  inline void _resetReserved() noexcept {
    _reserved[0] = 0;
  }

  //! \}

  //! \name Operand Flags
  //! \{

  //! Returns operand flags.
  inline OpRWFlags opFlags() const noexcept { return _opFlags; }
  //! Tests whether operand flags contain the given `flag`.
  inline bool hasOpFlag(OpRWFlags flag) const noexcept { return Support::test(_opFlags, flag); }

  //! Adds the given `flags` to operand flags.
  inline void addOpFlags(OpRWFlags flags) noexcept { _opFlags |= flags; }
  //! Removes the given `flags` from operand flags.
  inline void clearOpFlags(OpRWFlags flags) noexcept { _opFlags &= ~flags; }

  //! Tests whether this operand is read from.
  inline bool isRead() const noexcept { return hasOpFlag(OpRWFlags::kRead); }
  //! Tests whether this operand is written to.
  inline bool isWrite() const noexcept { return hasOpFlag(OpRWFlags::kWrite); }
  //! Tests whether this operand is both read and write.
  inline bool isReadWrite() const noexcept { return (_opFlags & OpRWFlags::kRW) == OpRWFlags::kRW; }
  //! Tests whether this operand is read only.
  inline bool isReadOnly() const noexcept { return (_opFlags & OpRWFlags::kRW) == OpRWFlags::kRead; }
  //! Tests whether this operand is write only.
  inline bool isWriteOnly() const noexcept { return (_opFlags & OpRWFlags::kRW) == OpRWFlags::kWrite; }

  //! Returns the type of a lead register, which is followed by consecutive registers.
  inline uint32_t consecutiveLeadCount() const noexcept { return _consecutiveLeadCount; }

  //! Tests whether this operand is Reg/Mem
  //!
  //! Reg/Mem operands can use either register or memory.
  inline bool isRm() const noexcept { return hasOpFlag(OpRWFlags::kRegMem); }

  //! Tests whether the operand will be zero extended.
  inline bool isZExt() const noexcept { return hasOpFlag(OpRWFlags::kZExt); }

  //! \}

  //! \name Memory Flags
  //! \{

  //! Tests whether this is a fake memory operand, which is only used, because of encoding. Fake memory operands do
  //! not access any memory, they are only used to encode registers.
  inline bool isMemFake() const noexcept { return hasOpFlag(OpRWFlags::kMemFake); }

  //! Tests whether the instruction's memory BASE register is used.
  inline bool isMemBaseUsed() const noexcept { return hasOpFlag(OpRWFlags::kMemBaseRW); }
  //! Tests whether the instruction reads from its BASE registers.
  inline bool isMemBaseRead() const noexcept { return hasOpFlag(OpRWFlags::kMemBaseRead); }
  //! Tests whether the instruction writes to its BASE registers.
  inline bool isMemBaseWrite() const noexcept { return hasOpFlag(OpRWFlags::kMemBaseWrite); }
  //! Tests whether the instruction reads and writes from/to its BASE registers.
  inline bool isMemBaseReadWrite() const noexcept { return (_opFlags & OpRWFlags::kMemBaseRW) == OpRWFlags::kMemBaseRW; }
  //! Tests whether the instruction only reads from its BASE registers.
  inline bool isMemBaseReadOnly() const noexcept { return (_opFlags & OpRWFlags::kMemBaseRW) == OpRWFlags::kMemBaseRead; }
  //! Tests whether the instruction only writes to its BASE registers.
  inline bool isMemBaseWriteOnly() const noexcept { return (_opFlags & OpRWFlags::kMemBaseRW) == OpRWFlags::kMemBaseWrite; }

  //! Tests whether the instruction modifies the BASE register before it uses it to calculate the target address.
  inline bool isMemBasePreModify() const noexcept { return hasOpFlag(OpRWFlags::kMemBasePreModify); }
  //! Tests whether the instruction modifies the BASE register after it uses it to calculate the target address.
  inline bool isMemBasePostModify() const noexcept { return hasOpFlag(OpRWFlags::kMemBasePostModify); }

  //! Tests whether the instruction's memory INDEX register is used.
  inline bool isMemIndexUsed() const noexcept { return hasOpFlag(OpRWFlags::kMemIndexRW); }
  //! Tests whether the instruction reads the INDEX registers.
  inline bool isMemIndexRead() const noexcept { return hasOpFlag(OpRWFlags::kMemIndexRead); }
  //! Tests whether the instruction writes to its INDEX registers.
  inline bool isMemIndexWrite() const noexcept { return hasOpFlag(OpRWFlags::kMemIndexWrite); }
  //! Tests whether the instruction reads and writes from/to its INDEX registers.
  inline bool isMemIndexReadWrite() const noexcept { return (_opFlags & OpRWFlags::kMemIndexRW) == OpRWFlags::kMemIndexRW; }
  //! Tests whether the instruction only reads from its INDEX registers.
  inline bool isMemIndexReadOnly() const noexcept { return (_opFlags & OpRWFlags::kMemIndexRW) == OpRWFlags::kMemIndexRead; }
  //! Tests whether the instruction only writes to its INDEX registers.
  inline bool isMemIndexWriteOnly() const noexcept { return (_opFlags & OpRWFlags::kMemIndexRW) == OpRWFlags::kMemIndexWrite; }

  //! \}

  //! \name Physical Register ID
  //! \{

  //! Returns a physical id of the register that is fixed for this operand.
  //!
  //! Returns \ref BaseReg::kIdBad if any register can be used.
  inline uint32_t physId() const noexcept { return _physId; }
  //! Tests whether \ref physId() would return a valid physical register id.
  inline bool hasPhysId() const noexcept { return _physId != BaseReg::kIdBad; }
  //! Sets physical register id, which would be fixed for this operand.
  inline void setPhysId(uint32_t physId) noexcept { _physId = uint8_t(physId); }

  //! \}

  //! \name Reg/Mem Information
  //! \{

  //! Returns Reg/Mem size of the operand.
  inline uint32_t rmSize() const noexcept { return _rmSize; }
  //! Sets Reg/Mem size of the operand.
  inline void setRmSize(uint32_t rmSize) noexcept { _rmSize = uint8_t(rmSize); }

  //! \}

  //! \name Read & Write Masks
  //! \{

  //! Returns read mask.
  inline uint64_t readByteMask() const noexcept { return _readByteMask; }
  //! Returns write mask.
  inline uint64_t writeByteMask() const noexcept { return _writeByteMask; }
  //! Returns extend mask.
  inline uint64_t extendByteMask() const noexcept { return _extendByteMask; }

  //! Sets read mask.
  inline void setReadByteMask(uint64_t mask) noexcept { _readByteMask = mask; }
  //! Sets write mask.
  inline void setWriteByteMask(uint64_t mask) noexcept { _writeByteMask = mask; }
  //! Sets externd mask.
  inline void setExtendByteMask(uint64_t mask) noexcept { _extendByteMask = mask; }

  //! \}
};

//! Read/Write information of an instruction.
struct InstRWInfo {
  //! \name Members
  //! \{

  //! Instruction flags (there are no flags at the moment, this field is reserved).
  uint32_t _instFlags;
  //! CPU flags read.
  CpuRWFlags _readFlags;
  //! CPU flags written.
  CpuRWFlags _writeFlags;
  //! Count of operands.
  uint8_t _opCount;
  //! CPU feature required for replacing register operand with memory operand.
  uint8_t _rmFeature;
  //! Reserved for future use.
  uint8_t _reserved[18];
  //! Read/Write onfo of extra register (rep{} or kz{}).
  OpRWInfo _extraReg;
  //! Read/Write info of instruction operands.
  OpRWInfo _operands[Globals::kMaxOpCount];

  //! \}

  //! \name Commons
  //! \{

  //! Resets this RW information to all zeros.
  inline void reset() noexcept { memset(this, 0, sizeof(*this)); }

  //! \}

  //! \name CPU Flags Information
  //! \{

  //! Returns a mask of CPU flags read.
  inline CpuRWFlags readFlags() const noexcept { return _readFlags; }
  //! Returns a mask of CPU flags written.
  inline CpuRWFlags writeFlags() const noexcept { return _writeFlags; }

  //! \}

  //! \name Reg/Mem Information
  //! \{

  //! Returns the CPU feature required to replace a register operand with memory operand. If the returned feature is
  //! zero (none) then this instruction either doesn't provide memory operand combination or there is no extra CPU
  //! feature required.
  //!
  //! X86 Specific
  //! ------------
  //!
  //! Some AVX+ instructions may require extra features for replacing registers with memory operands, for example
  //! VPSLLDQ instruction only supports `vpslldq reg, reg, imm` combination on AVX/AVX2 capable CPUs and requires
  //! AVX-512 for `vpslldq reg, mem, imm` combination.
  inline uint32_t rmFeature() const noexcept { return _rmFeature; }

  //! \}

  //! \name Operand Read/Write Information
  //! \{

  //! Returns RW information of extra register operand (extraReg).
  inline const OpRWInfo& extraReg() const noexcept { return _extraReg; }

  //! Returns RW information of all instruction's operands.
  inline const OpRWInfo* operands() const noexcept { return _operands; }

  //! Returns RW information of the operand at the given `index`.
  inline const OpRWInfo& operand(size_t index) const noexcept {
    ASMJIT_ASSERT(index < Globals::kMaxOpCount);
    return _operands[index];
  }

  //! Returns the number of operands this instruction has.
  inline uint32_t opCount() const noexcept { return _opCount; }

  //! \}
};

//! Validation flags that can be used with \ref InstAPI::validate().
enum class ValidationFlags : uint32_t {
  //! No flags.
  kNone = 0,
  //! Allow virtual registers in the instruction.
  kEnableVirtRegs = 0x01u
};
ASMJIT_DEFINE_ENUM_FLAGS(ValidationFlags)

//! Instruction API.
namespace InstAPI {

#ifndef ASMJIT_NO_TEXT
//! Appends the name of the instruction specified by `instId` and `instOptions` into the `output` string.
//!
//! \note Instruction options would only affect instruction prefix & suffix, other options would be ignored.
//! If `instOptions` is zero then only raw instruction name (without any additional text) will be appended.
ASMJIT_API Error instIdToString(Arch arch, InstId instId, String& output) noexcept;

//! Parses an instruction name in the given string `s`. Length is specified by `len` argument, which can be
//! `SIZE_MAX` if `s` is known to be null terminated.
//!
//! Returns the parsed instruction id or \ref BaseInst::kIdNone if no such instruction exists.
ASMJIT_API InstId stringToInstId(Arch arch, const char* s, size_t len) noexcept;
#endif // !ASMJIT_NO_TEXT

#ifndef ASMJIT_NO_VALIDATION
//! Validates the given instruction considering the given `validationFlags`.
ASMJIT_API Error validate(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, ValidationFlags validationFlags = ValidationFlags::kNone) noexcept;
#endif // !ASMJIT_NO_VALIDATION

#ifndef ASMJIT_NO_INTROSPECTION
//! Gets Read/Write information of the given instruction.
ASMJIT_API Error queryRWInfo(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, InstRWInfo* out) noexcept;

//! Gets CPU features required by the given instruction.
ASMJIT_API Error queryFeatures(Arch arch, const BaseInst& inst, const Operand_* operands, size_t opCount, CpuFeatures* out) noexcept;
#endif // !ASMJIT_NO_INTROSPECTION

} // {InstAPI}

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_INST_H_INCLUDED
