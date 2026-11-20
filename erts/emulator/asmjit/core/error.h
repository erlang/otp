// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ERROR_H_INCLUDED
#define ASMJIT_CORE_ERROR_H_INCLUDED

#include <asmjit/core/build_defs.h>

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_error_handling
//! \{

//! AsmJit error code.
enum class Error : uint32_t {
  // @EnumValuesBegin{"enum": "Error"}@

  //! No error (success).
  kOk = 0,

  //! Out of memory.
  kOutOfMemory,

  //! Invalid argument.
  kInvalidArgument,

  //! Invalid state.
  //!
  //! If this error is returned it means that either you are doing something wrong or AsmJit caught itself by
  //! doing something wrong. This error should never be ignored.
  kInvalidState,

  //! Invalid data.
  kInvalidData,

  //! Invalid or incompatible architecture.
  kInvalidArch,

  //! Invalid token (parsing).
  kInvalidToken,

  //! Truncated data (incomplete input or data).
  kTruncatedData,

  //! The object is not initialized.
  kNotInitialized,

  //! The object has been already initialized.
  kAlreadyInitialized,

  //! Either a built-in feature was disabled at compile time and it's not available or the feature is not
  //! available on the target platform.
  //!
  //! For example trying to allocate large pages on unsupported platform would return this error.
  kFeatureNotEnabled,

  //! Too many handles (Windows) or file descriptors (Unix/Posix).
  kTooManyHandles,
  //! Code generated is larger than allowed.
  kTooLarge,

  //! No code generated.
  //!
  //! Returned by runtime if the \ref CodeHolder contains no code.
  kNoCodeGenerated,

  //! Code cannot be consumed, because it has fixups, which weren't resolved.
  //!
  //! \remarks This error code describes a situation, in which the content of \ref CodeHolder is being consumed, but it
  //! contains unresolved fixups. This could be caused by a missing call to \ref CodeHolder::resolve_cross_section_fixups()
  //! or by having unbound labels in code.
  kUnresolvedFixups,

  //! Invalid directive.
  kInvalidDirective,
  //! Attempt to use uninitialized label.
  kInvalidLabel,
  //! Label index overflow - a single \ref BaseAssembler instance can hold almost 2^32 (4 billion) labels. If
  //! there is an attempt to create more labels then this error is returned.
  kTooManyLabels,
  //! Label is already bound.
  kLabelAlreadyBound,
  //! Label is already defined (named labels).
  kLabelAlreadyDefined,
  //! Label name is too long.
  kLabelNameTooLong,
  //! Label must always be local if it's anonymous (without a name).
  kInvalidLabelName,
  //! Parent id passed to \ref CodeHolder::new_named_label_id() was either invalid or parent is not supported by
  //! the requested `LabelType`.
  kInvalidParentLabel,

  //! Invalid section.
  kInvalidSection,
  //! Too many sections (section index overflow).
  kTooManySections,
  //! Invalid section name (most probably too long).
  kInvalidSectionName,

  //! Relocation index overflow (too many relocations).
  kTooManyRelocations,
  //! Invalid relocation entry.
  kInvalidRelocEntry,
  //! Reloc entry contains address that is out of range (unencodable).
  kRelocOffsetOutOfRange,

  //! Invalid assignment to a register, function argument, or function return value.
  kInvalidAssignment,
  //! Invalid instruction.
  kInvalidInstruction,
  //! Invalid register type.
  kInvalidRegType,
  //! Invalid register group.
  kInvalidRegGroup,
  //! Invalid physical register id.
  kInvalidPhysId,
  //! Invalid virtual register id.
  kInvalidVirtId,
  //! Invalid element index (ARM).
  kInvalidElementIndex,
  //! Invalid prefix combination (X86|X64).
  kInvalidPrefixCombination,
  //! Invalid LOCK prefix (X86|X64).
  kInvalidLockPrefix,
  //! Invalid REP prefix (X86|X64).
  kInvalidRepPrefix,
  //! Invalid REX prefix (X86|X64).
  kInvalidRexPrefix,
  //! Invalid {...} register (X86|X64).
  kInvalidExtraReg,
  //! Invalid {k} use (not supported by the instruction) (X86|X64).
  kInvalidKMaskUse,
  //! Invalid {k}{z} use (not supported by the instruction) (X86|X64).
  kInvalidKZeroUse,
  //! Invalid broadcast - Currently only related to invalid use of AVX-512 {1tox} (X86|X64).
  kInvalidBroadcast,
  //! Invalid 'embedded-rounding' {er} or 'suppress-all-exceptions' {sae} (AVX-512) (X86|X64).
  kInvalidEROrSAE,
  //! Invalid address used (not encodable).
  kInvalidAddress,
  //! Invalid index register used in memory address (not encodable).
  kInvalidAddressIndex,
  //! Invalid address scale (not encodable).
  kInvalidAddressScale,
  //! Invalid use of 64-bit address.
  kInvalidAddress64Bit,
  //! Invalid use of 64-bit address that require 32-bit zero-extension (X64).
  kInvalidAddress64BitZeroExtension,
  //! Invalid displacement (not encodable).
  kInvalidDisplacement,
  //! Invalid segment (X86|X86_64).
  kInvalidSegment,

  //! Invalid immediate (out of bounds on X86 and invalid pattern on ARM).
  kInvalidImmediate,

  //! Invalid pointer authentication data.
  kInvalidPtrAuth,

  //! Invalid operand size.
  kInvalidOperandSize,
  //! Ambiguous operand size (memory has zero size while it's required to determine the operation type.
  kAmbiguousOperandSize,
  //! Mismatching operand size (size of multiple operands doesn't match the operation size).
  kOperandSizeMismatch,

  //! Invalid option.
  kInvalidOption,
  //! Option already defined.
  kOptionAlreadyDefined,

  //! Invalid TypeId.
  kInvalidTypeId,
  //! Invalid use of a 8-bit GPB-HIGH register.
  kInvalidUseOfGpbHi,
  //! Invalid use of a 64-bit GPQ register in 32-bit mode.
  kInvalidUseOfGpq,
  //! Invalid use of an 80-bit float (\ref TypeId::kFloat80).
  kInvalidUseOfF80,
  //! Instruction requires the use of consecutive registers, but registers in operands weren't (AVX512, ASIMD load/store, etc...).
  kNotConsecutiveRegs,
  //! Failed to allocate consecutive registers - allocable registers either too restricted or a bug in RW info.
  kConsecutiveRegsAllocation,

  //! Illegal virtual register - reported by instruction validation.
  kIllegalVirtReg,
  //! AsmJit cannot create more virtual registers.
  kTooManyVirtRegs,

  //! AsmJit requires a physical register, but no one is available.
  kNoMorePhysRegs,
  //! A variable has been assigned more than once to a function argument (BaseCompiler).
  kOverlappedRegs,
  //! Invalid register to hold stack arguments offset.
  kOverlappingStackRegWithRegArg,

  //! Unbound label cannot be evaluated by expression.
  kExpressionLabelNotBound,
  //! Arithmetic overflow during expression evaluation.
  kExpressionOverflow,

  //! Failed to open anonymous memory handle or file descriptor.
  kFailedToOpenAnonymousMemory,

  //! Repetitive calls to \ref BaseBuilder::run_passes() or \ref BaseBuilder::finalize() after a previous failure.
  //!
  //! \note This is always a user error that AsmJit detects, because calling `run_passes()` or `finalize()` again
  //! after an unsuccessful attempt lead to an unrecoverable failure. So, AsmJit marks FuncNodes where passes are
  //! running and only unmarks that after the run was successful. On failure, the marker is not cleared and AsmJit
  //! knows to never attempt to run it again.
  kPassRerunAttempted,

  //! Protection failure can be returned from a virtual memory allocator or when trying to change memory access
  //! permissions.
  kProtectionFailure,

  // @EnumValuesEnd@

  //! Maximum value of a valid AsmJit `Error` code.
  kMaxValue = kProtectionFailure,

  //! \cond INTERNAL
  //! Error code used to inform the caller about an alternative success state.
  //!
  //! \remarks This value is only used internally in AsmJit.
  kBypass = 0xFFFFFFFFu
  //! \endcond
};

static inline constexpr Error kErrorOk = Error::kOk;

//! Returns the error `err` passed.
//!
//! Provided for debugging purposes. Putting a breakpoint inside `make_error` can help with tracing the origin of any
//! error reported or returned by AsmJit.
[[nodiscard]]
static constexpr Error make_error(Error err) noexcept { return err; }

//! Combines two errors with the condition that they must be compatible. So, if both are errors, they must be the same
//! error. This is used in places where two functions can return the same error (usually `Error::kOutOfMemory`).
[[nodiscard]]
static ASMJIT_INLINE_CONSTEXPR Error combine_compatible_errors(Error err1, Error err2) noexcept {
  return Error(uint32_t(err1) | uint32_t(err2));
}

//! Returns a printable version of `asmjit::Error` code.
[[nodiscard]]
ASMJIT_API const char* stringify_error(Error err) noexcept;

//! \def ASMJIT_PROPAGATE(...)
//!
//! Propagates a possible `Error` produced by `...` to the caller by returning the error immediately. Used by AsmJit
//! internally, but kept public for users that want to use the same technique to propagate errors to the caller.
//!
//! Example:
//!
//! ```
//! using namespace asmjit;
//!
//! Error function_that_can_fail(BaseCompiler* cc) {
//!   // Trying to bind invalid label would return Error; and
//!   // ASMJIT_PROPAGATE would return that error automatically.
//!   Label invalid_label;
//!   ASMJIT_PROPAGATE(cc->bind(invalid_label));
//!
//!   // ... some more code here ...
//!
//!   return Error::kOk;
//! }
//! ```
#define ASMJIT_PROPAGATE(...)                                          \
  do {                                                                 \
    ::asmjit::Error error_to_propagate = __VA_ARGS__;                  \
    if (ASMJIT_UNLIKELY(error_to_propagate != ::asmjit::Error::kOk)) { \
      return error_to_propagate;                                       \
    }                                                                  \
  } while (0)

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORe_ERROR_H_INCLUDED
