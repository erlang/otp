// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>

#include <asmjit/axl/commons.h>
#include <asmjit/axl/string_index.h>
#include <asmjit/core/error.h>

ASMJIT_BEGIN_NAMESPACE

// Error - Stringify
// =================

ASMJIT_FAVOR_SIZE const char* stringify_error(Error err) noexcept {
#ifndef ASMJIT_NO_TEXT
  // @EnumStringBegin{"enum": "Error", "output": "error_string", "strip": "k"}@
  static constexpr char error_string_data[] =
    "Ok\0"
    "OutOfMemory\0"
    "InvalidArgument\0"
    "InvalidState\0"
    "InvalidData\0"
    "InvalidArch\0"
    "InvalidToken\0"
    "TruncatedData\0"
    "NotInitialized\0"
    "AlreadyInitialized\0"
    "FeatureNotEnabled\0"
    "TooManyHandles\0"
    "TooLarge\0"
    "NoCodeGenerated\0"
    "UnresolvedFixups\0"
    "InvalidDirective\0"
    "InvalidLabel\0"
    "TooManyLabels\0"
    "LabelAlreadyBound\0"
    "LabelAlreadyDefined\0"
    "LabelNameTooLong\0"
    "InvalidLabelName\0"
    "InvalidParentLabel\0"
    "InvalidSection\0"
    "TooManySections\0"
    "InvalidSectionName\0"
    "TooManyRelocations\0"
    "InvalidRelocEntry\0"
    "RelocOffsetOutOfRange\0"
    "InvalidAssignment\0"
    "InvalidInstruction\0"
    "InvalidRegType\0"
    "InvalidRegGroup\0"
    "InvalidPhysId\0"
    "InvalidVirtId\0"
    "InvalidElementIndex\0"
    "InvalidPrefixCombination\0"
    "InvalidLockPrefix\0"
    "InvalidRepPrefix\0"
    "InvalidRexPrefix\0"
    "InvalidExtraReg\0"
    "InvalidKMaskUse\0"
    "InvalidKZeroUse\0"
    "InvalidBroadcast\0"
    "InvalidEROrSAE\0"
    "InvalidAddress\0"
    "InvalidAddressIndex\0"
    "InvalidAddressScale\0"
    "InvalidAddress64Bit\0"
    "InvalidAddress64BitZeroExtension\0"
    "InvalidDisplacement\0"
    "InvalidSegment\0"
    "InvalidImmediate\0"
    "InvalidPtrAuth\0"
    "InvalidOperandSize\0"
    "AmbiguousOperandSize\0"
    "OperandSizeMismatch\0"
    "InvalidOption\0"
    "OptionAlreadyDefined\0"
    "InvalidTypeId\0"
    "InvalidUseOfGpbHi\0"
    "InvalidUseOfGpq\0"
    "InvalidUseOfF80\0"
    "NotConsecutiveRegs\0"
    "ConsecutiveRegsAllocation\0"
    "IllegalVirtReg\0"
    "TooManyVirtRegs\0"
    "NoMorePhysRegs\0"
    "OverlappedRegs\0"
    "OverlappingStackRegWithRegArg\0"
    "ExpressionLabelNotBound\0"
    "ExpressionOverflow\0"
    "FailedToOpenAnonymousMemory\0"
    "PassRerunAttempted\0"
    "ProtectionFailure\0"
    "<Unknown>\0";
  // @EnumStringEnd@

  static constexpr auto error_string_index = axl::make_string_index<size_t(Error::kMaxValue) + 2u>(error_string_data);
  return error_string_data + error_string_index[axl::min<size_t>(size_t(err), error_string_index.size() - 1u)];
#else
  axl::maybe_unused(err);
  static const char no_message[] = "";
  return no_message;
#endif
}

ASMJIT_END_NAMESPACE
