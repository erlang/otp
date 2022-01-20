// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_FORMATTER_H_INCLUDED
#define ASMJIT_CORE_FORMATTER_H_INCLUDED

#include "../core/globals.h"
#include "../core/inst.h"
#include "../core/string.h"
#include "../core/support.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_logging
//! \{

class BaseBuilder;
class BaseEmitter;
class BaseNode;
struct Operand_;

//! Format flags used by \ref Logger and \ref FormatOptions.
enum class FormatFlags : uint32_t {
  //! No formatting flags.
  kNone = 0u,

  //! Show also binary form of each logged instruction (Assembler).
  kMachineCode = 0x00000001u,
  //! Show a text explanation of some immediate values.
  kExplainImms = 0x00000002u,
  //! Use hexadecimal notation of immediate values.
  kHexImms = 0x00000004u,
  //! Use hexadecimal notation of addresses and offsets in addresses.
  kHexOffsets = 0x00000008u,
  //! Show casts between virtual register types (Compiler output).
  kRegCasts = 0x00000010u,
  //! Show positions associated with nodes (Compiler output).
  kPositions = 0x00000020u
};
ASMJIT_DEFINE_ENUM_FLAGS(FormatFlags)

//! Format indentation group, used by \ref FormatOptions.
enum class FormatIndentationGroup : uint32_t {
  //! Indentation used for instructions and directives.
  kCode = 0u,
  //! Indentation used for labels and function nodes.
  kLabel = 1u,
  //! Indentation used for comments (not inline comments).
  kComment = 2u,

  //! \cond INTERNAL
  //! Reserved for future use.
  kReserved = 3u,
  //! \endcond

  //! Maximum value of `FormatIndentationGroup`.
  kMaxValue = kReserved
};

//! Format padding group, used by \ref FormatOptions.
enum class FormatPaddingGroup : uint32_t {
  //! Describes padding of a regular line, which can represent instruction, data, or assembler directives.
  kRegularLine = 0,
  //! Describes padding of machine code dump that is visible next to the instruction, if enabled.
  kMachineCode = 1,

  //! Maximum value of `FormatPaddingGroup`.
  kMaxValue = kMachineCode
};

//! Formatting options used by \ref Logger and \ref Formatter.
class FormatOptions {
public:
  //! \name Members
  //! \{

  //! Format flags.
  FormatFlags _flags = FormatFlags::kNone;
  //! Indentations for each indentation group.
  Support::Array<uint8_t, uint32_t(FormatIndentationGroup::kMaxValue) + 1> _indentation {};
  //! Paddings for each padding group.
  Support::Array<uint16_t, uint32_t(FormatPaddingGroup::kMaxValue) + 1> _padding {};

  //! \}

  //! \name Reset
  //! \{

  //! Resets FormatOptions to its default initialized state.
  inline void reset() noexcept {
    _flags = FormatFlags::kNone;
    _indentation.fill(uint8_t(0));
    _padding.fill(uint16_t(0));
  }

  //! \}

  //! \name Accessors
  //! \{

  //! Returns format flags.
  inline FormatFlags flags() const noexcept { return _flags; }
  //! Tests whether the given `flag` is set in format flags.
  inline bool hasFlag(FormatFlags flag) const noexcept { return Support::test(_flags, flag); }

  //! Resets all format flags to `flags`.
  inline void setFlags(FormatFlags flags) noexcept { _flags = flags; }
  //! Adds `flags` to format flags.
  inline void addFlags(FormatFlags flags) noexcept { _flags |= flags; }
  //! Removes `flags` from format flags.
  inline void clearFlags(FormatFlags flags) noexcept { _flags &= ~flags; }

  //! Returns indentation for the given indentation `group`.
  inline uint8_t indentation(FormatIndentationGroup group) const noexcept { return _indentation[group]; }
  //! Sets indentation for the given indentation `group`.
  inline void setIndentation(FormatIndentationGroup group, uint32_t n) noexcept { _indentation[group] = uint8_t(n); }
  //! Resets indentation for the given indentation `group` to zero.
  inline void resetIndentation(FormatIndentationGroup group) noexcept { _indentation[group] = uint8_t(0); }

  //! Returns pading for the given padding `group`.
  inline size_t padding(FormatPaddingGroup group) const noexcept { return _padding[group]; }
  //! Sets pading for the given padding `group`.
  inline void setPadding(FormatPaddingGroup group, size_t n) noexcept { _padding[group] = uint16_t(n); }
  //! Resets pading for the given padding `group` to zero, which means that a default padding will be used
  //! based on the target architecture properties.
  inline void resetPadding(FormatPaddingGroup group) noexcept { _padding[group] = uint16_t(0); }

  //! \}
};

//! Provides formatting functionality to format operands, instructions, and nodes.
namespace Formatter {

#ifndef ASMJIT_NO_LOGGING

//! Appends a formatted `typeId` to the output string `sb`.
ASMJIT_API Error formatTypeId(
  String& sb,
  TypeId typeId) noexcept;

//! Appends a formatted `featureId` to the output string `sb`.
//!
//! See \ref CpuFeatures.
ASMJIT_API Error formatFeature(
  String& sb,
  Arch arch,
  uint32_t featureId) noexcept;

//! Appends a formatted register to the output string `sb`.
//!
//! \note Emitter is optional, but it's required to format virtual registers, which won't be formatted properly
//! if the `emitter` is not provided.
ASMJIT_API Error formatRegister(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  RegType regType,
  uint32_t regId) noexcept;

//! Appends a formatted label to the output string `sb`.
//!
//! \note Emitter is optional, but it's required to format named labels properly, otherwise the formatted as
//! it is an anonymous label.
ASMJIT_API Error formatLabel(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  uint32_t labelId) noexcept;

//! Appends a formatted operand to the output string `sb`.
//!
//! \note Emitter is optional, but it's required to format named labels and virtual registers. See
//! \ref formatRegister() and \ref formatLabel() for more details.
ASMJIT_API Error formatOperand(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  const Operand_& op) noexcept;

//! Appends a formatted data-type to the output string `sb`.
ASMJIT_API Error formatDataType(
  String& sb,
  FormatFlags formatFlags,
  Arch arch,
  TypeId typeId) noexcept;

//! Appends a formatted data to the output string `sb`.
ASMJIT_API Error formatData(
  String& sb,
  FormatFlags formatFlags,
  Arch arch,
  TypeId typeId, const void* data, size_t itemCount, size_t repeatCount = 1) noexcept;

//! Appends a formatted instruction to the output string `sb`.
//!
//! \note Emitter is optional, but it's required to format named labels and virtual registers. See
//! \ref formatRegister() and \ref formatLabel() for more details.
ASMJIT_API Error formatInstruction(
  String& sb,
  FormatFlags formatFlags,
  const BaseEmitter* emitter,
  Arch arch,
  const BaseInst& inst, const Operand_* operands, size_t opCount) noexcept;

#ifndef ASMJIT_NO_BUILDER
//! Appends a formatted node to the output string `sb`.
//!
//! The `node` must belong to the provided `builder`.
ASMJIT_API Error formatNode(
  String& sb,
  const FormatOptions& formatOptions,
  const BaseBuilder* builder,
  const BaseNode* node) noexcept;

//! Appends formatted nodes to the output string `sb`.
//!
//! All nodes that are part of the given `builder` will be appended.
ASMJIT_API Error formatNodeList(
  String& sb,
  const FormatOptions& formatOptions,
  const BaseBuilder* builder) noexcept;

//! Appends formatted nodes to the output string `sb`.
//!
//! This function works the same as \ref formatNode(), but appends more nodes to the output string,
//! separating each node with a newline '\n' character.
ASMJIT_API Error formatNodeList(
  String& sb,
  const FormatOptions& formatOptions,
  const BaseBuilder* builder,
  const BaseNode* begin,
  const BaseNode* end) noexcept;
#endif

#endif

} // {Formatter}

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_FORMATTER_H_INCLUDED
