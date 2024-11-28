// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#ifndef ASMJIT_CORE_ASSEMBLER_H_INCLUDED
#define ASMJIT_CORE_ASSEMBLER_H_INCLUDED

#include "../core/codeholder.h"
#include "../core/emitter.h"
#include "../core/operand.h"

ASMJIT_BEGIN_NAMESPACE

//! \addtogroup asmjit_assembler
//! \{

//! Base assembler.
//!
//! This is a base class that provides interface used by architecture specific
//! assembler implementations. Assembler doesn't hold any data, instead it's
//! attached to \ref CodeHolder, which provides all the data that Assembler
//! needs and which can be altered by it.
//!
//! Check out architecture specific assemblers for more details and examples:
//!
//!   - \ref x86::Assembler - X86/X64 assembler implementation.
//!   - \ref a64::Assembler - AArch64 assembler implementation.
class ASMJIT_VIRTAPI BaseAssembler : public BaseEmitter {
public:
  ASMJIT_NONCOPYABLE(BaseAssembler)
  typedef BaseEmitter Base;

  //! Current section where the assembling happens.
  Section* _section = nullptr;
  //! Start of the CodeBuffer of the current section.
  uint8_t* _bufferData = nullptr;
  //! End (first invalid byte) of the current section.
  uint8_t* _bufferEnd = nullptr;
  //! Pointer in the CodeBuffer of the current section.
  uint8_t* _bufferPtr = nullptr;

  //! \name Construction & Destruction
  //! \{

  //! Creates a new `BaseAssembler` instance.
  ASMJIT_API BaseAssembler() noexcept;
  //! Destroys the `BaseAssembler` instance.
  ASMJIT_API ~BaseAssembler() noexcept override;

  //! \}

  //! \name Code-Buffer Management
  //! \{

  //! Returns the capacity of the current CodeBuffer.
  ASMJIT_INLINE_NODEBUG size_t bufferCapacity() const noexcept { return (size_t)(_bufferEnd - _bufferData); }
  //! Returns the number of remaining bytes in the current CodeBuffer.
  ASMJIT_INLINE_NODEBUG size_t remainingSpace() const noexcept { return (size_t)(_bufferEnd - _bufferPtr); }

  //! Returns the current position in the CodeBuffer.
  ASMJIT_INLINE_NODEBUG size_t offset() const noexcept { return (size_t)(_bufferPtr - _bufferData); }

  //! Sets the current position in the CodeBuffer to `offset`.
  //!
  //! \note The `offset` cannot be greater than buffer size even if it's
  //! within the buffer's capacity.
  ASMJIT_API Error setOffset(size_t offset);

  //! Returns the start of the CodeBuffer in the current section.
  ASMJIT_INLINE_NODEBUG uint8_t* bufferData() const noexcept { return _bufferData; }
  //! Returns the end (first invalid byte) in the current section.
  ASMJIT_INLINE_NODEBUG uint8_t* bufferEnd() const noexcept { return _bufferEnd; }
  //! Returns the current pointer in the CodeBuffer in the current section.
  ASMJIT_INLINE_NODEBUG uint8_t* bufferPtr() const noexcept { return _bufferPtr; }

  //! \}

  //! \name Section Management
  //! \{

  //! Returns the current section.
  ASMJIT_INLINE_NODEBUG Section* currentSection() const noexcept { return _section; }

  ASMJIT_API Error section(Section* section) override;

  //! \}

  //! \name Label Management
  //! \{

  ASMJIT_API Label newLabel() override;
  ASMJIT_API Label newNamedLabel(const char* name, size_t nameSize = SIZE_MAX, LabelType type = LabelType::kGlobal, uint32_t parentId = Globals::kInvalidId) override;
  ASMJIT_API Error bind(const Label& label) override;

  //! \}

  //! \name Embed
  //! \{

  ASMJIT_API Error embed(const void* data, size_t dataSize) override;
  ASMJIT_API Error embedDataArray(TypeId typeId, const void* data, size_t itemCount, size_t repeatCount = 1) override;
  ASMJIT_API Error embedConstPool(const Label& label, const ConstPool& pool) override;

  ASMJIT_API Error embedLabel(const Label& label, size_t dataSize = 0) override;
  ASMJIT_API Error embedLabelDelta(const Label& label, const Label& base, size_t dataSize = 0) override;

  //! \}

  //! \name Comment
  //! \{

  ASMJIT_API Error comment(const char* data, size_t size = SIZE_MAX) override;

  //! \}

  //! \name Events
  //! \{

  ASMJIT_API Error onAttach(CodeHolder* code) noexcept override;
  ASMJIT_API Error onDetach(CodeHolder* code) noexcept override;

  //! \}
};

//! \}

ASMJIT_END_NAMESPACE

#endif // ASMJIT_CORE_ASSEMBLER_H_INCLUDED
